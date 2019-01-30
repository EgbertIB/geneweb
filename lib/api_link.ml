#ifdef API

module MLink = Api_link_tree_piqi
module MLinkext = Api_link_tree_piqi_ext
module RC = Redis_sync.Client

open Config
open Def
open Gwdb
open Api_util

let redis_host = ref "127.0.0.1"
let redis_port = ref 6379
let api_servers = ref []

(**/**) (* Redis. *)

let create_redis_connection () =
  let connection_spec = {RC.host = !redis_host; RC.port = !redis_port} in
  RC.IO.run (RC.connect connection_spec)

let redis_p_key base ip =
  let p = poi base ip in
  let sn = Name.lower (sou base (get_surname p)) in
  let fn = Name.lower (sou base (get_first_name p)) in
  let occ = get_occ p in
  sn ^ "|" ^ fn ^ "|" ^ if occ > 0 then string_of_int occ else ""

let filter_bulk l =
  List.fold_right
    (fun reply accu ->
      match reply with
      | `Bulk s ->
          begin
            match s with
            | Some s -> s :: accu
            | None -> accu
          end
      | _ -> accu)
    l []

let filter_string l =
  List.fold_right
    (fun s accu ->
      match s with
      | Some s -> s :: accu
      | None -> accu)
    l []

(* Trouve une key en fonction de l'utilisateur et d'une référence GW *)
let findKeyBySourcenameAndRef redis bname geneweb_key =
  RC.IO.run (RC.zscore redis ("lia.keys." ^ bname) geneweb_key)

let findBridgesBySourcenameAndIdGlinks redis bname fb =
  let l = RC.zrangebyscore redis ("lia.bridges." ^ bname) fb fb in
  filter_bulk (RC.IO.run l)

let findLinksBySourcenameAndBridge redis bname s =
  RC.IO.run (RC.hget redis ("lia.links." ^ bname) s)

let findKeyBySourcenameAndIdGlinks redis bname fb =
  let l = RC.zrangebyscore redis ("lia.keys." ^ bname) fb fb in
  filter_bulk (RC.IO.run l)

(* connection -> string -> string -> string option *)
(* Cherche les données d'un bridge en fonction du sourcename et de l'ID du bridge. *)
let findBridgeDataBySourcenameAndBridgeId redis bname bridge_id =
  RC.IO.run (RC.hget redis ("lia.bridges_data." ^ bname) bridge_id)

let json_list_of_string s =
  Yojson.Basic.Util.filter_string
    (Yojson.Basic.Util.to_list
       (Yojson.Basic.from_string s))

(* conf -> base -> connection -> ip -> bool *)
(* Permet de récupérer les ponts d'une personne (en utilisant son index). *)
(* include_not_validated : booléen indiquant l'inclusion ou non des ponts non validés. *)
let get_bridges conf base redis ip include_not_validated =
  (* on récupère la clé *)
  match
    findKeyBySourcenameAndRef redis conf.bname (redis_p_key base ip)
  with
  | Some f ->
      (* Récupère tous les IDs de ponts. *)
      let bridge_ids =
        findBridgesBySourcenameAndIdGlinks redis conf.bname (RC.FloatBound.Inclusive f)
      in
      (* Filtre sur les ponts validés. *)
      List.fold_left
        (fun accu bridge_id ->
          (* Récupère les données du pont. *)
          match findBridgeDataBySourcenameAndBridgeId redis conf.bname bridge_id
          with
           | Some bridge_data ->
              (* Les ponts ne sont pas filtrés si les ponts non validés sont inclus. *)
              if include_not_validated then
                bridge_id::accu
              else
                (* Parsing du JSON. *)
                let is_validated_bridge = List.hd (Yojson.Basic.Util.filter_string
                  (Yojson.Basic.Util.filter_member "validated" [Yojson.Basic.from_string bridge_data])) in
                (* Ajoute l'ID du pont à la liste seulement s'il est validé. *)
                if ((int_of_string is_validated_bridge) == 1) then
                  bridge_id::accu
                else
                  accu
           | None -> accu)
        [] bridge_ids
  | None -> []

(* conf -> base -> connection -> ip -> bool *)
(* Permet de récupérer les liens d'une personne (en utilisant son index). *)
(* include_not_validated : booléen indiquant l'inclusion ou non des liens non validés. *)
let get_links conf base redis ip include_not_validated =
  (* L'inclusion ou non des liens non validés se fait lors de la récupération des ponts. *)
  (* On pourrait aussi le faire au niveau des liens puisqu'ils contiennent également l'information validated. *)
  match get_bridges conf base redis ip include_not_validated with
  | [] -> []
  | l ->
      (* on récupère les liens associées *)
    List.map (findLinksBySourcenameAndBridge redis conf.bname) l
    |> filter_string

(**/**) (* Convertion d'une date, personne, famille. *)

(* ************************************************************************ *)
(*  [Fonc] piqi_date_of_date : def.date -> piqi_date                        *)
(** [Description] : Converti une date en date piqi
    [Args] :
      - date : la date a convertir
    [Retour] :
      - piqi date : date du module MLink.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let piqi_date_of_date date =
  match date with
  | Dgreg (dmy, cal) ->
      let (cal, dmy) =
        match cal with
        | Dgregorian -> (None, dmy)
        | Djulian -> (Some `julian, Calendar.julian_of_gregorian dmy)
        | Dfrench -> (Some `french, Calendar.french_of_gregorian dmy)
        | Dhebrew -> (Some `hebrew, Calendar.hebrew_of_gregorian dmy)
      in
      let (prec, dmy, dmy2) =
        let d = Some (Int32.of_int dmy.day) in
        let m = Some (Int32.of_int dmy.month) in
        let y = Some (Int32.of_int dmy.year) in
        let delta = Some (Int32.of_int dmy.delta) in
        let dmy1 = {MLink.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {MLink.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`oryear, Some dmy2)
          | YearInt dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {MLink.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        MLink.Date.cal = cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Dtext txt ->
      {
        MLink.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }

let p_to_piqi_full_person conf base ip ip_spouse =
  let p = Util.pget conf base ip in
  let lastname = to_piqi_surname base p in
  let firstname = to_piqi_firstname base p in
  let death_type, death_date = to_piqi_death_type_n_date_aux (Opt.map piqi_date_of_date) p in
  let titles = [] in               (* FIXME *)
  { MLink.Person.baseprefix = to_piqi_baseprefix conf.command p
  ; ip = to_piqi_index p
  ; n = to_piqi_sn lastname
  ; p = to_piqi_fn firstname
  ; oc = to_piqi_occ p
  ; lastname
  ; firstname
  ; image = to_piqi_image_opt base p
  ; occupation = to_piqi_occupation_opt base p
  ; public_name = to_piqi_publicname base p
  ; qualifiers = to_piqi_qualifiers base p
  ; titles
  ; aliases = to_piqi_aliases base p
  ; sex = to_piqi_sex p
  ; birth_date = to_piqi_date_aux piqi_date_of_date (get_birth p)
  ; birth_place = to_piqi_birth_place_opt base p
  ; baptism_date = to_piqi_date_aux piqi_date_of_date (get_baptism p)
  ; baptism_place = to_piqi_baptism_place_opt base p
  ; death_type = death_type
  ; death_date
  ; death_place = to_piqi_death_place_opt base p
  ; burial_date =
      to_piqi_date_aux piqi_date_of_date
        (match get_burial p with Buried d | Cremated d -> d | _ -> Adef.cdate_None)
  ; burial_place = to_piqi_burial_place_opt base p
  ; families =
      Array.fold_right
        (fun ifam acc ->
           let isp = Gutil.spouse ip (Util.fget conf base ifam) in
           if isp = ip_spouse || ip_spouse = Adef.iper_of_int (-1) then
             let baseprefix = conf.command in
             { MLink.Family_link.baseprefix
             ; ifam = to_piqi_ifam ifam
             } :: acc
           else acc)
        (get_family p) []
  }

let fam_to_piqi_full_family conf base ip ifam add_children =
  let baseprefix = conf.command in
  let index = Int32.of_int (Adef.int_of_ifam ifam) in
  let fam = Util.fget conf base ifam in
  let ifath = get_father fam in
  let imoth = get_mother fam in
  let ifath = Int32.of_int (Adef.int_of_iper ifath) in
  let imoth = Int32.of_int (Adef.int_of_iper imoth) in
  let gen_f = Util.string_gen_family base (gen_family_of_family fam) in
  let marriage = Opt.map piqi_date_of_date (Adef.od_of_cdate gen_f.marriage) in
  let marriage_place = Opt.of_string gen_f.marriage_place in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
      (`divorced, Opt.map piqi_date_of_date (Adef.od_of_cdate cod))
    | Separated -> (`separated, None)
  in
  let children =
    if add_children then
      Array.to_list @@
      Array.map
        (fun ip ->
          let ip = Int32.of_int (Adef.int_of_iper ip) in
          MLink.Person_link.({
            baseprefix = baseprefix;
            ip = ip;
          }))
        (get_children fam)
    else
      let pl =
        let ip = Int32.of_int (Adef.int_of_iper ip) in
        MLink.Person_link.({
          baseprefix = baseprefix;
          ip = ip;
        })
      in
      [pl]
  in
  {
    MLink.Family.baseprefix = baseprefix;
    ifam = index;
    ifath = ifath;
    imoth = imoth;
    marriage_type = marriage_type;
    marriage_date = marriage;
    marriage_place = marriage_place;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    children = children;
  }


(**/**)

let get_families_asc conf base ip nb_asc =
  let rec loop_asc parents families =
    match parents with
    | [] -> families
    | (ip, gen) :: parents ->
    if gen = nb_asc then loop_asc parents families
    else
      let p = Util.pget conf base ip in
      match get_parents p with
      | Some ifam ->
        let cpl = foi base ifam in
        let ifath = get_father cpl in
        let imoth = get_mother cpl in
        loop_asc ((ifath, gen + 1) :: (imoth, gen + 1) :: parents)
          ((ip, ifam, gen) :: families)
      | None -> loop_asc parents families
  in
  loop_asc [(ip, 0)] []

let get_families_desc conf base ip ip_spouse from_gen_desc nb_desc =
  if from_gen_desc <= 0 then []
  else
    let rec loop_asc pl accu =
      match pl with
      | [] -> accu
      | (ip, gen) :: pl ->
          if gen = from_gen_desc then loop_asc pl accu
          else
            let p = Util.pget conf base ip in
            match get_parents p with
            | Some ifam ->
              let cpl = foi base ifam in
              let ifath = get_father cpl in
              let imoth = get_mother cpl in
              loop_asc ((ifath, gen + 1) :: (imoth, gen + 1) :: pl)
                    ((ip, gen) :: accu)
            | None -> loop_asc pl accu
    in
    (* Récupère les ascendants jusqu'au nombre de générations from_gen_desc. *)
    (* Utile pour le template affichant les parents des conjoints. *)
    let ipl = loop_asc [(ip, 0)] [] in
    let ipl =
      match ipl with
      | [] -> [(ip, 0)]
      | _ -> ipl
    in
    let rec loop_desc pl accu =
      match pl with
      | [] -> accu (* Retourne accu lorsqu'il n'y a plus rien à parcourir. *)
      | (ip, gen) :: pl ->
        let p = Util.pget conf base ip in
        let fam = Array.to_list (get_family p) in
        let fam =
          if gen = 0 && ip_spouse <> Adef.iper_of_int (-1) then
            List.filter
              (fun ifam ->
                 let fam = foi base ifam in
                 let isp = Gutil.spouse ip fam in
                 isp = ip_spouse)
              fam
          else fam
          in
          let accu =
            (* Si la génération est inférieure à celle demandée, les données ne sont pas retournées. *)
            if gen <= -nb_desc then accu
            else
              List.fold_left
                (fun accu ifam -> (ip, ifam, gen) :: accu)
                accu fam
          in
          let pl =
            List.fold_left
              (fun pl ifam ->
                let fam = foi base ifam in
                Array.fold_left
                  (* Ne récupère pas les descendants si la génération suivante est inférieure à celle demandée. *)
                  (fun pl ic -> if gen - 1 <= -nb_desc then pl else (ic, gen - 1) :: pl)
                  pl (get_children fam))
              pl fam
          in
          loop_desc pl accu
    in
    loop_desc ipl []

(**/**)

let get_link_tree_curl conf request basename bname ip s s2 nb_asc from_gen_desc nb_desc =
  let host =
    let rec loop api_servers =
      match api_servers with
      | [] -> ("")
      | (reg, host) :: l ->
          let regexp = Str.regexp reg in
          if Str.string_match regexp bname 0 then host
          else loop l
    in
    loop !api_servers
  in
  let index = Some (Int32.of_int (Adef.int_of_iper ip)) in
  let data =
    MLink.Link_tree_params.({
      basename = basename;
      ip = index;
      ref_person = Some s;
      ref_person2 = Some s2;
      nb_asc = Int32.of_int nb_asc;
      from_gen_desc = Int32.of_int from_gen_desc;
      nb_desc = Int32.of_int nb_desc;
    })
  in
  let data = MLinkext.gen_link_tree_params data `pb in
  let url =
    Printf.sprintf
      "http://%s/%s?m=API_LINK_TREE&input=pb&output=pb&sig=azerty&data=%s"
      host bname (Wserver.encode data)
  in
  let res = ref "" in
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  begin
    let result = Buffer.create 16384
    and errorBuffer = ref "" in
    try
      let connection = Curl.init () in
      let headers = [] in
      let headers =
        let auth = Wserver.extract_param "authorization: " '\r' request in
        if auth <> "" then
          ("Authorization: " ^ auth) :: ("Gw-Connection-Type: auto") ::headers
        else headers
      in
      let headers =
        let include_not_validated =
          Wserver.extract_param "inter-tree-links-include-not-validated: " '\r' request
        in
        if include_not_validated <> "" then
          ("Inter-Tree-Links-Include-Not-Validated: " ^ include_not_validated) :: headers
        else headers
      in
      Curl.set_httpheader connection headers;
      Curl.set_errorbuffer connection errorBuffer;
      Curl.set_writefunction connection
        (fun data ->
           Buffer.add_string result data;
           String.length data);
      Curl.set_followlocation connection true;
      Curl.set_url connection url;
      Curl.set_timeoutms connection 1000;
      Curl.perform connection;
      Curl.cleanup connection;
      res := Buffer.contents result
    with
    | Curl.CurlException _ ->
        Printf.fprintf stderr "Error: %s\n" !errorBuffer
    | Failure s ->
        Printf.fprintf stderr "Caught exception: %s\n" s
  end;
  Curl.global_cleanup ();
  let output_encoding =
    match Api_util.p_getenvbin conf.env "output" with
     | Some "pb" -> `pb
     | Some "json" -> `json
     | Some "xml" -> `xml
     | _ -> exit (-2)
  in
  MLinkext.parse_link_tree !res output_encoding


let print_link_tree conf base =
  let params = Api_util.get_params conf MLinkext.parse_link_tree_params in
  let basename = params.MLink.Link_tree_params.basename in
  let ip = params.MLink.Link_tree_params.ip in
  let ref_person = params.MLink.Link_tree_params.ref_person in
  let ref_person2 = params.MLink.Link_tree_params.ref_person2 in
  let nb_asc = Int32.to_int params.MLink.Link_tree_params.nb_asc in
  let from_gen_desc = Int32.to_int params.MLink.Link_tree_params.from_gen_desc in
  let nb_desc = Int32.to_int params.MLink.Link_tree_params.nb_desc in
  (* Gestion de l'inclusion des not validated. *)
  let include_not_validated =
    let h_include_not_validated = Wserver.extract_param "inter-tree-links-include-not-validated: " '\r' conf.request in
    if h_include_not_validated = "1" then true else false
  in
  let redis = create_redis_connection () in
  let ip_local =
    match ref_person with
    | Some s ->
      Opt.default (Adef.iper_of_int (-1)) (Link.ip_of_ref_person base s)
    | None ->
      Opt.map_default (Adef.iper_of_int (-1)) (fun x -> Adef.iper_of_int (Int32.to_int x)) ip
  in
  let ip_distant =
    Opt.map_default (Adef.iper_of_int (-1)) (fun x -> Adef.iper_of_int (Int32.to_int x)) ip
  in
  let ip_local_spouse =
    match ref_person2 with
    | Some s when s <> "" ->
      Opt.default (Adef.iper_of_int (-1)) (Link.ip_of_ref_person base s)
    | _ -> Adef.iper_of_int (-1)
  in
  (* La liste de toutes les personnes à renvoyer. *)
  let pl =
    get_families_desc conf base ip_local ip_local_spouse from_gen_desc nb_desc
  in
  (* On dédoublonne la liste. *)
  let pl =
    let ht = Hashtbl.create (List.length pl) in
    List.filter
      (fun (ip, ifam, gen) ->
         not (Hashtbl.mem ht (ip, ifam, gen)) ||
         (Hashtbl.add ht (ip, ifam, gen) (); true))
      pl
  in
  (* Familles ascendantes locales. *)
  let local_asc_fam =
    if conf.bname <> basename &&
       ip_local <> Adef.iper_of_int (-1) &&
       ip_distant <> Adef.iper_of_int (-1)
    then
      let families = get_families_asc conf base ip_local nb_asc in
      List.map
        (fun (ip, ifam, gen) ->
          let add_children = gen < from_gen_desc in
          fam_to_piqi_full_family conf base ip ifam add_children)
        families
    else []
  in
  (* Familles descendantes locales. *)
  let local_desc_fam =
    if conf.bname <> basename &&
       ip_local <> Adef.iper_of_int (-1) &&
       ip_distant <> Adef.iper_of_int (-1)
    then
      List.map
        (fun (ip, ifam, _) -> fam_to_piqi_full_family conf base ip ifam true)
        pl
    else []
  in
  (* Familles locales. *)
  let local_families = local_asc_fam @ local_desc_fam in
  (* Personnes locales issues des familles asc et desc. *)
  let local_persons =
    let ht = Hashtbl.create 101 in
    List.fold_left
      (fun accu fam ->
         let ifath = Adef.iper_of_int (Int32.to_int fam.MLink.Family.ifath) in
         let imoth = Adef.iper_of_int (Int32.to_int fam.MLink.Family.imoth) in
         let accu =
           if Hashtbl.mem ht ifath then accu
           else
             begin
               Hashtbl.add ht ifath ();
               let ip_spouse =
                 if ip_local = ifath then ip_local_spouse
                 else Adef.iper_of_int (-1)
               in
               p_to_piqi_full_person conf base ifath ip_spouse :: accu
             end
         in
         let accu =
           if Hashtbl.mem ht imoth then accu
           else
             begin
               Hashtbl.add ht imoth ();
               let ip_spouse =
                 if ip_local = imoth then ip_local_spouse
                 else Adef.iper_of_int (-1)
               in
               p_to_piqi_full_person conf base imoth ip_spouse :: accu
             end
         in
         List.fold_left
           (fun accu c ->
              let ic = Adef.iper_of_int (Int32.to_int c.MLink.Person_link.ip) in
              if Hashtbl.mem ht ic then accu
              else
                begin
                  Hashtbl.add ht ic ();
                  let ip_spouse =
                    if ip_local = ic then ip_local_spouse
                    else Adef.iper_of_int (-1)
                  in
                  p_to_piqi_full_person conf base ic ip_spouse :: accu
                end)
           accu fam.MLink.Family.children)
      [] local_families
  in
  (* Correspondances locales. *)
  (* On constitue la liste de toutes les personnes, *)
  (* puis on ira chercher les correspondances.      *)
  let all_persons =
    let ht = Hashtbl.create 101 in
    List.fold_left
      (fun accu (_, ifam, _) ->
         let fam = foi base ifam in
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let accu =
           if Hashtbl.mem ht ifath then accu
           else
             begin
               Hashtbl.add ht ifath ();
               p_to_piqi_full_person conf base ifath (Adef.iper_of_int (-1)) :: accu
             end
         in
         let accu =
           if Hashtbl.mem ht imoth then accu
           else
             begin
               Hashtbl.add ht imoth ();
               p_to_piqi_full_person conf base imoth (Adef.iper_of_int (-1)) :: accu
             end
         in
         List.fold_left
           (fun accu ic ->
              if Hashtbl.mem ht ic then accu
              else
                begin
                  Hashtbl.add ht ic ();
                  p_to_piqi_full_person conf base ic (Adef.iper_of_int (-1)) :: accu
                end)
           accu (Array.to_list (get_children fam)))
      [] pl
  in
  let local_connections =
    List.fold_left
      (fun accu p ->
        let ip = Adef.iper_of_int (Int32.to_int p.MLink.Person.ip) in
        let bl = get_bridges conf base redis ip include_not_validated in
        List.fold_left
          (fun accu s ->
             match String.split_on_char ':' s with
             | [_; bname_link; id_link] ->
                 begin
                   match
                     findKeyBySourcenameAndIdGlinks
                       redis bname_link
                       (RC.FloatBound.Inclusive (float_of_string id_link))
                   with
                   | [s] ->
                       let from_ref = redis_p_key base ip in
                       let corresp =
                         MLink.Connection.({
                           from_baseprefix = conf.bname;
                           from_ref = from_ref;
                           to_baseprefix = bname_link;
                           to_ref = s;
                         })
                       in
                       corresp :: accu
                   | _ -> accu
                 end
             | _ -> accu)
          accu bl)
      [] all_persons
  in
  (* Descendance distante. *)
  let distant_desc_fam =
    let pl =
      match pl with
      | [] -> [(ip_local, Adef.ifam_of_int (-1), 0)]
      | _ -> pl
    in
    let ht_request = Hashtbl.create 101 in
    List.fold_left
      (fun (accu_fam, accu_pers, accu_conn) (ip, _, gen) ->
        if Hashtbl.mem ht_request ip then (accu_fam, accu_pers, accu_conn)
        else
          begin
            Hashtbl.add ht_request ip ();
            let links = get_links conf base redis ip include_not_validated in
            List.fold_left
              (fun (accu_fam, accu_pers, accu_conn) s ->
                List.fold_left
                  (fun (accu_fam, accu_pers, accu_conn) x ->
                    match String.split_on_char ':' x with
                    | [_; bname_link; id_link; "spouse-children"; id_link_spouse] ->
                        let pl =
                          findKeyBySourcenameAndIdGlinks redis bname_link
                            (RC.FloatBound.Inclusive (float_of_string id_link))
                        in
                        let pl2 =
                          findKeyBySourcenameAndIdGlinks redis bname_link
                            (RC.FloatBound.Inclusive (float_of_string id_link_spouse))
                        in
                        begin
                          match (pl, pl2) with
                          | ([s], [s2]) ->
                              let fam =
                                get_link_tree_curl conf conf.request conf.bname
                                  bname_link ip s s2 0 1 (nb_desc + gen)
                              in
                              let corr =
                                let from_ref = redis_p_key base ip in
                                MLink.Connection.({
                                  from_baseprefix = conf.bname;
                                  from_ref = from_ref;
                                  to_baseprefix = bname_link;
                                  to_ref = s;
                                })
                              in
                              let (accu_fam, accu_pers, accu_conn) =
                                (fam.MLink.Link_tree.families @ accu_fam,
                                 fam.MLink.Link_tree.persons @ accu_pers,
                                 corr :: fam.MLink.Link_tree.connections @ accu_conn)
                              in
                              (accu_fam, accu_pers, accu_conn)
                          | _ -> (accu_fam, accu_pers, accu_conn)
                        end
                    | _ -> (accu_fam, accu_pers, accu_conn))
                  (accu_fam, accu_pers, accu_conn) (json_list_of_string s))
              (accu_fam, accu_pers, accu_conn) links
           end)
      ([], [], []) pl
  in
  (* Ascendance distante. *)
  let distant_asc_fam =
    if nb_asc > 0 && ip_local <> Adef.iper_of_int (-1) then
      let rec loop parents persons =
        match parents with
        | [] -> persons
        | (ip, gen) :: parents ->
            if gen = nb_asc then loop parents persons
            else
            match get_parents (Util.pget conf base ip) with
            | Some ifam ->
                let cpl = foi base ifam in
                let ifath = get_father cpl in
                let imoth = get_mother cpl in
                loop ((ifath, gen + 1) :: (imoth, gen + 1) :: parents) persons
            | None -> loop parents ((ip, gen) :: persons)
      in
      let persons = loop [(ip_local, 0)] [] in
      List.fold_left
        (fun (accu_fam, accu_pers, accu_conn) (ip, gen) ->
           let links = get_links conf base redis ip include_not_validated in
           List.fold_left
             (fun (accu_fam, accu_pers, accu_conn) s ->
                List.fold_left
                  (fun (accu_fam, accu_pers, accu_conn) x ->
                     match String.split_on_char ':' x with
                     | [_; bname_link; id_link; "parents"] ->
                         let pl =
                           findKeyBySourcenameAndIdGlinks redis bname_link
                             (RC.FloatBound.Inclusive (float_of_string id_link))
                         in
                         begin
                           match pl with
                           | [s] ->
                               let fam =
                                 get_link_tree_curl conf conf.request conf.bname bname_link
                                   ip s "" (nb_asc - gen) (from_gen_desc - gen)
                                   (if conf.bname <> basename then gen + nb_desc else gen)
                               in
                               let corr =
                                 let from_ref = redis_p_key base ip in
                                 MLink.Connection.({
                                   from_baseprefix = conf.bname;
                                   from_ref = from_ref;
                                   to_baseprefix = bname_link;
                                   to_ref = s;
                                 })
                               in
                               (fam.MLink.Link_tree.families @ accu_fam,
                                fam.MLink.Link_tree.persons @ accu_pers,
                                corr :: fam.MLink.Link_tree.connections @ accu_conn)
                           | _ -> (accu_fam, accu_pers, accu_conn)
                         end
                     | _ -> (accu_fam, accu_pers, accu_conn))
                  (accu_fam, accu_pers, accu_conn) (json_list_of_string s))
             (accu_fam, accu_pers, accu_conn) links)
        ([], [], []) persons
    else ([], [], [])
  in
  let (distant_asc_fam, distant_asc_pers, distant_asc_conn) = distant_asc_fam in
  let (distant_desc_fam, distant_desc_pers, distant_desc_conn) = distant_desc_fam in
  let families = local_families @ distant_asc_fam @ distant_desc_fam in
  let persons = local_persons @ distant_asc_pers @ distant_desc_pers in
  let connections = local_connections @ distant_asc_conn @ distant_desc_conn in
  let data =
    MLink.Link_tree.({
      families = families;
      persons = persons;
      connections = connections;
    })
  in
  let data = MLinkext.gen_link_tree data in
  Api_util.print_result conf data

#endif
