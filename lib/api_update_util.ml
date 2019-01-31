#ifdef API

(** No security check is done here since this should only be used by admins.
    We can use [Gwdb.foi] and [Gwdb.poi]
*)

module M = Api_piqi
module Mext = Api_piqi_ext

module Mwrite = Api_saisie_write_piqi
module Mext_write = Api_saisie_write_piqi_ext

open Config
open Def
open Gwdb
open Util
open Api_util

let api_find_free_occ base fn sn =
  if fn = "" || sn = "" then None (* FIXME: || or && ? *)
  else Some (Int32.of_int @@ Gutil.find_free_occ base fn sn 0)

(**/**) (* Type de retour de modification. *)

(* Voir également update.mli, erreurs possibles :
     - "UnknownPerson"
     - "AlreadyDefined"
     - "OwnAncestor"
     - "BadSexOfMarriedPerson"
     - "BaseChanged"
     - "BadDateFormat"
     - "CreateConflictOcc"
     - "AlreadyHasParent"
     - "FatherShouldBeMale"
     - "MotherShouldBeFemale"
     - "Disconnected"
     - "error"
   On ajoute également les erreurs suivantes :
     - "PersonKey"
   => On ne renvoie en fait qu'une seule string qui est
      directement traduite du côté GeneWeb.
*)
type update_base_status =
  | UpdateSuccess of CheckItem.base_warning list * CheckItem.base_misc list * (unit -> unit) list
  | UpdateError of string
  | UpdateErrorConflict of Mwrite.Create_conflict.t


(* Exception qui gère les conflits de création de personnes. *)
exception ModErrApiConflict of Mwrite.Create_conflict.t ;;

let error_conflict_person_link base (f, s, o, create, _, force_create) =
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  | Update.Create (_, _) ->
      if f <> "?" && s <> "?" then
        if force_create then
          (* On a dit qu'on voulait forcer la création, donc on a *)
          (* calculer un occ de libre, si c'est pas le cas, c'est *)
          (* une erreur, et on doit quitter.                      *)
          if Gwdb.person_of_key base f s o <> None
          then failwith "error_conflict_person_link"
          else false
        else
          match Gwdb.persons_of_name base (f ^ " " ^ s) with
         | [] -> false
          | l ->
              (* On test nom et prénom individuellement, si c'est  *)
              (* trop gourmand, alors on pourrait juste dire qu'il *)
              (* y a un conflit.                                   *)
              let f = Name.lower f in
              let s = Name.lower s in
              let rec loop l =
                match l with
                | [] -> false
                | ip :: l ->
                    let p = poi base ip in
                    let fn = Name.lower (sou base (get_first_name p)) in
                    let sn = Name.lower (sou base (get_surname p)) in
                    if fn = f && sn = s then true
                    else loop l
              in
              loop l
        (*
        (match Gwdb.person_of_key base f s o with
         | Some ip -> true
         | None -> false)
        *)
             (*
             let fn = Util.translate_eval f in
             let sn = Util.translate_eval s in
             let key = fn ^ " " ^ sn in
             let ipl = Gutil.person_ht_find_all base key in
             let name = Name.lower (f ^ " " ^ s) in
             let rec loop ipl =
               match ipl with
               | [] -> false
               | ip :: ipl ->
                   let p1 = poi base ip in
                   if Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) = name &&
                      o = 0
                   then
                     true
                   else
                     loop ipl
             in
             loop ipl)
             *)
      else false
  | _ -> false

let check_person_conflict conf base sp =
  (* Vérification de la personne. *)
  if nb_of_persons base = 0 then ()
  else
    begin
      let op = poi base (sp.key_index) in
      let ofn = sou base (get_first_name op) in
      let osn = sou base (get_surname op) in
      let oocc = get_occ op in
      if ofn = sp.first_name && osn = sp.surname && oocc = sp.occ then ()
      else
        begin
          let fn = Util.translate_eval sp.first_name in
          let sn = Util.translate_eval sp.surname in
          let key = fn ^ " " ^ sn in
          let ipl = Gutil.person_ht_find_all base key in
          (try UpdateIndOk.check_conflict conf base sp ipl
           with Update.ModErrApi _ ->
             let conflict =
               let form = Some `person_form1 in
               let lastname = sp.surname in
               let firstname = sp.first_name in
               {
                 Mwrite.Create_conflict.form = form;
                 witness = false;
                 rparents = false;
                 event = false;
                 pos = None;
                 pos_witness = None;
                 lastname = lastname;
                 firstname = firstname;
               }
             in
             raise (ModErrApiConflict conflict))
        end;
      (* Vérification des rparents. *)
      let rec loop rparents i =
        match rparents with
        | [] -> ()
        | r :: l ->
            match (r.r_fath, r.r_moth) with
            | (Some (f, s, o, create, var, force_create), None) |
              (None, Some (f, s, o, create, var, force_create)) ->
                if error_conflict_person_link base (f, s, o, create, var, force_create) then
                  let form = Some `person_form1 in
                  let conflict =
                    {
                      Mwrite.Create_conflict.form = form;
                      witness = false;
                      rparents = true;
                      event = false;
                      pos = Some (Int32.of_int i);
                      pos_witness = None;
                      lastname = s;
                      firstname = f;
                    }
                  in
                  raise (ModErrApiConflict conflict)
                else
                  loop l (i + 1)
            | _ ->
              (* Dans l'API, ne peut pas arriver *)
              loop l (i + 1)
      in
      loop sp.rparents 0;
      (* Vérification des pevents. *)
      let rec loop pevents i =
        match pevents with
        | [] -> ()
        | evt :: l ->
            begin
            let rec loop2 witnesses j =
              match witnesses with
              | [] -> ()
              | ((f, s, o, create, var, force_create), _) :: l ->
                  if error_conflict_person_link base (f, s, o, create, var, force_create) then
                    let form = Some `person_form1 in
                    let conflict =
                      {
                        Mwrite.Create_conflict.form = form;
                        witness = true;
                        rparents = false;
                        event = true;
                        pos = Some (Int32.of_int i);
                        pos_witness = Some (Int32.of_int j);
                        lastname = s;
                        firstname = f;
                      }
                    in
                    raise (ModErrApiConflict conflict)
                  else
                    loop2 l (j + 1)
            in
            loop2 (Array.to_list evt.epers_witnesses) 0;
            loop l (i + 1)
            end
      in
      loop sp.pevents 0
    end

let check_family_conflict base sfam scpl sdes =
  (* Vérification des parents. *)
  let rec loop parents i =
    match parents with
    | [] -> ()
    | (f, s, o, create, var, force_create) :: l ->
        if error_conflict_person_link base (f, s, o, create, var, force_create) then
          let form =
            if i = 0 then Some `person_form1
            else  Some `person_form2
          in
          let conflict =
            {
              Mwrite.Create_conflict.form = form;
              witness = false;
              rparents = false;
              event = false;
              pos = None;
              pos_witness = None;
              lastname = s;
              firstname = f;
            }
          in
          raise (ModErrApiConflict conflict)
        else
          loop l (i + 1)
  in
  loop (Array.to_list (Adef.parent_array scpl)) 0;
  (* Vérification des fevents. *)
  let rec loop fevents i =
    match fevents with
    | [] -> ()
    | evt :: l ->
        begin
        let rec loop2 witnesses j =
          match witnesses with
          | [] -> ()
          | ((f, s, o, create, var, force_create), _) :: l ->
              if error_conflict_person_link base (f, s, o, create, var, force_create) then
                let form = Some `family_form in
                let conflict =
                  {
                    Mwrite.Create_conflict.form = form;
                    witness = true;
                    rparents = false;
                    event = true;
                    pos = Some (Int32.of_int i);
                    pos_witness = Some (Int32.of_int j);
                    lastname = s;
                    firstname = f;
                  }
                in
                raise (ModErrApiConflict conflict)
              else
                loop2 l (j + 1)
        in
        loop2 (Array.to_list evt.efam_witnesses) 0;
        loop l (i + 1)
        end
  in
  loop sfam.fevents 0;
  (* Vérification des enfants. *)
  let rec loop children i =
    match children with
    | [] -> ()
    | (f, s, o, create, var, force_create) :: l ->
        if error_conflict_person_link base (f, s, o, create, var, force_create) then
          let form = Some `person_form1 in
          let conflict =
            {
              Mwrite.Create_conflict.form = form;
              witness = false;
              rparents = false;
              event = false;
              pos = None;
              pos_witness = None;
              lastname = s;
              firstname = f;
            }
          in
          raise (ModErrApiConflict conflict)
        else
          loop l (i + 1)
  in
  loop (Array.to_list sdes.children) 0


(**/**) (* Convertion d'une date. *)


(* ************************************************************************ *)
(*  [Fonc] piqi_date_of_date : def.date -> piqi_date                        *)
(** [Description] : Converti une date en date piqi
    [Args] :
      - date : la date a convertir
    [Retour] :
      - piqi date : date du module Mwrite.
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
        let dmy1 = {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;} in
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
                {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`oryear, Some dmy2)
          | YearInt dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {Mwrite.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        Mwrite.Date.cal = cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Dtext txt ->
      {
        Mwrite.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }


(* ************************************************************************ *)
(*  [Fonc] date_of_piqi_date : piqi_date -> option def.date                 *)
(** [Description] : Converti date piqi en date
    [Args] :
      - date : date du module Mwrite
    [Retour] :
      - date : date
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let date_of_piqi_date conf date =
  match date.Mwrite.Date.text with
  | Some txt -> Some (Dtext txt)
  | _ ->
      (* Si on a une année, on a une date. *)
      match date.Mwrite.Date.dmy with
      | Some dmy ->
          begin
            match dmy.Mwrite.Dmy.year with
            | Some _ ->
                let cal =
                  match date.Mwrite.Date.cal with
                  | Some `julian -> Djulian
                  | Some `french -> Dfrench
                  | Some `hebrew -> Dhebrew
                  | _ -> Dgregorian
                in
                let get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec =
                  let day =
                    match dmy.Mwrite.Dmy.day with
                    | Some day -> Int32.to_int day
                    | None -> 0
                  in
                  let month =
                    match dmy.Mwrite.Dmy.month with
                    | Some month -> Int32.to_int month
                    | None -> 0
                  in
                  let year =
                    match dmy.Mwrite.Dmy.year with
                    | Some year -> Int32.to_int year
                    | None -> 0
                  in
                  let delta =
                    match dmy.Mwrite.Dmy.delta with
                    | Some delta -> Int32.to_int delta
                    | None -> 0
                  in
                  (* Error handling. *)
                  let (day, month, year) =
                    if year = 0 && month <= 0
                    then
                        (0, 0, year)
                    else
                        (day, month, year)
                  in
                  let adef_dmy =
                    {day = day; month = month; year = year; delta = delta; prec = prec}
                  in
                  let day_to_check =
                    adef_dmy.day >= 1 && adef_dmy.day <= 31
                  in
                  let month_to_check =
                    adef_dmy.month >= 1 && adef_dmy.month <= 13
                  in
                  (* Returns date directy if there is no month. *)
                  if adef_dmy.month = 0 then adef_dmy
                  (* If no specified day, checks the month value. *)
                  else if adef_dmy.day = 0 && month_to_check
                  then
                    (* Check the month in the gregorian calendar. *)
                    if cal = Dgregorian
                    then
                      begin
                        (* The day is set to 1 for checking. *)
                        Update.check_greg_day conf {day = 1; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  (* Day and month are specified here. *)
                  else if day_to_check && month_to_check
                  then
                    (* Check the date in the gregorian calendar. *)
                    if cal = Dgregorian
                    then
                      begin
                        Update.check_greg_day conf {day = adef_dmy.day; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  else
                    Update.bad_date conf adef_dmy
                in
                let delta2 = 0 in
                let prec =
                  match date.Mwrite.Date.prec with
                  | Some `about -> About
                  | Some `maybe -> Maybe
                  | Some `before -> Before
                  | Some `after -> After
                  | Some `oryear ->
                      (match date.Mwrite.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Mwrite.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Sure in
                              OrYear {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Sure
                          end
                      | None -> Sure (*OrYear {day2 = 0; month2 = 0; year2 = 0; delta2 = 0}*) (* erreur*))
                  | Some `yearint ->
                      (match date.Mwrite.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Mwrite.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Sure in
                              YearInt {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Sure
                          end
                      | None -> Sure (*YearInt {day2 = 0; month2 = 0; year2 = 0; delta2 = 0}*) (* erreur*))
                  | _ -> Sure
                in
                let dmy =
                  match date.Mwrite.Date.dmy with
                  | Some dmy ->
                      get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec
                  | None -> (* erreur*)
                      {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
                in
                let dmy =
                  match cal with
                  | Dgregorian ->
                      let _check_date = Update.check_greg_day conf dmy in
                      dmy
                  | Djulian -> Calendar.gregorian_of_julian dmy
                  | Dfrench -> Calendar.gregorian_of_french dmy
                  | Dhebrew -> Calendar.gregorian_of_hebrew dmy
                in
                Some (Dgreg (dmy, cal))
          | None -> None
          end
      | None -> None


(**/**) (* Convertion d'une personne pour la lecture. *)


(* Copie de util.ml pour supprimer le html *)

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (eq_istr (get_surname p) (get_surname fath)) then
      person_text_no_html conf base fath
    else
      gen_person_text_no_html (p_first_name, (fun _ _ -> "")) conf base fath
  in
  let a = poi base (get_key_index p) in
  let ifam =
    match get_parents a with
    | Some ifam ->
        let cpl = foi base ifam in
        let fath =
          let fath = poi base (get_father cpl) in
          if p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = poi base (get_mother cpl) in
          if p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
  | Some (None, None) | None -> ""
  | Some (fath, moth) ->
      let s =
        match (fath, moth) with
        | (Some fath, None) -> print_father fath
        | (None, Some moth) -> person_text_no_html conf base moth
        | (Some fath, Some moth) ->
            print_father fath ^ " " ^ transl_nth conf "and" 0 ^ " " ^
              person_text_no_html conf base moth
        | _ -> ""
      in
      let is = index_of_sex (get_sex p) in
      translate_eval
        (transl_a_of_gr_eq_gen_lev conf
           (transl_nth conf "son/daughter/child" is) s)

let pers_to_piqi_simple_person conf base p =
  let (birth_date, death_date, _) = Date.get_birth_death_date p in
  let birth_short_date = Opt.map (Date.string_slash_of_date conf) birth_date in
  let birth_place =
    match to_piqi_birth_place_opt base p with
    | Some s -> Some (Util.string_of_place conf s)
    | None ->
      Opt.map (Util.string_of_place conf) (to_piqi_baptism_place_opt base p)
  in
  let death_short_date = Opt.map (Date.string_slash_of_date conf) death_date  in
  let death_place =
    match to_piqi_death_place_opt base p with
    | Some s -> Some (Util.string_of_place conf s)
    | None ->
      Opt.map (Util.string_of_place conf) (to_piqi_burial_place_opt base p)
  in
  let (firstname, lastname) = person_firstname_surname_txt base p in
  { Mwrite.Simple_person.index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname
  ; firstname
  ; birth_short_date
  ; birth_place
  ; death_short_date
  ; death_place
  ; image = to_piqi_image_opt base p
  ; sosa = to_piqi_sosa p
  }

let husband_wife conf base p =
  let rec loop i =
    if i < Array.length (get_family p) then
      let fam = foi base (get_family p).(i) in
      let conjoint = Gutil.spouse (get_key_index p) fam in
      let conjoint = poi base conjoint in
      if p_first_name base conjoint <> "?" || p_surname base conjoint <> "?"
      then
        translate_eval
          (Printf.sprintf (relation_txt conf (get_sex p) fam) (fun () -> "")
           ^ " " ^ (person_text_no_html conf base conjoint) )
      else loop (i + 1)
    else ""
  in
  loop 0

let pers_to_piqi_person_search conf base p =
  let (firstname, lastname) = person_firstname_surname_txt base p in
  let dates = short_dates_text conf p in
  let family =
    let hw = husband_wife conf base p in
    if hw <> "" then hw
    else child_of_parent conf base p
  in
  { Mwrite.Person_search.index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname
  ; firstname
  ; dates = Opt.of_string dates
  ; image = to_piqi_image_opt base p
  ; sosa = to_piqi_sosa p
  ; family
  }

let pers_to_piqi_person_search_info conf base p =
  let events =
    Api_saisie_read.fill_events
      conf base p ""
      (fun conf base p _ -> pers_to_piqi_simple_person conf base p)
      (fun witness_type witness -> Mwrite.Witness_event.({ witness_type ; witness }))
      (fun ~name ~type_:_ ~date ~date_long:_ ~date_raw:_ ~date_conv
        ~date_conv_long:_ ~date_cal ~place ~note ~src ~spouse ~witnesses ->
        { Mwrite.Event.name
        ; date
        ; date_conv
        ; date_cal
        ; place
        ; reason = None
        ; note
        ; src
        ; spouse
        ; witnesses
        })
  in
  let titles =
    Perso.nobility_titles_list conf base p
    |> List.map (Perso.string_of_title { conf with cancel_links = true } base "" p)
  in
  let related =
    let list =
      let list = List.sort_uniq compare (get_related p) in
      List.fold_left
        (fun list ic ->
           let c = poi base ic in
           let rec loop list =
             function
             | r :: rl ->
               (match r.r_fath with
                | Some ip when ip = get_key_index p ->
                  loop ((c, r) :: list) rl
                | _ ->
                  (match r.r_moth with
                   | Some ip when ip = get_key_index p ->
                     loop ((c, r) :: list) rl
                   | _ -> loop list rl))
             | [] -> list
           in loop list (get_rparents c))
        [] list
    in
    let list =
      List.sort
        (fun (c1, _) (c2, _) ->
           let d1 =
             match Adef.od_of_cdate (get_baptism c1) with
             | None -> Adef.od_of_cdate (get_birth c1)
             | x -> x
           in
           let d2 =
             match Adef.od_of_cdate (get_baptism c2) with
             | None -> Adef.od_of_cdate (get_birth c2)
             | x -> x
           in
           match (d1, d2) with
           |(Some d1, Some d2) ->
             if CheckItem.strictly_before d1 d2 then -1 else 1
           | _ -> -1 )
        (List.rev list)
    in
    List.map
      (fun (p, rp) ->
         let p = pers_to_piqi_simple_person conf base p in
         let r_type =
           match rp.r_type with
           | Adoption -> `rchild_adoption
           | Recognition -> `rchild_recognition
           | CandidateParent -> `rchild_candidate_parent
           | GodParent -> `rchild_god_parent
           | FosterParent -> `rchild_foster_parent
         in
         {
           Mwrite.Relation_person.r_type = r_type;
           person = p;
         } )
      list
  in
  let rparents =
    List.fold_left
      (fun rl rp ->
         let r_type =
           match rp.r_type with
           | Adoption -> `rparent_adoption
           | Recognition -> `rparent_recognition
           | CandidateParent -> `rparent_candidate_parent
           | GodParent -> `rparent_god_parent
           | FosterParent -> `rparent_foster_parent
         in
         let rl =
           match rp.r_fath with
           | Some ip ->
             let p = poi base ip in
             let p = pers_to_piqi_simple_person conf base p in
             let p =
               {
                 Mwrite.Relation_person.r_type = r_type;
                 person = p;
               }
             in
             p :: rl
           | None -> rl
         in
         match rp.r_moth with
         | Some ip ->
           let p = poi base ip in
           let p = pers_to_piqi_simple_person conf base p in
           let p =
             {
               Mwrite.Relation_person.r_type = r_type;
               person = p;
             }
           in
           p :: rl
         | None -> rl)
      [] (get_rparents p)
  in
  let was_witness =
    let list =
      let list = ref [] in
      let related = List.sort_uniq compare (get_related p) in
      let rec make_list =
        function
        | ic :: icl ->
          let c = poi base ic in
          if get_sex c = Male then
            Array.iter
              (fun ifam ->
                 let fam = foi base ifam in
                 if Array.mem (get_key_index p) (get_witnesses fam)
                 then
                   list := (ifam, fam) :: !list
                 else ())
              (get_family (poi base ic))
          else ();
          make_list icl
        | [] -> ()
      in
      make_list related;
      !list
    in
    let list =
      List.sort
        (fun (_, fam1) (_, fam2) ->
           match
             (Adef.od_of_cdate (get_marriage fam1),
              Adef.od_of_cdate (get_marriage fam2))
           with
           | (Some d1, Some d2) ->
             if CheckItem.strictly_before d1 d2 then -1
             else if CheckItem.strictly_before d2 d1 then 1
             else 0
           | _ -> 0 )
        list
    in
    List.map
      (fun (_, fam) ->
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let father = poi base ifath in
         let mother = poi base imoth in
         let father_auth = authorized_age conf base father in
         let husband =
           if not father_auth && (is_hide_names conf father) then "x x"
           else p_first_name base father ^ " " ^ p_surname base father
         in
         let mother_auth = authorized_age conf base mother in
         let wife =
           if not mother_auth && (is_hide_names conf mother) then "x x"
           else p_first_name base mother ^ " " ^ p_surname base mother
         in
         (*
         let husband = pers_to_piqi_simple_person conf base father in
         let wife = pers_to_piqi_simple_person conf base mother in
         *)
         Mwrite.Was_witness.({
             husband = husband;
             wife = wife;
           }) )
      list
  in
  let psources = Api_saisie_read.fill_psources conf base p in
  { Mwrite.Person_search_info.index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname = to_piqi_surname base p
  ; firstname = to_piqi_firstname base p
  ; public_name = to_piqi_publicname base p
  ; aliases = to_piqi_aliases base p
  ; qualifiers = to_piqi_qualifiers base p
  ; firstname_aliases = to_piqi_firstname_aliases base p
  ; surname_aliases = to_piqi_surname_aliases base p
  ; image = to_piqi_image_opt base p
  ; events
  ; occupation = to_piqi_occupation_opt_wiki conf base p
  ; notes = Api_saisie_read.fill_pnotes { conf with no_note = false } base p
  ; psources
  ; has_sources = psources <> None
  ; titles = titles
  ; related = related
  ; rparents = rparents
  ; was_witness = was_witness
  ; sosa = to_piqi_sosa p
  }

(* !!! Double check this !!! *)
let pers_to_piqi_person_link conf base p =
  let dates = match short_dates_text conf p with
    | "" -> None
    | x -> Some ("(" ^ x ^ ")")
  in
  { Mwrite.Person_link.create_link = `link
  ; index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname = to_piqi_surname base p
  ; firstname = to_piqi_firstname base p
  ; occ = to_piqi_occ_opt p
  ; dates = dates;
  }

let pers_to_piqi_mod_person conf base p =
  let titles =
    let fn ~title_type:_ ~name ~title ~fief ~date_begin ~date_end ~nth =
      { Mwrite.Title.name ; title ; fief ; nth
      ; date_begin = Opt.map piqi_date_of_date date_begin
      ; date_end = Opt.map piqi_date_of_date date_end
      }
    in
    to_piqi_titles fn base p
  in
  let events = get_pevents p in
  let death_type = to_piqi_death_type p in
  let pevents =
    List.map
      (fun evt ->
         let (pevent_type, event_perso) =
           match evt.epers_name with
           | Epers_Name n -> (None, Some (sou base n))
           | x -> (Some (to_piqi_pevent_aux x), None)
         in
         let witnesses =
           List.map
             (fun (ip, wk) ->
                let witness_type =
                  match wk with
                  | Witness -> `witness
                  | Witness_GodParent -> `witness_godparent
                in
                let p = poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                Mwrite.Witness.({
                  witness_type = witness_type;
                  person = Some person_link;
                }))
             (Array.to_list evt.epers_witnesses)
         in
         { Mwrite.Pevent.pevent_type
         ; date = Opt.map piqi_date_of_date @@ Adef.od_of_cdate evt.epers_date
         ; place = to_piqi_string_opt_aux base evt.epers_place
         ; reason = None
         ; note = to_piqi_string_opt_aux base evt.epers_note
         ; src = to_piqi_string_opt_aux base evt.epers_src
         ; witnesses = witnesses
         ; event_perso
         })
      events
  in
  (* Si la personne n'a aucun évènement et/ou est décédée mais *)
  (* sans évènement, on ajoute les évènements nécessaires.     *)
  let pevents =
    let mk_pevents pevent_type =
      { Mwrite.Pevent.pevent_type = Some pevent_type
      ; date = None
      ; place = None
      ; reason = None
      ; note = None
      ; src = None
      ; witnesses = []
      ; event_perso = None
      }
    in
    let pevents =
      if List.exists (fun e -> e.epers_name = Epers_Birth) events
      then pevents
      else mk_pevents `epers_birth :: pevents
    in
    if not (List.exists (fun e -> e.epers_name = Epers_Death) events)
    && Adef.int_of_iper (get_key_index p) >= 0
    && death_type != `not_dead
    then pevents @ [ mk_pevents `epers_death ]
    else pevents
  in
  let rparents =
    let fn ~father ~mother ~source ~rpt_type =
      let person =
        match father, mother with
        | Some i, None | None, Some i ->
          let i = Adef.iper_of_int (Int32.to_int i) in
          Some (pers_to_piqi_person_link conf base (poi base i))
        | _ -> assert false (* FIXME *)
      in
      let rpt_type =
        match father, mother with
        | Some _, None -> begin match rpt_type with
            | `rpt_adoption -> `rpt_adoption_father
            | `rpt_candidate_parent -> `rpt_candidate_parent_father
            | `rpt_foster_parent -> `rpt_foster_parent_father
            | `rpt_god_parent -> `rpt_god_parent_father
            | `rpt_recognition -> `rpt_recognition_father
          end
        | None, Some _ -> begin match rpt_type with
            | `rpt_adoption -> `rpt_adoption_mother
            | `rpt_candidate_parent -> `rpt_candidate_parent_mother
            | `rpt_foster_parent -> `rpt_foster_parent_mother
            | `rpt_god_parent -> `rpt_god_parent_mother
            | `rpt_recognition -> `rpt_recognition_mother
          end
        | _ -> assert false     (* FIXME *)
      in
      { Mwrite.Relation_parent.rpt_type
      ; person
      ; source
      }
    in
    to_piqi_rparents fn base p
  in
  { Mwrite.Person.digest = Update.digest_person (UpdateInd.string_person_of base p)
  ; index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname = to_piqi_surname base p
  ; firstname = to_piqi_firstname base p
  ; occ = to_piqi_occ_opt p
  ; public_name = to_piqi_publicname base p
  ; aliases = to_piqi_aliases base p
  ; qualifiers = to_piqi_qualifiers base p
  ; firstname_aliases = to_piqi_firstname_aliases base p
  ; surname_aliases = to_piqi_surname_aliases base p
  ; image = to_piqi_image_opt base p
  ; death_type = death_type
  ; occupation = to_piqi_occupation_opt base p
  ; psources = to_piqi_psources_opt base p
  ; notes = to_piqi_notes_opt base p
  ; titles = titles
  ; pevents = pevents
  ; related = to_piqi_related_unboxed p
  ; rparents
  ; access = to_piqi_access p
  ; parents = to_piqi_parents p
  ; families = to_piqi_families_unboxed p
  ; create_link = `link;
  }

(* ************************************************************************ *)
(*  [Fonc] fam_to_piqi_mod_family :
             config -> base -> ip -> family -> piqi family                  *)
(** [Description] : Converti une personne en personne piqi.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] :
      - piqi person : person du module Mwrite.
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
let fam_to_piqi_mod_family conf base ifam fam =
  let digest = "" in
  let index = Int32.of_int (Adef.int_of_ifam ifam) in
  let fevents =
    List.map
      (fun evt ->
         let (fevent_type, event_perso) =
           match evt.efam_name with
           | Efam_Marriage -> (Some `efam_marriage, None)
           | Efam_NoMarriage -> (Some `efam_no_marriage, None)
           | Efam_NoMention -> (Some `efam_no_mention, None)
           | Efam_Engage -> (Some `efam_engage, None)
           | Efam_Divorce -> (Some `efam_divorce, None)
           | Efam_Separated -> (Some `efam_separated, None)
           | Efam_Annulation -> (Some `efam_annulation, None)
           | Efam_MarriageBann -> (Some `efam_marriage_bann, None)
           | Efam_MarriageContract -> (Some `efam_marriage_contract, None)
           | Efam_MarriageLicense -> (Some `efam_marriage_license, None)
           | Efam_PACS -> (Some `efam_pacs, None)
           | Efam_Residence -> (Some `efam_residence, None)
           | Efam_Name n -> (None, Some (sou base n))
         in
         let date =
           match Adef.od_of_cdate evt.efam_date with
           | Some d -> Some (piqi_date_of_date d)
           | _ -> None
         in
         let place = sou base evt.efam_place in
         let reason = None in
         let note = sou base evt.efam_note in
         let src = sou base evt.efam_src in
         let witnesses =
           List.map
             (fun (ip, wk) ->
                let witness_type =
                  match wk with
                  | Witness -> `witness
                  | Witness_GodParent -> `witness_godparent
                in
                let p = poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                Mwrite.Witness.({
                  witness_type = witness_type;
                  person = Some person_link;
                }))
             (Array.to_list evt.efam_witnesses)
         in
         {
           Mwrite.Fevent.fevent_type = fevent_type;
           date = date;
           place = if place = "" then None else Some place;
           reason = reason;
           note = if note = "" then None else Some note;
           src = if src = "" then None else Some src;
           witnesses = witnesses;
           event_perso = event_perso;
         })
      (get_fevents fam)
  in
  let fsources = sou base (get_fsources fam) in
  let origin_file = sou base (get_origin_file fam) in
  let comment = sou base (get_comment fam) in
  let father = poi base (get_father fam) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = poi base (get_mother fam) in
  let mother = pers_to_piqi_mod_person conf base mother in
  let children =
    List.map
      (fun ip ->
         let child = poi base ip in
         pers_to_piqi_person_link conf base child)
      (Array.to_list (get_children fam))
  in
  (* Compatibilité avec GeneWeb. *)
  let old_witnesses =
    List.map
      (fun ip -> Int32.of_int (Adef.int_of_iper ip))
      (Array.to_list (get_witnesses fam))
  in
  {
    Mwrite.Family.digest = digest;
    index = index;
    fevents = fevents;
    fsources = if fsources = "" then None else Some fsources;
    comment = if comment = "" then None else Some comment;
    origin_file = if origin_file = "" then None else Some origin_file;
    father = father;
    mother = mother;
    children = children;
    old_witnesses = old_witnesses;
  }


(* ************************************************************************** *)
(*  [Fonc] piqi_mod_person_of_person_start :
             config -> base -> Person_start -> Person                         *)
(** [Description] : Converti une personne start pour la première saisie en
                    Person afin de suivre le chemin classique de modification
                    de la base.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - start_    : Person_start
    [Retour] : Person
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let piqi_mod_person_of_person_start conf base start_p =
  let p = Gwdb.empty_person base (Adef.iper_of_int (-1)) in
  let mod_p = pers_to_piqi_mod_person conf base p in
  (* Les index négatifs ne marchent pas. *)
  mod_p.Mwrite.Person.index <- Int32.of_int 0;
  mod_p.Mwrite.Person.lastname <- start_p.M.Person_start.lastname;
  mod_p.Mwrite.Person.firstname <- start_p.M.Person_start.firstname;
  mod_p.Mwrite.Person.sex <- start_p.M.Person_start.sex;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  mod_p.Mwrite.Person.access <- `access_iftitles;
  let birth_date =
    match start_p.M.Person_start.birth_date_year with
    | Some y ->
        let y = Int32.to_int y in
        if y > 0 then
          (match start_p.M.Person_start.birth_date_month with
           | Some m ->
               let m = Int32.to_int m in
               (match start_p.M.Person_start.birth_date_day with
               | Some d ->
                   let d = Int32.to_int d in
                   let dmy =
                     {day = d; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Dgreg (dmy, Dgregorian))
               | None ->
                   let dmy =
                     {day = 0; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Dgreg (dmy, Dgregorian)))
           | None ->
               let dmy =
                 {day = 0; month = 0; year = y; prec = Sure; delta = 0}
               in
               Some (Dgreg (dmy, Dgregorian)))
        else
          None
    | None -> None
  in
  let birth_date =
    match birth_date with
    | Some d -> Some (piqi_date_of_date d)
    | _ -> None
  in
  let birth =
    {
      Mwrite.Pevent.pevent_type = Some `epers_birth;
      date = birth_date;
      place = None;
      reason = None;
      note = None;
      src = None;
      witnesses = [];
      event_perso = None;
    }
  in
  mod_p.Mwrite.Person.pevents <- [birth];
  mod_p


(**/**) (* Famille vide. *)


let piqi_empty_family conf base ifam =
  let father = Gwdb.empty_person base (Adef.iper_of_int (-1)) in
  let mother = Gwdb.empty_person base (Adef.iper_of_int (-1)) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = pers_to_piqi_mod_person conf base mother in
  (* Les index négatifs ne marchent pas ! *)
  father.Mwrite.Person.index <- Int32.of_int 0;
  mother.Mwrite.Person.index <- Int32.of_int 0;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  father.Mwrite.Person.access <- `access_iftitles;
  mother.Mwrite.Person.access <- `access_iftitles;
  let fevents =
    let evt =
      {
        Mwrite.Fevent.fevent_type = Some `efam_marriage;
        date = None;
        place = None;
        reason = None;
        note = None;
        src = None;
        witnesses = [];
        event_perso = None;
      }
    in
    [evt]
  in
  {
    Mwrite.Family.digest = "";
    index = Int32.of_int (Adef.int_of_ifam ifam);
    fevents = fevents;
    fsources = None;
    comment = None;
    origin_file = None;
    father = father;
    mother = mother;
    children = [];
    old_witnesses = [];
  }

(* List of strings in which some characters were removed. *)
let removed_string = ref [] ;;

let reconstitute_somebody base person =
  let create_link = person.Mwrite.Person_link.create_link in
  let (fn, sn, occ, create, var, force_create) = match create_link with
    | `link ->
      let ip = Int32.to_int person.Mwrite.Person_link.index in
      let p = poi base (Adef.iper_of_int ip) in
      let fn = sou base (get_first_name p) in
      let sn = sou base (get_surname p) in
      let occ =
        if fn = "?" || sn = "?" then
          Adef.int_of_iper (get_key_index p)
        else get_occ p
      in
      (fn, sn, occ, Update.Link, "", false)
    | _ ->
      let sex =
        match person.Mwrite.Person_link.sex with
          | `male -> Male
          | `female -> Female
          | `unknown -> Neuter
      in
      let fn = person.Mwrite.Person_link.firstname in
      let sn = person.Mwrite.Person_link.lastname in
      let (occ, force_create) = match create_link with
        | `create_default_occ ->
          (match person.Mwrite.Person_link.occ with
            | Some occ -> (Int32.to_int occ, false)
            | None -> (0, false))
        | `create ->
          (* Update the person because if we want to find it, we have to know its occ. *)
          person.Mwrite.Person_link.occ <- api_find_free_occ base fn sn ;
          (Opt.map_default 0 Int32.to_int (person.Mwrite.Person_link.occ), true)
        | _ -> (0, false) (* Should not happen. *)
      in
      (fn, sn, occ, Update.Create (sex, None), "", force_create)
  in
  let (fn, sn) =
    (* If there are forbidden characters, delete them. *)
    let contain_fn = String.contains fn in
    let contain_sn = String.contains sn in
    if (List.exists contain_fn Name.forbidden_char)
      || (List.exists contain_sn Name.forbidden_char) then
      begin
        removed_string :=
          (Name.purge fn ^ " " ^ Name.purge sn) :: !removed_string;
        (Name.purge fn, Name.purge sn)
      end
    else (fn, sn)
  in
  (fn, sn, occ, create, var, force_create)

#endif
