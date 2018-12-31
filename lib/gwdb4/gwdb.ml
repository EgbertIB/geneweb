open Def
open Json

type iper = string
type ifam = string
type istr = string

module J = Yojson.Basic.Util

let string_of_iper : iper -> string = fun x -> x
let string_of_ifam : ifam -> string = fun x -> x
let string_of_istr : istr -> string = fun x -> x

let iper_of_string x = x
let ifam_of_string x = x
let istr_of_string x = x

type person = Yojson.Basic.json
type family = Yojson.Basic.json

type relation = (iper, istr) gen_relation
type title = istr gen_title
type pers_event = (iper, istr) gen_pers_event
type fam_event = (iper, istr) gen_fam_event

type string_person_index =
  { find : istr -> iper list
  ; cursor : string -> istr
  ; next : istr -> istr
  }

type base = (string * (string -> string))

let open_base name =
  ( name
  , fun request ->
    let url = Printf.sprintf "http://localhost:8529/_db/Trees/geneweb/%s/%s" name request in
    (* print_endline __LOC__ ;
     * print_endline url ;
     * print_endline __LOC__ ; *)
    Curl.global_init Curl.CURLINIT_GLOBALALL;
    let res = ref "" in
    let result = Buffer.create 16384
    and errorBuffer = ref "" in
    begin try
        let connection = Curl.init () in
        let headers = [] in
        Curl.set_httpheader connection headers;
        Curl.set_errorbuffer connection errorBuffer;
        Curl.set_writefunction connection
          (fun data ->
             Buffer.add_string result data;
             String.length data);
        Curl.set_followlocation connection true;
        Curl.set_url connection url;
        Curl.set_timeoutms connection 10000;
        Curl.perform connection;
        Curl.cleanup connection;
        res := Buffer.contents result
      with
      | Curl.CurlException _ ->
        Printf.fprintf stderr "Error: %s\n" !errorBuffer
      | Failure s ->
        Printf.fprintf stderr "Caught exception: %s\n" s
    end ;
    Curl.global_cleanup () ;
    !res
  )

let close_base _base = ()

let dummy_iper = ""
let dummy_ifam = ""
let dummy_istr = ""

let eq_istr = (=)
let is_empty_string = (=) ""
let is_quest_string = (=) "?"
let empty_person _ _ = `Null
let empty_family _ _ = `Null

let get_access p =
  match J.member "access" p with
  | `Int 2 -> Private
  | `Int 1 -> Public
  | `Int 0 -> IfTitles
  | _ -> failwith __LOC__

let get_aliases p =
  match J.member "aliases" p with
  | `List l -> List.map J.to_string l
  | `Null -> []
  | _ -> failwith __LOC__

let get_pevents p =
  get_list p "pevents" pevent_of_json

let get_event_aux names fn =
  let rec loop = function
    | [] -> None
    | e :: _ when List.mem e.epers_name names -> Some e
    | _ :: tl -> loop tl
  in
  ( (fun p -> fn @@ loop (get_pevents p) )
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_place | None -> "")
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_note | None -> "")
  , (fun p -> match loop (get_pevents p) with Some e -> e.epers_src | None -> "")
  )

let get_baptism, get_baptism_place, get_baptism_note, get_baptism_src =
  get_event_aux [ Epers_Baptism ] @@
  function Some e -> e.epers_date | None -> Adef.cdate_None

let get_birth, get_birth_place, get_birth_note, get_birth_src =
  get_event_aux [ Epers_Birth ] @@
  function Some e -> e.epers_date | None -> Adef.cdate_None

let get_burial, get_burial_place, get_burial_note, get_burial_src =
  get_event_aux [ Epers_Cremation ; Epers_Burial ] @@
  function
  | Some { epers_name = Epers_Cremation ; epers_date } -> Cremated epers_date
  | Some { epers_name = Epers_Burial ; epers_date } -> Buried epers_date
  | _ -> UnknownBurial

(* FIXME *)
let get_death, get_death_place, get_death_note, get_death_src =
  get_event_aux [ Epers_Death ] @@
  function Some e -> Death (Unspecified, e.epers_date)
         | None -> NotDead

let get_first_name p =
  match J.member "firstname" p with
  | `String s -> s
  | _ -> ""

let get_first_names_aliases p =
  get_list p "first_names_aliases" J.to_string

let get_image p =
  get_string p "image"

let get_key_index : person -> iper = fun p ->
  "pierfit:" ^ string_of_int (get_int ~__LOC__ p "index")      (* FIXME *)

let get_notes p =
  get_string p "note"

let get_occ p =
  get_int ~__LOC__ p "occ"

let get_occupation p =
  get_string p "occupation"

let get_psources p =
  get_string p "psources"

let get_public_name p =
  get_string p "public_name"

let get_qualifiers p =
  get_list p "qualifiers" J.to_string

let get_related p =
  get_list p "related" J.to_int
  |> List.map (fun i -> "pierfit:" ^ string_of_int i) (* FIXME *)

let get_rparents p =
  get_list p "rparents" rparent_of_json

let get_parents p =
  match J.member "parents" p with
  | `Int i -> Some ("pierfit:" ^ string_of_int i)
  | _ -> None (* FIXME *)

let get_sex p = match get_int ~__LOC__ p "sex" with
  | 1 -> Def.Male
  | 2 -> Def.Female
  | _ -> Def.Neuter

let get_surname p =
  match J.member "lastname" p with
  | `String s -> s
  | _ -> ""

let get_surnames_aliases p =
  get_list p "surnames_aliases" J.to_string

let get_titles : person -> title list = fun _p -> []

let clear_families_array _ = ()
let clear_persons_array _ = ()
let clear_strings_array _ = ()
let clear_descends_array _ = ()
let clear_couples_array _ = ()
let clear_unions_array _ = ()
let clear_ascends_array _ = ()
let load_families_array _ = ()
let load_persons_array _ = ()
let load_strings_array _ = ()
let load_descends_array _ = ()
let load_couples_array _ = ()
let load_unions_array _ = ()
let load_ascends_array _ = ()

let sou _base istr = istr

let foi_cache = Hashtbl.create 42

let foi (_, get) ifam =
  try Hashtbl.find foi_cache ifam
  with Not_found ->
    let x =
      get @@ "families/" ^ ifam
      |> Yojson.Basic.from_string
      |> J.member "family"
    in
    Hashtbl.add foi_cache ifam x ;
    x

let poi_cache = Hashtbl.create 42

(* FIXME *)
let poi (_, get) iper =
  try Hashtbl.find poi_cache iper
  with Not_found ->
    print_endline @@ Printf.sprintf "%s:%s" __LOC__ iper ;
    let x =
      match Yojson.Basic.from_string @@ get @@ "persons/" ^ iper with
      | `List (hd :: _) -> (* print_endline __LOC__ ;  *)J.member "person" hd
      | x -> (* print_endline __LOC__ ;  *)J.member "person" x
    in
    (* print_endline (Yojson.Basic.to_string x) ; *)
    Hashtbl.add poi_cache iper x ;
    x

let family_of_gen_family _base (f, _c, _d)
  =
  let open Def in
  `Assoc [ ("marriage", json_of_cdate f.marriage)
         ; ("marriage_place", `String f.marriage_place)
         ; ("marriage_note", `String f.marriage_note)
         ; ("marriage_src", `String f.marriage_src)
         ; ("witnesses", `List (Array.to_list @@ Array.map (fun x -> `String x) f.witnesses) )
         ; ("relation", json_of_relation_kind f.relation)
         ; ("divorce", json_of_divorce f.divorce)
         ; ("fevents", `List (List.map json_of_fevent f.fevents))
         ; ("comment", `String f.comment)
         ; ("origin_file", `String f.origin_file)
         ; ("fsources", `String f.fsources)
         ; ("fam_index", `String f.fam_index)
         ]

let json_of_death _ = failwith __LOC__
let json_of_burial _ = failwith __LOC__

let person_of_gen_person _base (p, _a, _u) =
  let open Def in
  `Assoc [ ("first_name", `String p.first_name)
         ; ("lastname", `String p.surname)
         ; ("occ", `Int p.occ)
         ; ("image", `String p.image)
         ; ("public_name", `String p.public_name)
         ; ("qualifiers", `List (List.map (fun x -> `String x) p.qualifiers) )
         ; ("aliases", `List (List.map (fun x -> `String x) p.aliases) )
         ; ("first_names_aliases", `List (List.map (fun x -> `String x) p.first_names_aliases) )
         ; ("surnames_aliases", `List (List.map (fun x -> `String x) p.surnames_aliases) )
         ; ("titles", `List (List.map json_of_title p.titles))
         ; ("rparents", `List (List.map json_of_rparent p.rparents))
         ; ("related", `List (List.map (fun x -> `String x) p.related))
         ; ("occupation", `String p.occupation)
         ; ("sex", match p.sex with Male -> `Int 0 | Female -> `Int 1 | Neuter -> `Int 2)
         ; ("access", match p.access with Private -> `Int 2 | Public  -> `Int 1 | IfTitles -> `Int 0)
         ; ("birth", json_of_cdate p.birth)
         ; ("birth_place", `String p.birth_place)
         ; ("birth_note", `String p.birth_note)
         ; ("birth_src", `String p.birth_src)
         ; ("baptism", json_of_cdate p.baptism)
         ; ("baptism_place", `String p.baptism_place)
         ; ("baptism_note", `String p.baptism_note)
         ; ("baptism_src", `String p.baptism_src)
         ; ("death", json_of_death p.death)
         ; ("death_place", `String p.death_place)
         ; ("death_note", `String p.death_note)
         ; ("death_src", `String p.death_src)
         ; ("burial", json_of_burial p.burial)
         ; ("burial_place", `String p.burial_place)
         ; ("burial_note", `String p.burial_note)
         ; ("burial_src", `String p.burial_src)
         ; ("pevents", `List (List.map json_of_pevent p.pevents))
         ; ("notes", `String p.notes)
         ; ("psources", `String p.psources)
         ; ("key_index", `String p.key_index)
         ]

let gen_person_of_person : person -> (iper, iper, istr) Def.gen_person =
  fun p ->
  let open Def in
  { first_name = get_first_name p
  ; surname = get_surname p
  ; occ = get_occ p
  ; image = get_image p
  ; public_name = get_public_name p
  ; qualifiers = get_qualifiers p
  ; aliases = get_aliases p
  ; first_names_aliases = get_first_names_aliases p
  ; surnames_aliases = get_surnames_aliases p
  ; titles = get_titles p
  ; rparents = get_rparents p
  ; related = get_related p
  ; occupation = get_occupation p
  ; sex = get_sex p
  ; access = get_access p
  ; birth = get_birth p
  ; birth_place = get_birth_place p
  ; birth_note = get_birth_note p
  ; birth_src = get_birth_src p
  ; baptism = get_baptism p
  ; baptism_place = get_baptism_place p
  ; baptism_note = get_baptism_note p
  ; baptism_src = get_baptism_src p
  ; death = get_death p
  ; death_place = get_death_place p
  ; death_note = get_death_note p
  ; death_src = get_death_src p
  ; burial = get_burial p
  ; burial_place = get_burial_place p
  ; burial_note = get_burial_note p
  ; burial_src = get_burial_src p
  ; pevents = get_pevents p
  ; notes = get_notes p
  ; psources = get_psources p
  ; key_index = get_key_index p
  }

module Collection = struct
  type 'a t = 'a array
  let length = Array.length
  let map = Array.map
  let iter = Array.iter
  let iteri = Array.iteri
  let fold = Array.fold_left
  let fold_until _ _ _ _ = failwith __LOC__
  let iterator c =
    let i = ref 0 in
    fun () ->
      try let r = Array.get c !i in incr i ; Some r
      with _ -> None
end

module Marker = struct
  type ('k, 'v) t = ('v * ('k, 'v) Hashtbl.t)
  let create nb d = (d, Hashtbl.create nb)
  let get (d, m) k = try Hashtbl.find m k with Not_found -> d
  let set (_, m) k v = Hashtbl.replace m k v
end

let of_list fn = function
  | [] -> [||]
  | hd :: tl as l ->
    let init = fn hd in
    let a = Array.make (List.length l) init in
    List.iteri (fun i x -> Array.unsafe_set a (i + 1) (fn x)) tl ;
    a

let ipers (_, get) : iper Collection.t =
  match Yojson.Basic.from_string (get "ipers") with
  | `List l -> (of_list J.to_string l)
  | _ -> print_endline __LOC__ ; [| (* FIXME *) |]

let persons (_, get) =
  match Yojson.Basic.from_string (get "persons") with
  | `List l -> Array.of_list l
  | _ -> print_endline __LOC__ ; [| (* FIXME *) |]

let ifams (_, get) : ifam Collection.t =
  match Yojson.Basic.from_string (get "ifams") with
  | `List l -> (of_list J.to_string l)
  | _ -> print_endline __LOC__ ; [| (* FIXME *) |]

let families (_, get) =
  match Yojson.Basic.from_string (get "families") with
  | `List l -> Array.of_list l
  | _ -> print_endline __LOC__ ; [| (* FIXME *) |]

let ifam_marker ifams init =
  Marker.create (Collection.length ifams) init

let iper_marker ipers init =
  Marker.create (Collection.length ipers) init

let date_of_last_change _base = 0. (* FIXME? *)
let p_surname _base = get_surname
let p_first_name _base = get_first_name

let persons_of_surname _base =
  (* fun (_, get) p n oc ->
   * (\* FIXME *\)
   * match
   *   get (Printf.sprintf "persons?n=%s&p=%s&oc=%d" (Wserver.encode n) (Wserver.encode p) oc)
   *   |> Yojson.Basic.from_string
   * with
   * | `List [] -> None
   * | `List (x :: _) -> Some (get_key_index x)
   * | _ -> assert false *)
failwith __LOC__
let persons_of_first_name _f = failwith __LOC__
let persons_of_name _f = failwith __LOC__

let nobtit _base _ _ p =
  get_list p "titles" title_of_json

let person_misc_names _f = failwith __LOC__
let gen_person_misc_names _f = failwith __LOC__
let base_wiznotes_dir _f = failwith __LOC__
let base_notes_dir _f = failwith __LOC__
let base_notes_origin_file _f = failwith __LOC__
let base_notes_are_empty _base _istr = true (* FIXME *)
let base_notes_read_first_line _f = failwith __LOC__
let base_notes_read _f = failwith __LOC__
let ascends_array _f = failwith __LOC__
let persons_array _base = failwith __LOC__
let base_strings_of_surname _f = failwith __LOC__
let base_strings_of_first_name _f = failwith __LOC__
let base_particles _f = failwith __LOC__
let base_visible_write _f = failwith __LOC__
let base_visible_get _f = failwith __LOC__
let spi_find _f = failwith __LOC__
let spi_next _f = failwith __LOC__
let spi_first _f = failwith __LOC__


let is_deleted_family _f = failwith __LOC__
let delete_family _f = failwith __LOC__
let insert_family _f = failwith __LOC__
let insert_person _f = failwith __LOC__
let patched_ascends _f = failwith __LOC__
let is_patched_person _f = failwith __LOC__
let commit_notes _f = failwith __LOC__
let commit_patches _f = failwith __LOC__
let insert_string _f = failwith __LOC__
let delete_key _f = failwith __LOC__
let patch_key _f = failwith __LOC__
let patch_name _f = failwith __LOC__
let patch_couple _f = failwith __LOC__
let patch_descend _f = failwith __LOC__
let patch_family _f = failwith __LOC__
let patch_union _f = failwith __LOC__
let patch_ascend _f = failwith __LOC__
let patch_person _f = failwith __LOC__

let nb_of_families : base -> int = fun (_, get) ->
  get "nb_family"
  |> Yojson.Basic.from_string
  |> J.to_list
  |> List.hd
  |> J.to_int

let nb_of_persons : base -> int = fun (_, get) ->
  get "nb_persons"
  |> Yojson.Basic.from_string
  |> J.to_list
  |> List.hd
  |> J.to_int

let person_of_key : base -> string -> string -> int -> iper option =
  fun (_, get) p n oc ->
  (* FIXME *)
  match
    get (Printf.sprintf "persons?n=%s&p=%s&oc=%d" (Wserver.encode n) (Wserver.encode p) oc)
    |> Yojson.Basic.from_string
  with
  | `List [] -> None
  | `List (x :: _) ->
    (* print_endline __LOC__;
     * print_endline @@ Yojson.Basic.to_string x;
     * print_endline __LOC__ ; *)
    let x = J.member "person" x in
    (* print_endline @@ Yojson.Basic.to_string x ;
     * print_endline __LOC__ ; *)
    Some (get_key_index x)
  | _ -> assert false

(* FIXME: get ids instead of ints *)
let get_children f =
  get_list f "children" J.to_int
  |> Array.of_list
  |> Array.map (fun i -> "pierfit:" ^ string_of_int i)

let get_parent_array _f = failwith __LOC__

(* FIXME: father/mother should be string basename:xxxx *)
let get_mother f =
  match J.member "parents" f with
  | `List [ _ ; `Int mother ] -> "pierfit:" ^ string_of_int mother
  | _ -> failwith __LOC__

let get_father f =
  match J.member "parents" f with
  | `List [ `Int father ; _ ] -> "pierfit:" ^ string_of_int father
  | _ -> failwith __LOC__

let get_witnesses f : iper array =
  Array.of_list (get_list f "witnesses" J.to_string)

let get_relation f =
  J.member "relation_kind" f
  |> relation_kind_of_json

let get_origin_file f =
  get_string f "origin_file"

let get_marriage_src f =
  get_string f "marriage_src"

let get_marriage_note f =
  get_string f "marriage_note"

let get_marriage_place f =
  get_string f "marriage_place"

let get_marriage f =
  cdate_of_json @@ J.member "marriage" f

let get_fsources f =
  get_string f "fsources"

let get_fevents f =
  get_list f "fevents" fevent_of_json

let get_divorce f =
  divorce_of_json (J.member "divorce" f)

let get_comment f =
  get_string f "comment"

let get_family p =
  match J.member "families" p with
  | `List list -> Array.map J.to_string (Array.of_list list)
  | _ -> [||]

let get_consang _f = Adef.no_consang (* FIXME *)

let get_fam_index f =
  get_string f "fam_index"

let gen_family_of_family : family -> (iper, ifam, istr) Def.gen_family =
  fun f ->
  let open Def in
  { marriage = get_marriage f
  ; marriage_place = get_marriage_place f
  ; marriage_note = get_marriage_note f
  ; marriage_src = get_marriage_src f
  ; witnesses = get_witnesses f
  ; relation = get_relation f
  ; divorce = get_divorce f
  ; fevents = get_fevents f
  ; comment = get_comment f
  ; origin_file = get_origin_file f
  ; fsources = get_fsources f
  ; fam_index = get_fam_index f
  }

let gen_couple_of_couple : family -> iper Def.gen_couple =
  fun f ->
  Adef.couple (get_father f) (get_mother f)

let gen_descend_of_descend : family -> iper Def.gen_descend =
  fun f ->
 { Def.children = get_children f }
