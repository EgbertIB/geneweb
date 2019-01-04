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

type person = (iper * Yojson.Basic.json)
type family = Yojson.Basic.json

type relation = (iper, istr) gen_relation
type title = istr gen_title
type pers_event = (iper, istr) gen_pers_event
type fam_event = (iper, istr) gen_fam_event

type base = (string * (__LOC__:string -> string -> string))

let iper_of_int i =
  Printf.sprintf "pierfit:%d" i

let open_base name =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let result = Buffer.create 16384 in
  let write data =
    Buffer.add_string result data ;
    String.length data
  in
  (* print_endline __LOC__ ; *)
  ( name
  , fun ~__LOC__:_ request ->
    try
      (* print_endline __LOC__ ; *)
      let url = Printf.sprintf "http://localhost:8529/_db/Trees/geneweb/%s/%s" name request in
      print_endline @@ Printf.sprintf "%s: %s" __LOC__ url ;
      Buffer.reset result ;
      let connection = Curl.init () in
      Curl.set_httpheader connection [];
      Curl.set_writefunction connection write ;
      Curl.set_followlocation connection true ;
      Curl.set_timeoutms connection 10000;
      Curl.set_url connection url ;
      Curl.perform connection ;
      Curl.cleanup connection ;
      Buffer.contents result
    with
    | Curl.CurlException (_curlCode, i, s) as e ->
      failwith @@ (Printf.sprintf "%s %d %s %s" __LOC__ i s (Printexc.to_string e))
  )

let close_base _base = ()

let dummy_iper : iper = ""
let dummy_ifam : ifam = ""
let dummy_istr : istr = ""

let eq_istr = (=)
let is_empty_string = (=) ""
let is_quest_string = (=) "?"
let empty_person _ _ = ("", `Null)
let empty_family _ _ = `Null

let get_access (_key, p) =
  match J.member "access" p with
  | `Int 2 -> Private
  | `Int 1 -> Public
  | `Int 0 -> IfTitles
  | _ -> failwith __LOC__

let get_aliases (_key, p) =
  match J.member "aliases" p with
  | `List l -> List.map J.to_string l
  | `Null -> []
  | _ -> failwith __LOC__

let get_pevents (_key, p) =
  get_list "pevents" pevent_of_json p

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
  function None -> NotDead
         | Some e ->
           if Adef.od_of_cdate e.epers_date <> None
           then Death (Unspecified, e.epers_date)
           else DeadDontKnowWhen

let get_first_name (_key, p) =
  match J.member "firstname" p with
  | `String s -> s
  | _ -> ""

let get_first_names_aliases (_key, p) =
  get_list "first_names_aliases" J.to_string p

let get_image (_key, p) =
  get_string ~__LOC__ p "image"

let get_key_index : person -> iper = fun (key, _p) -> key
  (* get_string ~__LOC__ p "index" *)

let get_notes (_key, p) =
  get_string ~__LOC__ p "note"

let get_occ (_key, p) =
  get_int ~__LOC__ p "occ"

let get_occupation (_key, p) =
  get_string ~__LOC__ p "occupation"

let get_psources (_key, p) =
  get_string ~__LOC__ p "psources"

let get_public_name (_key, p) =
  get_string ~__LOC__ p "public_name"

let get_qualifiers (_key, p) =
  get_list "qualifiers" J.to_string p

let get_related (_key, p) =
  get_list "related" (* J.to_string *) J.to_int p
  |> List.map iper_of_int

let get_rparents (_key, p) =
  get_list "rparents" rparent_of_json p

let get_parents (_key, p) =
  match J.member "parents" p with
  | `String i -> Some i
  | _ -> None (* FIXME *)

let get_sex (_key, p) = match get_int ~__LOC__ p "sex" with
  | 1 -> Def.Male
  | 2 -> Def.Female
  | _ -> Def.Neuter

let get_surname (_key, p) =
  match J.member "lastname" p with
  | `String s -> s
  | _ -> ""

let get_surnames_aliases (_key, p) =
  get_list "surnames_aliases" J.to_string p

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
  if ifam = dummy_ifam then raise Not_found ;
  try Hashtbl.find foi_cache ifam
  with Not_found ->
    let x =
      get ~__LOC__ @@ "families/" ^ (String.split_on_char ':' ifam |> String.concat "%3A")
      |> Yojson.Basic.from_string
      |> J.member "family"
    in
    Hashtbl.add foi_cache ifam x ;
    x

let poi_cache = Hashtbl.create 42

(* FIXME *)
let poi (_, get) iper =
  if iper = dummy_iper then raise Not_found ;
  try Hashtbl.find poi_cache iper
  with Not_found ->
    let x =
      match Yojson.Basic.from_string @@
        get ~__LOC__ @@ "persons/" ^ (String.split_on_char ':' iper |> String.concat "%3A")
      with
      | `List (hd :: _) -> (* print_endline __LOC__ ;  *)
        ( J.to_string (J.member "_key" hd)
        , J.member "person" hd )
      | x -> (* print_endline __LOC__ ;  *)
        ( J.to_string (J.member "_key" x)
        , J.member "person" x )
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

let person_of_gen_person _base (p, _a, _u) =
  let open Def in
  ( p.key_index
  , `Assoc [ ("first_name", `String p.first_name)
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
           ; ("pevents", `List (List.map json_of_pevent p.pevents))
           ; ("notes", `String p.notes)
           ; ("psources", `String p.psources)
           ]
  )

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

let date_of_last_change _base = 0. (* FIXME? *)
let p_surname _base = get_surname
let p_first_name _base = get_first_name

type string_person_index =
  { find : istr -> iper list
  ; cursor : string -> istr
  ; next : istr -> istr
  }

let mk_spi fn ((_, get) : base) =
  { find = begin fun istr ->
        match get ~__LOC__ (fn istr) |> Yojson.Basic.from_string
        with
        | `List l -> List.map (fun x -> J.to_string @@ J.member "_key" x) l
        | _ -> []
      end
  ; cursor = begin fun _ -> assert false end
  ; next = begin fun _ -> assert false end
  }


let spi_find spi = spi.find
let spi_first spi = spi.cursor
let spi_next (spi : string_person_index) istr (_need_whole_list : bool) = spi.next istr, 1

let persons_of_surname =
  mk_spi (fun istr -> Printf.sprintf "persons?lastname=%s&start_with=true" (Wserver.encode istr))

let persons_of_first_name  =
  mk_spi (fun istr -> Printf.sprintf "persons?firstname=%s&start_with=true" (Wserver.encode istr))

(* FIXME! *)
let persons_of_name _base _istr = []

let base_strings_of_surname _base _istr = [] (* FIXME *)

let base_strings_of_first_name _base _istr = [] (* FIXME *)

let nobtit _base _ _ (_key, p) =
  get_list "titles" title_of_json p

let person_misc_names _f = failwith __LOC__
let gen_person_misc_names _f = failwith __LOC__

(* Copied from gwdb1/database.ml *)
let read_notes bname fnotes rn_mode =
  let bname =
    if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
  in
  let fname =
    if fnotes = "" then "notes"
    else Filename.concat "notes_d" (fnotes ^ ".txt")
  in
  try
    let ic = Secure.open_in (Filename.concat bname fname) in
    let str =
      match rn_mode with
        RnDeg -> if in_channel_length ic = 0 then "" else " "
      | Rn1Ln -> (try input_line ic with End_of_file -> "")
      | RnAll ->
        let rec loop len =
          match input_char ic with
          | exception End_of_file -> Buff.get len
          | c -> loop (Buff.store len c)
        in
        loop 0
    in
    close_in ic ;
    str
  with Sys_error _ -> ""


let base_wiznotes_dir _base =
  "notes_d"

let base_notes_dir _base =
  "wiznotes"

(* FIXME *)
let base_notes_origin_file _base =
  let () = print_endline __LOC__ in ""

let base_notes_are_empty (bname, _get) fnotes =
  read_notes bname fnotes RnDeg = ""

let base_notes_read_first_line (bname, _get) fnotes =
  read_notes bname fnotes Rn1Ln

let base_notes_read (bname, _get) fnotes =
  read_notes bname fnotes RnAll

let ascends_array _f = let () = print_endline __LOC__ in failwith __LOC__
let persons_array _base = let () = print_endline __LOC__ in failwith __LOC__
let base_particles (bname, _get) =
  (* FIXME: memoize *)
  let () = print_endline __LOC__ in
  Mutil.input_particles (Filename.concat bname "particles.txt")

let base_visible_write _ = (* failwith __LOC__ *) let () = print_endline __LOC__ in ()
let base_visible_get _ _ _ = (* failwith __LOC__ *) let () = print_endline __LOC__ in true

let delete_family _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_family _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_person _f = let () = print_endline __LOC__ in failwith __LOC__
let patched_ascends _f = let () = print_endline __LOC__ in failwith __LOC__
let is_patched_person _f = let () = print_endline __LOC__ in failwith __LOC__
let commit_notes _f = let () = print_endline __LOC__ in failwith __LOC__
let commit_patches _f = let () = print_endline __LOC__ in failwith __LOC__
let insert_string _base s = s
let delete_key _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_key _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_name _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_couple _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_descend _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_family _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_union _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_ascend _f = let () = print_endline __LOC__ in failwith __LOC__
let patch_person _f = let () = print_endline __LOC__ in failwith __LOC__

let nb_of_families : base -> int = fun (_, get) ->
  get ~__LOC__ "nb_families"
  |> Yojson.Basic.from_string
  |> J.to_list
  |> List.hd
  |> J.to_int

let nb_of_persons : base -> int = fun (_, get) ->
  get ~__LOC__ "nb_persons"
  |> Yojson.Basic.from_string
  |> J.to_list
  |> List.hd
  |> J.to_int

let person_of_key : base -> string -> string -> int -> iper option =
  fun (_, get) p n oc ->
  (* FIXME *)
  match
    get ~__LOC__ (Printf.sprintf "persons?n=%s&p=%s&occ=%d" (Wserver.encode n) (Wserver.encode p) oc)
    |> Yojson.Basic.from_string
  with
  | `List [] -> None
  | `List (x :: _) -> Some (J.to_string @@ J.member "_key" x)
  | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (Yojson.Basic.to_string x)

let get_children f =
  get_list "children" J.to_int (* J.to_string *) f
  |> List.map iper_of_int
  |> Array.of_list

let get_parent_array f =
  match J.member "parent_array" f with
  | `List [ `Int father ; `Int mother ] -> [| iper_of_int father ; iper_of_int mother |] (* FIXME: To be removed *)
  | `List [ `String father ; `String mother ] -> [| father ; mother |] (* FIXME: To be removed *)
  | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (Yojson.Basic.to_string x)

let get_mother f =
  match J.member "parents" f with
  | `List [ _ ; `String mother ] -> mother
  | `List [ _ ; `Int mother ] -> iper_of_int mother (* FIXME: To be removed *)
  | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (Yojson.Basic.to_string x)

let get_father f =
  match J.member "parents" f with
  | `List [ `String father ; _ ] -> father
  | `List [ `Int father ; _ ] -> iper_of_int father (* FIXME: To be removed *)
  | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ (Yojson.Basic.to_string x)

let get_witnesses f : iper array =
  Array.of_list (get_list "witnesses" J.to_string f)

let get_relation f =
  J.member "relation_kind" f
  |> relation_kind_of_json

let get_origin_file f =
  get_string ~__LOC__ f "origin_file"

let get_marriage_src f =
  get_string ~__LOC__ f "marriage_src"

let get_marriage_note f =
  get_string ~__LOC__ f "marriage_note"

let get_marriage_place f =
  get_string ~__LOC__ f "marriage_place"

let get_marriage f =
  cdate_of_json @@ J.member "marriage" f

let get_fsources f =
  get_string ~__LOC__ f "fsources"

let get_fevents f =
  get_list "fevents" fevent_of_json f

let get_divorce f =
  divorce_of_json (J.member "divorce" f)

let get_comment f =
  get_string ~__LOC__ f "comment"

let get_family (_key, p) =
  match J.member "families" p with
  | `List list -> Array.map J.to_string (Array.of_list list)
  | _ -> [||]

let get_consang _f = Adef.no_consang (* FIXME *)

let get_fam_index f =
  get_string ~__LOC__ f "fam_index"

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


module Collection = struct

  type 'a t =
    { length : int
    ; get : int -> 'a
    }

  let length x =
    x.length

  let map fn x =
    { x with get = fun i -> fn (x.get i) }

  let iter fn x =
    for i = 0 to length x - 1 do fn (x.get i) done

  let iteri fn x =
    for i = 0 to length x - 1 do fn i (x.get i) done

  let fold fn acc { get ; length } =
    let rec loop acc i =
      if i = length then acc
      else loop (fn acc (get i)) (i + 1)
    in
    loop acc 0

  let fold_until continue fn acc { get ; length } =
    let rec loop acc i =
      if not (continue acc) || i = length then acc
      else loop (fn acc (get i)) (i + 1)
    in
    loop acc 0

  let iterator { get ; length } =
    let cursor = ref 0 in
    fun () ->
      if !cursor < length then
        let v = get !cursor in
        incr cursor ;
        Some v
      else None

end

(* TODO: do not keep full array, only current bulk *)
let mk_collection len init get bulk_size =
  let current = ref (-1) in
  let cache = Array.make len init in
  let fetch i =
    let offset = (i / bulk_size) * bulk_size in
    let list = get offset bulk_size in
    List.iteri (fun i v -> Array.set cache (i + offset) v) list ;
    current := offset + bulk_size
  in
  let get i =
    if i < !current then Array.get cache i
    else begin fetch i ; Array.get cache i end
  in
  Collection.{ length = len ; get }

module Marker = struct
  type ('k, 'v) t = ('v * ('k, 'v) Hashtbl.t)
  let create nb d = (d, Hashtbl.create nb)
  let get (d, m) k = try Hashtbl.find m k with Not_found -> d
  let set (_, m) k v = Hashtbl.replace m k v
end

(* let of_list fn = function
 *   | [] -> [||]
 *   | hd :: tl as l ->
 *     let init = fn hd in
 *     let a = Array.make (List.length l) init in
 *     List.iteri (fun i x -> Array.unsafe_set a (i + 1) (fn x)) tl ;
 *     a *)

(* FIXME *)
let ipers ((_, get) as base) : iper Collection.t =
  mk_collection (nb_of_persons base) dummy_iper
    (fun offset limit ->
       Printf.sprintf "persons?scope=ids&offset=%d&limit=%d" offset limit
       |> get ~__LOC__
       |> Yojson.Basic.from_string
       |> function
       | `List list -> List.map J.to_string list
       | _ -> failwith __LOC__
    )
    100000

let persons ((_, get) as base : base) : person Collection.t =
  mk_collection (nb_of_persons base) ("", `Null)
    (fun offset limit ->
       Printf.sprintf "persons?offset=%d&limit=%d" offset limit
       |> get ~__LOC__
       |> Yojson.Basic.from_string
       |> function
       | `List list ->
         List.map
           (fun x -> ( J.to_string (J.member "_key" x), J.member "person" x) )
           list
       | _ -> failwith __LOC__
    )
    10000

let ifams ((_, get) as base) : ifam Collection.t =
  mk_collection (nb_of_families base) dummy_ifam
    (fun offset limit ->
       Printf.sprintf "families?scope=ids&offset=%d&limit=%d" offset limit
       |> get ~__LOC__
       |> Yojson.Basic.from_string
       |> function
       | `List list -> List.map J.to_string list
       | _ -> failwith __LOC__
    )
    100000

let families ((_, get) as base) : family Collection.t =
  mk_collection (nb_of_families base) `Null
    (fun offset limit ->
       Printf.sprintf "families?offset=%d&limit=%d" offset limit
       |> get ~__LOC__
       |> Yojson.Basic.from_string
       |> function
       | `List list -> List.map (J.member "family") list
       | _ -> failwith __LOC__
    )
    10000

let ifam_marker ifams init =
  Marker.create (Collection.length ifams) init

let iper_marker ipers init =
  Marker.create (Collection.length ipers) init
