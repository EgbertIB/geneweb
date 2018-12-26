type iper = string
type ifam = string
type istr = string

module J = Yojson.Basic.Util

let get_string js name =
  match J.member name js with
  | `String s -> s
  | _ -> ""

let get_int js name =
  J.to_int (J.member name js)

let get_list js name fn =
  J.member name js |> J.to_list |> List.map fn

let string_of_iper : iper -> string = fun x -> x
let string_of_ifam : ifam -> string = fun x -> x
let string_of_istr : istr -> string = fun x -> x

let iper_of_string x = x
let ifam_of_string x = x
let istr_of_string x = x

type person = Yojson.Basic.json
type family = Yojson.Basic.json

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

type string_person_index

type base = (string * (string -> string))

let open_base name =
  assert (name = "pierfit") ;
  ( name
  , fun request ->
    let url = Printf.sprintf "http://virt6:8529/_db/Trees/geneweb/%s/%s" name request in
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
        Curl.set_timeoutms connection 1000;
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
  | `Int 2 -> Def.Private
  | `Int 1 -> Def.Public
  | `Int 0 -> Def.IfTitles
  | _ -> assert false

let get_aliases p =
  match J.member "access" p with
  | `List l -> List.map J.to_string l
  | `Null -> []
  | _ -> assert false

let get_baptism _p = Adef.cdate_None
let get_baptism_place _p = ""
let get_baptism_note _p = ""
let get_baptism_src _p = ""

let get_birth _p = Adef.cdate_None
let get_birth_place _p = ""
let get_birth_note _p = ""
let get_birth_src _p = ""

let get_burial _p = assert false
let get_burial_place _p = ""
let get_burial_note _p = ""
let get_burial_src _p = ""

let get_death _p = Def.NotDead
let get_death_place _p = ""
let get_death_note _p = ""
let get_death_src _p = ""

let get_first_name p =
  match J.member "firstname" p with
  | `String s -> s
  | _ -> ""

let get_first_names_aliases p =
  get_list p "first_names_aliases" J.to_string

let get_image p =
  get_string p "image"

let get_key_index : person -> iper = fun p ->
  get_string p "index"

let get_notes p =
  get_string p "note"

let get_occ p =
  get_int p "occ"

let get_occupation p =
  get_string p "occupation"

let pevent_name_of_string =
  let open Def in
  function
  | "birth" -> Epers_Birth
  | "baptism" -> Epers_Baptism
  | "death" -> Epers_Death
  | "burial" -> Epers_Burial
  | "cremation" -> Epers_Cremation
  | "accomplishment" -> Epers_Accomplishment
  | "aquisition" -> Epers_Acquisition
  | "adhesion" -> Epers_Adhesion
  | "baptismlds" -> Epers_BaptismLDS
  | "barmitzvah" -> Epers_BarMitzvah
  | "batmitzvah" -> Epers_BatMitzvah
  | "benediction" -> Epers_Benediction
  | "changename" -> Epers_ChangeName
  | "circumcision" -> Epers_Circumcision
  | "confirmation" -> Epers_Confirmation
  | "confirmationlds" -> Epers_ConfirmationLDS
  | "decoration" -> Epers_Decoration
  | "demobilisationmilitaire" -> Epers_DemobilisationMilitaire
  | "diploma" -> Epers_Diploma
  | "distinction" -> Epers_Distinction
  | "dotation" -> Epers_Dotation
  | "dotationlds" -> Epers_DotationLDS
  | "education" -> Epers_Education
  | "election" -> Epers_Election
  | "emigration" -> Epers_Emigration
  | "excommunication" -> Epers_Excommunication
  | "familylinklds" -> Epers_FamilyLinkLDS
  | "firstcommunion" -> Epers_FirstCommunion
  | "funeral" -> Epers_Funeral
  | "graduate" -> Epers_Graduate
  | "hospitalisation" -> Epers_Hospitalisation
  | "illness" -> Epers_Illness
  | "immigration" -> Epers_Immigration
  | "listepassenger" -> Epers_ListePassenger
  | "militarydistinction" -> Epers_MilitaryDistinction
  | "militarypromotion" -> Epers_MilitaryPromotion
  | "militaryservice" -> Epers_MilitaryService
  | "mobilisationmilitaire" -> Epers_MobilisationMilitaire
  | "naturalisation" -> Epers_Naturalisation
  | "occupation" -> Epers_Occupation
  | "ordination" -> Epers_Ordination
  | "property" -> Epers_Property
  | "recensement" -> Epers_Recensement
  | "residence" -> Epers_Residence
  | "retired" -> Epers_Retired
  | "scellentchildlds" -> Epers_ScellentChildLDS
  | "scellentparentlds" -> Epers_ScellentParentLDS
  | "scellentspouselds" -> Epers_ScellentSpouseLDS
  | "ventebien" -> Epers_VenteBien
  | "will" -> Epers_Will
  | s -> Epers_Name s

(* let dmy_of_json _json =
 *   { Def.day = get_int json "day"
 *   ; Def.month = get_int json "month"
 *   ; Def.year = get_int json "year"
 *   } *)

(* let cdate_of_json = function
 *   | `Null -> Adef.cdate_of_od None
 *   | `String _t -> Adef.cdate_of_od None (\* Dtext t *\)
 *   | `Assoc _json ->
 *     let d1 = dmy_of_json (J.member "dmy1" json) in
 *     let calendar = match get_string json "calendar" with
 *       | "gregorian" -> Dgregorian
 *       | "julian" -> Djulian
 *       | "french" -> Dfrench
 *       | "hebrew" -> Dhebrew
 *       | s -> failwith @@ "Unknown calendar \"" ^  s ^ "\""
 *     in
 *     let prec =  match get_string json "prec" with
 *       | "sure" -> Sure
 *       | "about" -> About
 *       | "maybe" -> Maybe
 *       | "before" -> Before
 *       | "after" -> After
 *       | "or" -> OrYear (dmy_of_json (J.member "dmy2" json))
 *       | "between" -> YearInt (dmy_of_json (J.member "dmy2" json))
 *       | s -> failwith @@ "Unknown prec \"" ^  s ^ "\""
 *     in
 *     Dgreg ({ prec ; calendar ; }) *)

let pevent_of_json json =
  { Def.epers_place = get_string json "place"
  ; epers_reason = get_string json "reason"
  ; epers_note = get_string json "note"
  ; epers_src = get_string json "src"
  ; epers_name = pevent_name_of_string (get_string json "name")
  ; epers_date = Adef.cdate_of_od None(*** date_of_json (J.member "member" json) ***)
  ; epers_witnesses = [||]
  }

let get_pevents p =
  get_list p "pevents" pevent_of_json

let get_psources _p = __LOC__

let get_public_name _p = __LOC__

let get_qualifiers _p = [ __LOC__ ]

let get_related _p = []

let get_rparents _p = []

let get_sex p = match get_int p "sex" with
  | 1 -> Def.Male
  | 2 -> Def.Female
  | _ -> Def.Neuter

let get_surname _p = __LOC__

let get_surnames_aliases _p = [ __LOC__ ]

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

let foi (_, get) ifam =
  get @@ "family/" ^ ifam
  |> Yojson.Basic.from_string

let poi (_, get) iper =
  get @@ "person/" ^ iper
  |> Yojson.Basic.from_string

(* FIXME https://github.com/geneweb/geneweb/pull/726/ *)
let json_of_cdate _ = assert false
let json_of_relation_kind _ = assert false
let json_of_divorce _ = assert false
let json_of_fevent _ = assert false
let json_of_death _ = assert false
let json_of_burial _ = assert false

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
  `Assoc [ ("first_name", `String p.first_name)
         ; ("surname", `String p.surname)
         ; ("occ", `Int p.occ)
         ; ("image", `String p.image)
         ; ("public_name", `String p.public_name)
         ; ("qualifiers", `List (List.map (fun x -> `String x) p.qualifiers) )
         ; ("aliases", `List (List.map (fun x -> `String x) p.aliases) )
         ; ("first_names_aliases", `List (List.map (fun x -> `String x) p.first_names_aliases) )
         ; ("surnames_aliases", `List (List.map (fun x -> `String x) p.surnames_aliases) )
         ; ("titles", `List [])   (* FIXME *)
         ; ("rparents", `List [])
         ; ("related", `List [])
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
         ; ("pevents", `List []) (* FIXME *)
         ; ("notes", `String p.notes)
         ; ("psources", `String p.psources)
         ; ("key_index", `String p.key_index)
         ]

let gen_person_of_person = assert false
let gen_descend_of_descend = assert false
let gen_couple_of_couple = assert false
let gen_family_of_family = assert false

module Collection = struct
  type 'a t = 'a array
  let length = Array.length
  let map = Array.map
  let iter = Array.iter
  let iteri = Array.iteri
  let fold = Array.fold_left
  let fold_until _ _ _ _ = assert false
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
  | _ -> assert false

let persons (_, get) =
  match Yojson.Basic.from_string (get "persons") with
  | `List l -> Array.of_list l
  | _ -> assert false

let ifams (_, get) : ifam Collection.t =
  match Yojson.Basic.from_string (get "ifams") with
  | `List l -> (of_list J.to_string l)
  | _ -> assert false

let families (_, get) =
  match Yojson.Basic.from_string (get "families") with
  | `List l -> Array.of_list l
  | _ -> assert false

let ifam_marker ifams init =
  Marker.create (Collection.length ifams) init

let iper_marker ipers init =
  Marker.create (Collection.length ipers) init

let date_of_last_change = assert false
let p_surname = assert false
let p_first_name = assert false
let nobtit = assert false
let person_misc_names = assert false
let gen_person_misc_names = assert false
let base_wiznotes_dir = assert false
let base_notes_dir = assert false
let base_notes_origin_file = assert false
let base_notes_are_empty = assert false
let base_notes_read_first_line = assert false
let base_notes_read = assert false
let ascends_array = assert false
let persons_array = assert false
let base_strings_of_surname = assert false
let base_strings_of_first_name = assert false
let base_particles = assert false
let base_visible_write = assert false
let base_visible_get = assert false
let spi_find = assert false
let spi_next = assert false
let spi_first = assert false
let persons_of_surname = assert false
let persons_of_first_name = assert false
let persons_of_name = assert false
let person_of_key = assert false
let is_deleted_family = assert false
let delete_family = assert false
let insert_family = assert false
let insert_person = assert false
let patched_ascends = assert false
let is_patched_person = assert false
let commit_notes = assert false
let commit_patches = assert false
let insert_string = assert false
let delete_key = assert false
let patch_key = assert false
let patch_name = assert false
let patch_couple = assert false
let patch_descend = assert false
let patch_family = assert false
let patch_union = assert false
let patch_ascend = assert false
let patch_person = assert false
let nb_of_families = assert false
let nb_of_persons = assert false
let get_children = assert false
let get_parent_array = assert false
let get_mother = assert false
let get_father = assert false
let get_witnesses = assert false
let get_relation = assert false
let get_origin_file = assert false
let get_marriage_src = assert false
let get_marriage_note = assert false
let get_marriage_place = assert false
let get_marriage = assert false
let get_fsources = assert false
let get_fevents = assert false
let get_divorce = assert false
let get_comment = assert false
let get_family = assert false
let get_consang = assert false
let get_parents = assert false
