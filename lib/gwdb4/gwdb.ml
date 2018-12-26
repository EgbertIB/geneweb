type iper
type ifam
type istr

module J = Yojson.Basic.Util

let get_string js name =
  match J.member name js with
  | `String s -> s
  | _ -> ""

let get_int js name =
  J.to_int (J.member name js)

let get_list js name fn =
  J.member name js |> J.to_list |> List.map fn

let string_of_iper x = x
let string_of_ifam x = x
let string_of_istr x = x

let iper_of_string x = x
let ifam_of_string x = x
let istr_of_string x = x

type person = Yojson.json
type family = Yojson.json

type relation = Yojson.json
type title = Yojson.json
type pers_event = Yojson.json
type fam_event = Yojson.json

type string_person_index

type base = string

let open_base name = name
let close_base _base = ()

let dummy_iper = ""
let dummy_ifam = ""
let dummy_istr = ""

let eq_istr = (=)
let is_empty_string = (=) ""
let is_quest_string = (=) "?"
let empty_person = `Null
let empty_family = `Null

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

let get_burial _p = Adef.cdate_None
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

let get_key_index p =
  get_int p "index"

let get_notes p =
  get_string p "note"

let get_occ p =
  get_int p "occ"

let get_occupation p =
  get_string p "occupation"

let pevent_name_of_string =
  let open Def in
  function
  | `String "birth" -> Epers_Birth
  | `String "baptism" -> Epers_Baptism
  | `String "death" -> Epers_Death
  | `String "burial" -> Epers_Burial
  | `String "cremation" -> Epers_Cremation
  | `String "accomplishment" -> Epers_Accomplishment
  | `String "aquisition" -> Epers_Acquisition
  | `String "adhesion" -> Epers_Adhesion
  | `String "baptismlds" -> Epers_BaptismLDS
  | `String "barmitzvah" -> Epers_BarMitzvah
  | `String "batmitzvah" -> Epers_BatMitzvah
  | `String "benediction" -> Epers_Benediction
  | `String "changename" -> Epers_ChangeName
  | `String "circumcision" -> Epers_Circumcision
  | `String "confirmation" -> Epers_Confirmation
  | `String "confirmationlds" -> Epers_ConfirmationLDS
  | `String "decoration" -> Epers_Decoration
  | `String "demobilisationmilitaire" -> Epers_DemobilisationMilitaire
  | `String "diploma" -> Epers_Diploma
  | `String "distinction" -> Epers_Distinction
  | `String "dotation" -> Epers_Dotation
  | `String "dotationlds" -> Epers_DotationLDS
  | `String "education" -> Epers_Education
  | `String "election" -> Epers_Election
  | `String "emigration" -> Epers_Emigration
  | `String "excommunication" -> Epers_Excommunication
  | `String "familylinklds" -> Epers_FamilyLinkLDS
  | `String "firstcommunion" -> Epers_FirstCommunion
  | `String "funeral" -> Epers_Funeral
  | `String "graduate" -> Epers_Graduate
  | `String "hospitalisation" -> Epers_Hospitalisation
  | `String "illness" -> Epers_Illness
  | `String "immigration" -> Epers_Immigration
  | `String "listepassenger" -> Epers_ListePassenger
  | `String "militarydistinction" -> Epers_MilitaryDistinction
  | `String "militarypromotion" -> Epers_MilitaryPromotion
  | `String "militaryservice" -> Epers_MilitaryService
  | `String "mobilisationmilitaire" -> Epers_MobilisationMilitaire
  | `String "naturalisation" -> Epers_Naturalisation
  | `String "occupation" -> Epers_Occupation
  | `String "ordination" -> Epers_Ordination
  | `String "property" -> Epers_Property
  | `String "recensement" -> Epers_Recensement
  | `String "residence" -> Epers_Residence
  | `String "retired" -> Epers_Retired
  | `String "scellentchildlds" -> Epers_ScellentChildLDS
  | `String "scellentparentlds" -> Epers_ScellentParentLDS
  | `String "scellentspouselds" -> Epers_ScellentSpouseLDS
  | `String "ventebien" -> Epers_VenteBien
  | `String "will" -> Epers_Will
  | `String s -> Epers_Name s
  | _ -> assert false

let pevent_of_json json =
  { Def.epers_place = get_string json "place"
  ; epers_reason = get_string json "reason"
  ; epers_note = get_string json "note"
  ; epers_src = get_string json "src"
  ; epers_name = pevent_name_of_string (get_string json "name")
  ; epers_date = date_of_json (J.member "member" json)
  ; epers_witnesses = []
  }

let get_pevents p =
  get_list p "pevents" pevent_of_json

let get_psources : person -> istr
let get_public_name : person -> istr
let get_qualifiers : person -> istr list
let get_related : person -> iper list
let get_rparents : person -> relation list
let get_sex : person -> Def.sex
let get_surname : person -> istr
let get_surnames_aliases : person -> istr list
let get_titles : person -> title list *)
