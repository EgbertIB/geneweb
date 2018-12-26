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

let dmy_of_json _json = failwith __LOC__
  (* { Def.day = get_int json "day"
   * ; Def.month = get_int json "month"
   * ; Def.year = get_int json "year"
   * } *)

let cdate_of_json = function
  | `Null -> Adef.cdate_of_od None
  | `String _t -> Adef.cdate_of_od None (* Dtext t *)
  | `Assoc _json ->
    failwith __LOC__
    (* let d1 = dmy_of_json (J.member "dmy1" json) in
     * let calendar = match get_string json "calendar" with
     *   | "gregorian" -> Dgregorian
     *   | "julian" -> Djulian
     *   | "french" -> Dfrench
     *   | "hebrew" -> Dhebrew
     *   | s -> failwith @@ "Unknown calendar \"" ^  s ^ "\""
     * in
     * let prec =  match get_string json "prec" with
     *   | "sure" -> Sure
     *   | "about" -> About
     *   | "maybe" -> Maybe
     *   | "before" -> Before
     *   | "after" -> After
     *   | "or" -> OrYear (dmy_of_json (J.member "dmy2" json))
     *   | "between" -> YearInt (dmy_of_json (J.member "dmy2" json))
     *   | s -> failwith @@ "Unknown prec \"" ^  s ^ "\""
     * in
     * Dgreg ({ prec ; calendar ; }) *)

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

let get_titles _p = [ __LOC__ ]
