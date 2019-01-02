open Adef
open Def

module J = Yojson.Basic.Util

let get_string js name =
  match J.member name js with
  | `String s -> s
  | _ -> ""

let get_int ~__LOC__:_ js name =
  (* print_endline __LOC__ ; *)
  J.to_int (J.member name js)

let get_list name fn js =
  J.member name js |> J.to_list |> List.map fn

(** gwdb to json  *)

let json_of_dmy dmy = `Assoc [
    ("day", `Int dmy.day);
    ("month", `Int dmy.month);
    ("year", `Int dmy.year);
  ]

let dmy_of_json prec json =
  { day = get_int ~__LOC__ json "day"
  ; month = get_int ~__LOC__ json "month"
  ; year = get_int ~__LOC__ json "year"
  ; prec
  }

let json_of_dmy2 dmy = `Assoc [
    ("day", `Int dmy.day2);
    ("month", `Int dmy.month2);
    ("year", `Int dmy.year2);
  ]

let dmy2_of_json json =
  { day2 = get_int ~__LOC__ json "day"
  ; month2 = get_int ~__LOC__ json "month"
  ; year2 = get_int ~__LOC__ json "year"
  }

let json_of_date_cal dt cal =
  let date1 = json_of_dmy dt in
  let prec = match dt.prec with
    | Sure -> "sure"
    | About -> "about"
    | Maybe -> "maybe"
    | Before -> "before"
    | After -> "after"
    | OrYear _ -> "or"
    | YearInt _ -> "between"
  in
  let date2 = match dt.prec with
    | OrYear dmy2 -> json_of_dmy2 dmy2
    | YearInt dmy2 -> json_of_dmy2 dmy2
    | _ -> `Null
  in
  `Assoc [
    ("prec", `String prec);
    ("dmy1", date1);
    ("dmy2", date2);
    ("calendar", `String cal);
  ]

let json_of_date oc =
  match oc with
  | Dgreg (d, Dgregorian) -> json_of_date_cal d "gregorian"
  | Dgreg (d, Djulian) -> json_of_date_cal d "julian"
  | Dgreg (d, Dfrench) -> json_of_date_cal d "french"
  | Dgreg (d, Dhebrew) -> json_of_date_cal d "hebrew"
  | Dtext t -> `String t

let date_of_json = function
  | `String t -> Dtext t
  | json ->
    let prec = match J.to_string @@ J.member "prec" json with
      | "sure" -> Sure
      | "about" -> About
      | "maybe" -> Maybe
      | "before" -> Before
      | "after" -> After
      | "or" -> OrYear (dmy2_of_json @@ J.member "dmy2" json)
      | "between" -> YearInt (dmy2_of_json @@ J.member "dmy2" json)
      | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ x
    in
    let d = dmy_of_json prec (J.member "dmy1" json) in
    match J.to_string @@ J.member "calendar" json with
    | "gregorian" -> Dgreg (d, Dgregorian)
    | "julian" -> Dgreg (d, Djulian)
    | "french" -> Dgreg (d, Dfrench)
    | "hebrew" -> Dgreg (d, Dhebrew)
    | x -> failwith @@ Printf.sprintf "%s: %s" __LOC__ x

let json_of_cdate cd = match Adef.od_of_cdate cd with
  | None -> `Null
  | Some date -> json_of_date date

let cdate_of_json = function
  | `Null -> cdate_of_od None
  | json -> cdate_of_od @@ Some (date_of_json json)

let json_of_pevent_name = function
  | Epers_Birth -> `String "birth"
  | Epers_Baptism -> `String "baptism"
  | Epers_Death -> `String "death"
  | Epers_Burial -> `String "burial"
  | Epers_Cremation -> `String "cremation"
  | Epers_Accomplishment -> `String "accomplishment"
  | Epers_Acquisition -> `String "aquisition"
  | Epers_Adhesion -> `String "adhesion"
  | Epers_BaptismLDS -> `String "baptismlds"
  | Epers_BarMitzvah -> `String "barmitzvah"
  | Epers_BatMitzvah -> `String "batmitzvah"
  | Epers_Benediction -> `String "benediction"
  | Epers_ChangeName -> `String "changename"
  | Epers_Circumcision -> `String "circumcision"
  | Epers_Confirmation -> `String "confirmation"
  | Epers_ConfirmationLDS -> `String "confirmationlds"
  | Epers_Decoration -> `String "decoration"
  | Epers_DemobilisationMilitaire -> `String "demobilisationmilitaire"
  | Epers_Diploma -> `String "diploma"
  | Epers_Distinction -> `String "distinction"
  | Epers_Dotation -> `String "dotation"
  | Epers_DotationLDS -> `String "dotationlds"
  | Epers_Education -> `String "education"
  | Epers_Election -> `String "election"
  | Epers_Emigration -> `String "emigration"
  | Epers_Excommunication -> `String "excommunication"
  | Epers_FamilyLinkLDS -> `String "familylinklds"
  | Epers_FirstCommunion -> `String "firstcommunion"
  | Epers_Funeral -> `String "funeral"
  | Epers_Graduate -> `String "graduate"
  | Epers_Hospitalisation -> `String "hospitalisation"
  | Epers_Illness -> `String "illness"
  | Epers_Immigration -> `String "immigration"
  | Epers_ListePassenger -> `String "listepassenger"
  | Epers_MilitaryDistinction -> `String "militarydistinction"
  | Epers_MilitaryPromotion -> `String "militarypromotion"
  | Epers_MilitaryService -> `String "militaryservice"
  | Epers_MobilisationMilitaire -> `String "mobilisationmilitaire"
  | Epers_Naturalisation -> `String "naturalisation"
  | Epers_Occupation -> `String "occupation"
  | Epers_Ordination -> `String "ordination"
  | Epers_Property -> `String "property"
  | Epers_Recensement -> `String "recensement"
  | Epers_Residence -> `String "residence"
  | Epers_Retired -> `String "retired"
  | Epers_ScellentChildLDS -> `String "scellentchildlds"
  | Epers_ScellentParentLDS -> `String "scellentparentlds"
  | Epers_ScellentSpouseLDS -> `String "scellentspouselds"
  | Epers_VenteBien -> `String "ventebien"
  | Epers_Will -> `String "will"
  | Epers_Name name -> `String name

let pevent_name_of_string = function
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

let json_of_pevent_witness _w = assert false
let pevent_witness_of_json _json = assert false

let json_of_pevent pevent =
  `Assoc [ ("place", `String pevent.epers_place)
         ; ("reason", `String pevent.epers_reason)
         ; ("note", `String pevent.epers_note)
         ; ("src", `String pevent.epers_src)
         ; ("name", json_of_pevent_name pevent.epers_name)
         ; ("date", json_of_cdate pevent.epers_date)
         ; ("witnesses", `List [] (* (Array.to_list @@ Array.map json_of_pevent_witness pevent.epers_witnesses) *) )
         ]
(* FIXME: witnesses *)

let pevent_of_json json =
  { epers_place = get_string json "place"
  ; epers_reason = get_string json "reason"
  ; epers_note = get_string json "note"
  ; epers_src = get_string json "src"
  ; epers_name = pevent_name_of_string (get_string json "name")
  ; epers_date = cdate_of_json (J.member "date" json)
  ; epers_witnesses = [||] (* Array.of_list (get_list json "witnesses" pevent_witness_of_json) *)
  }

let json_of_title_name = function
  | Tmain -> `String ""
  | Tname s -> `String s
  | Tnone -> `Null

let title_name_of_json = function
  | `String "" -> Tmain
  | `String s -> Tname s
  | `Null -> Tnone
  | _ -> failwith __LOC__

let json_of_title gen_title =
  `Assoc [ ("name", json_of_title_name gen_title.t_name)
         ; ("date_start", json_of_cdate gen_title.t_date_start)
         ; ("date_end", json_of_cdate gen_title.t_date_end)
         ; ("nth", `Int gen_title.t_nth)
         ; ("ident", `String gen_title.t_ident)
         ; ("place", `String gen_title.t_place)
         ]

let title_of_json json =
  { t_name = title_name_of_json (J.member "name" json)
  ; t_ident = get_string json "ident"
  ; t_place = get_string json "place"
  ; t_date_start = cdate_of_json (J.member "date_start" json)
  ; t_date_end = cdate_of_json (J.member "date_end" json)
  ; t_nth = get_int ~__LOC__ json "nth" }

let json_of_relation_kind = function
  | Married -> `String "married"
  | NotMarried -> `String "not_married"
  | Engaged -> `String  "engaged"
  | NoSexesCheckNotMarried -> `String "no_sexes_check_not_married"
  | NoMention -> `String "no_mention"
  | NoSexesCheckMarried -> `String "no_sexes_check_married"

let relation_kind_of_json = function
  | `String "married" -> Married
  | `String "not_married" -> NotMarried
  | `String "engaged" -> Engaged
  | `String "no_sexes_check_not_married" -> NoSexesCheckNotMarried
  | `String "no_mention" -> NoMention
  | `String "no_sexes_check_married" -> NoSexesCheckMarried
  | _ -> failwith __LOC__

let json_of_fevent_name = function
  | Efam_Marriage -> `String "marriage"
  | Efam_NoMarriage -> `String "no_marriage"
  | Efam_NoMention -> `String "no_mention"
  | Efam_Engage -> `String "engaged"
  | Efam_Divorce -> `String "divorce"
  | Efam_Separated -> `String "separated"
  | Efam_Annulation -> `String "annulation"
  | Efam_MarriageBann -> `String "marriage_bann"
  | Efam_MarriageContract -> `String "marriage_contract"
  | Efam_MarriageLicense -> `String "marriage_license"
  | Efam_PACS -> `String "pacs"
  | Efam_Residence -> `String "residence"
  | Efam_Name s -> `String s

(* FIXME *)
let fevent_name_of_string = function
  | `String "marriage" -> Efam_Marriage
  | `String "no_marriage" -> Efam_NoMarriage
  | `String "no_mention" -> Efam_NoMention
  | `String "engaged" -> Efam_Engage
  | `String "divorce" -> Efam_Divorce
  | `String "separated" -> Efam_Separated
  | `String "annulation" -> Efam_Annulation
  | `String "marriage_bann" -> Efam_MarriageBann
  | `String "marriage_contract" -> Efam_MarriageContract
  | `String "marriage_license" -> Efam_MarriageLicense
  | `String "pacs" -> Efam_PACS
  | `String "residence" -> Efam_Residence
  | `String s -> Efam_Name s
  | _ -> failwith __LOC__

let json_of_fevent_witness_kind = function
  | Witness -> `String "witness"
  | Witness_GodParent -> `String "godparent"
  (* | Witness_Officer -> `String "officer" *)


let fevent_witness_kind_of_json = function
  | `String "witness" -> Witness
  | `String "godparent" -> Witness_GodParent
  | _ -> failwith __LOC__
  (* | `String "officer" -> Witness_Officer *)

let json_of_fevent_witness (person , witness_kind) =
  `Assoc [ ("person", `String person)
         ; ("type", json_of_fevent_witness_kind witness_kind)
         ]

let fevent_witness_of_json json =
  ( get_string json "person"
  , fevent_witness_kind_of_json (J.member "type" json) )

let json_of_fevent fevent =
  `Assoc [ ("place", `String fevent.efam_place)
         ; ("reason", `String fevent.efam_reason)
         ; ("note", `String fevent.efam_note)
         ; ("src", `String fevent.efam_src)
         ; ("name", json_of_fevent_name fevent.efam_name)
         ; ("date", json_of_cdate fevent.efam_date)
         ; ("witnesses", `List (Array.to_list @@ Array.map json_of_fevent_witness fevent.efam_witnesses) )
         ]

let fevent_of_json json =
  { efam_place = get_string json "place"
  ; efam_reason = get_string json "reason"
  ; efam_note = get_string json "note"
  ; efam_src = get_string json "src"
  ; efam_name = fevent_name_of_string (J.member "name" json)
  ; efam_date = cdate_of_json (J.member "date" json)
  ; efam_witnesses = Array.of_list (get_list "witnesses" fevent_witness_of_json json)
  }

let json_of_divorce = function
  | NotDivorced -> `Null
  | Divorced date -> json_of_cdate date
  | Separated -> `Bool true

let divorce_of_json = function
  | `Null -> NotDivorced
  | `Bool true -> Separated
  | date -> Divorced (cdate_of_json date)

let json_of_relation_type = function
  | Adoption -> `String "adoption"
  | Recognition -> `String "recognition"
  | CandidateParent -> `String "candidate_parent"
  | GodParent -> `String "god_parent"
  | FosterParent -> `String "foster_parent"

let relation_type_of_json = function
  | `String "adoption" -> Adoption
  | `String "recognition" -> Recognition
  | `String "candidate_parent" -> CandidateParent
  | `String "god_parent" -> GodParent
  | `String "foster_parent" -> FosterParent
  | _ -> failwith __LOC__

let json_of_rparent gen_relation =
  `Assoc [ ("type", json_of_relation_type gen_relation.r_type )
         ; ("source", `String gen_relation.r_sources)
         ; ("father", match gen_relation.r_fath with Some i -> `String i | _ -> `Null)
         ; ("mother", match gen_relation.r_moth with Some i -> `String i | _ -> `Null)
         ]

let rparent_of_json json =
  { r_type = relation_type_of_json (J.member "type" json)
  ; r_fath = (match (J.member "father" json) with `String i -> Some i | _ -> None)
  ; r_moth = (match (J.member "mother" json) with `String i -> Some i | _ -> None)
  ; r_sources = get_string json "source"
  }
