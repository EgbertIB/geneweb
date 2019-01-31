#ifdef API

module M = Api_piqi
module Mext = Api_piqi_ext

module Mapp = Api_app_piqi
module Mext_app = Api_app_piqi_ext

(* Dans un premier temps, ce module dupliques certaines  *)
(* fonctions déjà présentes, mais c'est pour qu'il reste *)
(* le plus indépendant possible des autres modules.      *)

open Config
open Def
open Gwdb
open Util
open Api_def

let to_piqi_pevent_aux = function
  | Epers_Birth -> `epers_birth
  | Epers_Baptism -> `epers_baptism
  | Epers_Death -> `epers_death
  | Epers_Burial -> `epers_burial
  | Epers_Cremation -> `epers_cremation
  | Epers_Accomplishment -> `epers_accomplishment
  | Epers_Acquisition -> `epers_acquisition
  | Epers_Adhesion -> `epers_adhesion
  | Epers_BaptismLDS -> `epers_baptismlds
  | Epers_BarMitzvah -> `epers_barmitzvah
  | Epers_BatMitzvah -> `epers_batmitzvah
  | Epers_Benediction -> `epers_benediction
  | Epers_ChangeName -> `epers_changename
  | Epers_Circumcision-> `epers_circumcision
  | Epers_Confirmation -> `epers_confirmation
  | Epers_ConfirmationLDS -> `epers_confirmationlds
  | Epers_Decoration -> `epers_decoration
  | Epers_DemobilisationMilitaire -> `epers_demobilisationmilitaire
  | Epers_Diploma -> `epers_diploma
  | Epers_Distinction -> `epers_distinction
  | Epers_Dotation -> `epers_dotation
  | Epers_DotationLDS -> `epers_dotationlds
  | Epers_Education -> `epers_education
  | Epers_Election -> `epers_election
  | Epers_Emigration -> `epers_emigration
  | Epers_Excommunication -> `epers_excommunication
  | Epers_FamilyLinkLDS -> `epers_familylinklds
  | Epers_FirstCommunion -> `epers_firstcommunion
  | Epers_Funeral -> `epers_funeral
  | Epers_Graduate -> `epers_graduate
  | Epers_Hospitalisation -> `epers_hospitalisation
  | Epers_Illness -> `epers_illness
  | Epers_Immigration-> `epers_immigration
  | Epers_ListePassenger -> `epers_listepassenger
  | Epers_MilitaryDistinction -> `epers_militarydistinction
  | Epers_MilitaryPromotion -> `epers_militarypromotion
  | Epers_MilitaryService -> `epers_militaryservice
  | Epers_MobilisationMilitaire -> `epers_mobilisationmilitaire
  | Epers_Naturalisation -> `epers_naturalisation
  | Epers_Occupation -> `epers_occupation
  | Epers_Ordination -> `epers_ordination
  | Epers_Property -> `epers_property
  | Epers_Recensement -> `epers_recensement
  | Epers_Residence -> `epers_residence
  | Epers_Retired -> `epers_retired
  | Epers_ScellentChildLDS -> `epers_scellentchildlds
  | Epers_ScellentParentLDS -> `epers_scellentparentlds
  | Epers_ScellentSpouseLDS -> `epers_scellentspouselds
  | Epers_VenteBien -> `epers_ventebien
  | Epers_Will -> `epers_will
  | Epers_Name _ -> assert false

let to_piqi_pevent = function
  | Epers_Name _ -> `epers_custom
  | x -> to_piqi_pevent_aux x

let to_piqi_fevent_aux = function
  | Efam_Marriage -> `efam_marriage
  | Efam_NoMarriage -> `efam_no_marriage
  | Efam_NoMention -> `efam_no_mention
  | Efam_Engage -> `efam_engage
  | Efam_Divorce -> `efam_divorce
  | Efam_Separated -> `efam_separated
  | Efam_Annulation -> `efam_annulation
  | Efam_MarriageBann -> `efam_marriage_bann
  | Efam_MarriageContract -> `efam_marriage_contract
  | Efam_MarriageLicense -> `efam_marriage_license
  | Efam_PACS -> `efam_pacs
  | Efam_Residence -> `efam_residence
  | Efam_Name _ -> assert false

let to_piqi_fevent = function
  | Efam_Name _ -> `efam_custom
  | x -> to_piqi_fevent_aux x

let to_piqi_sex p =
  match get_sex p with
  | Male -> `male
  | Female -> `female
  | Neuter -> `unknown

let of_piqi_sex = function
  | `male -> Male
  | `female -> Female
  | `unknown -> Neuter

let to_piqi_access p =
  match get_access p with
  | IfTitles -> `access_iftitles
  | Public -> `access_public
  | Private -> `access_private

let to_piqi_sosa_aux sosa_nb =
  if Sosa.eq sosa_nb Sosa.zero then `no_sosa
  else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
  else `sosa

let to_piqi_sosa p =
  to_piqi_sosa_aux (Perso.get_sosa_person p)

let to_piqi_sn = Name.lower

let to_piqi_fn = Name.lower

let to_piqi_occ p = Int32.of_int (get_occ p)

let to_piqi_occ_opt p = match get_occ p with 0 -> None | x -> Some (Int32.of_int x)

let of_piqi_occ_opt = Opt.map_default 0 Int32.to_int

let to_piqi_surname base p = sou base (get_surname p)

let to_piqi_firstname base p = sou base (get_first_name p)

let to_piqi_string_aux base s =
  if is_empty_string s
  then ""
  else sou base s

let to_piqi_string_opt_aux base s =
  if is_empty_string s
  then None
  else Some (sou base s)

let to_piqi_publicname base p =
  to_piqi_string_opt_aux base (get_public_name p)

let to_piqi_aliases base p =
  List.map (sou base) (get_aliases p)

let to_piqi_qualifiers base p =
  List.map (sou base) (get_qualifiers p)

let to_piqi_firstname_aliases base p =
  List.map (sou base) (get_first_names_aliases p)

let to_piqi_surname_aliases base p =
  List.map (sou base) (get_surnames_aliases p)

let to_piqi_image base p =
  to_piqi_string_aux base (get_image p)

let to_piqi_image_opt base p =
  to_piqi_string_opt_aux base (get_image p)

let to_piqi_iper i = Int32.of_int (Adef.int_of_iper i)

let of_piqi_iper i = Adef.iper_of_int (Int32.to_int i)

let to_piqi_index p = to_piqi_iper (get_key_index p)

let to_piqi_ifam i = Int32.of_int (Adef.int_of_ifam i)

let to_piqi_findex f = to_piqi_ifam (get_ifam f)

(* BIENTOT DEPRECATED *)
let string_of_prec_dmy d =
  let s =
    match (d.day, d.month, d.year) with
     | (0, 0, _) -> string_of_int d.year
     | (0, _, _) -> string_of_int d.month ^ "/" ^ string_of_int d.year
     | _ ->
        string_of_int d.day ^ "/" ^ string_of_int d.month ^ "/"
         ^ string_of_int d.year
  in
  match d.prec with
   | Sure -> Mutil.nominative s
   | About -> "~" ^ s
   | Before -> "<" ^ s
   | After -> ">" ^ s
   | Maybe -> "?" ^ s
   | OrYear d2 -> s ^ "|" ^ string_of_int d2.year2
   | YearInt d2 -> s ^ ".." ^ string_of_int d2.year2

let string_of_date = function
    Dgreg (d, _) -> string_of_prec_dmy d
  | Dtext t -> "(" ^ t ^ ")"

let to_piqi_date_aux fn date =
  Opt.map fn (Adef.od_of_cdate date)

let to_piqi_date_opt_aux =
  to_piqi_date_aux string_of_date

let to_piqi_date_str_aux date =
  Opt.map_default "" string_of_date (Adef.od_of_cdate date)

let to_piqi_birth p =
  to_piqi_date_str_aux (get_birth p)

let to_piqi_birth_opt p =
  to_piqi_date_opt_aux (get_birth p)

let to_piqi_baptism p =
  to_piqi_date_str_aux (get_baptism p)

let to_piqi_baptism_opt p =
  to_piqi_date_opt_aux (get_baptism p)

(* Util.string_of_place conf *)
let to_piqi_birth_place base p =
  to_piqi_string_aux base (get_birth_place p)

let to_piqi_birth_place_opt base p =
  to_piqi_string_opt_aux base (get_birth_place p)

let to_piqi_baptism_place base p =
  to_piqi_string_aux base (get_baptism_place p)

let to_piqi_baptism_place_opt base p =
  to_piqi_string_opt_aux base (get_baptism_place p)

let to_piqi_death_place base p =
  to_piqi_string_aux base (get_death_place p)

let to_piqi_death_place_opt base p =
  to_piqi_string_opt_aux base (get_death_place p)

let to_piqi_burial_place base p =
  to_piqi_string_aux base (get_burial_place p)

let to_piqi_burial_place_opt base p =
  to_piqi_string_opt_aux base (get_burial_place p)

let to_piqi_birth_src base p =
  to_piqi_string_aux base (get_birth_src p)

let to_piqi_birth_src_opt base p =
  to_piqi_string_opt_aux base (get_birth_src p)

let to_piqi_baptism_src base p =
  to_piqi_string_aux base (get_baptism_src p)

let to_piqi_baptism_src_opt base p =
  to_piqi_string_opt_aux base (get_baptism_src p)

let to_piqi_burial_src base p =
  to_piqi_string_aux base (get_burial_src p)

let to_piqi_burial_src_opt base p =
  to_piqi_string_opt_aux base (get_burial_src p)

let to_piqi_death_src base p =
  to_piqi_string_aux base (get_death_src p)

let to_piqi_death_src_opt base p =
  to_piqi_string_opt_aux base (get_death_src p)

let to_piqi_death_type p =
  match get_death p with
  | NotDead -> `not_dead
  | Death _ -> `dead
  | DeadYoung -> `dead_young
  | DeadDontKnowWhen -> `dead_dont_know_when
  | DontKnowIfDead -> `dont_know_if_dead
  | OfCourseDead -> `of_course_dead

let to_piqi_death_type_n_date p =
  match get_death p with
  | NotDead -> (`not_dead, "")
  | Death (_, d) -> (`dead, string_of_date @@ Adef.date_of_cdate d)
  | DeadYoung -> (`dead_young, "")
  | DeadDontKnowWhen -> (`dead_dont_know_when, "")
  | DontKnowIfDead -> (`dont_know_if_dead, "")
  | OfCourseDead -> (`of_course_dead, "")

let to_piqi_death_type_n_date_opt p =
  match get_death p with
  | NotDead -> (`not_dead, None)
  | Death (_, d) -> (`dead, Some (string_of_date @@ Adef.date_of_cdate d))
  | DeadYoung -> (`dead_young, None)
  | DeadDontKnowWhen -> (`dead_dont_know_when, None)
  | DontKnowIfDead -> (`dont_know_if_dead, None)
  | OfCourseDead -> (`of_course_dead, None)

let to_piqi_death_type_n_date_aux fn p =
  match get_death p with
  | NotDead -> (`not_dead, fn None)
  | Death (_, d) -> (`dead, fn (Adef.od_of_cdate d))
  | DeadYoung -> (`dead_young, fn None)
  | DeadDontKnowWhen -> (`dead_dont_know_when, fn None)
  | DontKnowIfDead -> (`dont_know_if_dead, fn None)
  | OfCourseDead -> (`of_course_dead, fn None)

let to_piqi_burial_type p =
  match get_burial p with
  | Buried _ -> `buried
  | Cremated _ -> `cremated
  | _ -> `dont_know

let to_piqi_burial_aux fn p =
  match get_burial p with
  | Buried d | Cremated d -> fn (Adef.od_of_cdate d)
  | _ -> fn None

let to_piqi_burial p =
  match get_burial p with
  | Buried d | Cremated d ->
    Opt.map_default "" string_of_date (Adef.od_of_cdate d)
  | _ -> ""

let to_piqi_burial_opt p =
  match get_burial p with
  | Buried d | Cremated d -> Opt.map string_of_date (Adef.od_of_cdate d)
  | _ -> None

let to_piqi_notes base p =
  to_piqi_string_aux base (get_notes p)

let to_piqi_notes_opt base p =
  to_piqi_string_opt_aux base (get_notes p)

let to_piqi_marriage f =
  Opt.map_default "" string_of_date (Adef.od_of_cdate (get_marriage f))

let to_piqi_marriage_opt f =
  Opt.map string_of_date (Adef.od_of_cdate (get_marriage f))

let to_piqi_marriage_src_opt base f =
  to_piqi_string_opt_aux base (get_marriage_src f)

let to_piqi_marriage_src base f =
  to_piqi_string_aux base (get_marriage_src f)

let to_piqi_fsources base f =
  to_piqi_string_aux base (get_fsources f)

let to_piqi_fsources_opt base f =
  to_piqi_string_opt_aux base (get_fsources f)

let to_piqi_comment base f =
  to_piqi_string_aux base (get_comment f)

let to_piqi_comment_opt base f =
  to_piqi_string_opt_aux base (get_comment f)

let to_piqi_marriage_type f =
  match get_relation f with
  | Married -> `married
  | NotMarried -> `not_married
  | Engaged -> `engaged
  | NoSexesCheckNotMarried -> `no_sexes_check_not_married
  | NoMention -> `no_mention
  | NoSexesCheckMarried -> `no_sexes_check_married

let to_piqi_marriage_place base f =
  to_piqi_string_aux base (get_marriage_place f)

let to_piqi_marriage_place_opt base f =
  to_piqi_string_opt_aux base (get_marriage_place f)

let to_piqi_witnesses f =
  Array.map
    (fun i -> { M.Internal_int32.value = to_piqi_iper i })
    (get_witnesses f)

let to_piqi_witnesses_unboxed f =
  Array.map to_piqi_iper (get_witnesses f)

let to_piqi_father f = to_piqi_iper (get_father f)

let to_piqi_mother f = to_piqi_iper (get_mother f)

let to_piqi_children f =
  Array.map
    (fun i -> { M.Internal_int32.value = to_piqi_iper i })
    (get_children f)

let to_piqi_children_unboxed f =
  Array.map to_piqi_iper (get_children f)

let to_piqi_divorce_type fam =
  match get_divorce fam with
  | NotDivorced -> `not_divorced
  | Divorced _ -> `divorced
  | Separated -> `separated

let to_piqi_divorce_type_n_date_aux fn f =
  match get_divorce f with
  | NotDivorced -> (`not_divorced, None)
  | Divorced cod -> (`divorced, Opt.map fn (Adef.od_of_cdate cod))
  | Separated -> (`separated, None)

let to_piqi_titles fn base p =
  List.map
    (fun t ->
       let (title_type, name) =
         match t.t_name with
         | Tmain -> (`title_main, None)
         | Tname name -> (`title_name, Some (sou base name))
         | Tnone -> (`title_none, None)
       in
       fn
         ~title_type
         ~name
         ~title:(to_piqi_string_opt_aux base t.t_ident)
         ~fief:(to_piqi_string_opt_aux base t.t_place)
         ~date_begin:(Adef.od_of_cdate t.t_date_start)
         ~date_end:(Adef.od_of_cdate t.t_date_end)
         ~nth:(Some (Int32.of_int t.t_nth))
    )
    (get_titles p)

let to_piqi_occupation base p =
  to_piqi_string_aux base (get_occupation p)

let to_piqi_occupation_opt base p =
  to_piqi_string_opt_aux base (get_occupation p)

let to_piqi_occupation_opt_wiki conf base p =
  Opt.map
    (fun s ->
       let s =
         let wi =
           {Wiki.wi_mode = "NOTES"; Wiki.wi_cancel_links = conf.cancel_links;
            Wiki.wi_file_path = Notes.file_path conf base;
            Wiki.wi_person_exists = person_exists conf base;
            Wiki.wi_always_show_link = conf.wizard || conf.friend}
         in
         Wiki.syntax_links conf wi s
       in
       string_with_macros conf [] s)
    (to_piqi_occupation_opt base p)

let to_piqi_psources base p =
  to_piqi_string_aux base (get_psources p)

let to_piqi_psources_opt base p =
  to_piqi_string_opt_aux base (get_psources p)

let to_piqi_related p =
  List.map
    (fun i -> { M.Internal_int32.value = to_piqi_iper i} )
    (get_related p)

let to_piqi_related_unboxed p =
  List.map to_piqi_iper (get_related p)

let to_piqi_rparents fn base p =
  List.map
    (fun rp ->
       let rpt_type =
         match rp.r_type with
         | Adoption -> `rpt_adoption
         | Recognition -> `rpt_recognition
         | CandidateParent -> `rpt_candidate_parent
         | GodParent -> `rpt_god_parent
         | FosterParent -> `rpt_foster_parent
       in
       fn
         ~father:(Opt.map to_piqi_iper rp.r_fath)
         ~mother:(Opt.map to_piqi_iper rp.r_moth)
         ~source:(to_piqi_string_opt_aux base rp.r_sources)
         ~rpt_type
    )
    (get_rparents p)

let to_piqi_families p =
  Mutil.array_to_list_map
    (fun i -> { M.Internal_int32.value = to_piqi_ifam i })
    (get_family p)

let to_piqi_families_unboxed p =
  Mutil.array_to_list_map to_piqi_ifam (get_family p)

let to_piqi_parents p = Opt.map to_piqi_ifam (get_parents p)

let to_piqi_baseprefix baseprefix p =
  if (Adef.iper_of_int (-1)) = (get_key_index p) then "" else baseprefix

let person_firstname_surname_txt base p =
  if not (is_empty_string (get_public_name p)) then
    let fn = sou base (get_public_name p) in
    let sn =
      match get_qualifiers p with
      | s :: _ -> " " ^ sou base s
      | _ -> sou base (get_surname p)
    in
    (fn, sn)
  else
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    let sn =
      match get_qualifiers p with
      | s :: _ -> sn ^ " " ^ sou base s
      | _ -> sn
    in
    (fn, sn)

(* Copie de date.ml sans les balises HTML => on devrait créer *)
(* un date_api.ml qu'on utiliserait à la place de date.ml     *)

let short_prec_year_text conf d =
  let prec =
    match d.prec with
    | About | OrYear _ | YearInt _ ->
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        (match transl conf "about (short date)" with
         | "ca" -> "ca "
         | s -> s ^ " ")
    | Maybe -> "? "
    | Before -> "< "
    | After -> "> "
    | _ -> ""
  in
  prec ^ string_of_int d.year

let partial_short_dates_text conf birth_date death_date p =
  match (birth_date, death_date) with
  | (Some (Dgreg (b, _)), Some (Dtext _)) -> short_prec_year_text conf b ^ "-"
  | (Some (Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
          short_prec_year_text conf b ^ "-"
      | _ -> short_prec_year_text conf b )
  | (None, Some (Dtext _)) ->
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Date.death_symbol conf
      | _ -> "" )
  | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Date.death_symbol conf
      | _ -> "" )
  | (_, _) -> ""

let short_dates_text conf p =
  let (birth_date, death_date, _) = Date.get_birth_death_date p in
  match (birth_date, death_date) with
  | (Some (Dgreg (b, _)), Some (Dgreg (d, _))) ->
    short_prec_year_text conf b ^ "-" ^ short_prec_year_text conf d
  | (Some (Dgreg (b, _)), None) ->
    (* La personne peut être décédée mais ne pas avoir de date. *)
    (match get_death p with
     | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
       short_prec_year_text conf b ^ "-"
     | _ -> short_prec_year_text conf b )
  | (None, Some (Dgreg (d, _))) ->
    (match get_death p with
     | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
       Date.death_symbol conf ^ short_prec_year_text conf d
     | _ -> "" )
  | (None, None) ->
    (* La personne peut être décédée mais ne pas avoir de date. *)
    (match get_death p with
     | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
       Date.death_symbol conf
     | _ -> "" )
  (* On ne peut pas traiter les dates au format texte, mais on *)
  (* affiche tout de même les dates au format Dgreg.           *)
  | (_, _) -> partial_short_dates_text conf birth_date death_date p

let short_dates_text_opt conf p =
  Opt.of_string (short_dates_text conf p)

(* ... utils ... *)

(**/**)

(* *********************************************************************** *)
(*  [Fonc] p_getenvbin : (string * string) list -> string -> string option *)
(** [Description] : Renvoie la valeur associée à la clé donnée. Attention,
                    on ne supprime pas les espaces sinon on peut avoir des
                    mauvaises surprises.
    [Args] :
      - env   : l'environnement dans lequel on cherche la clé
      - label : la clé (dont on cherche la valeur)
    [Retour] :
      - string : la valeur de la clé.
    [Rem] : Non exporté en clair hors de ce module.                        *)
(* *********************************************************************** *)
let p_getenvbin env label =
  let decode_varenv = Wserver.gen_decode false in
  try Some (decode_varenv (List.assoc (decode_varenv label) env))
  with Not_found -> None


(* ********************************************************************* *)
(*  [Fonc] has_bas_loop : config -> base -> bool                         *)
(** [Description] : Renvoie true s'il y a une boucle dans la base.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
    [Retour] :
      - bool : Vrai s'il y a une boucle.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let has_base_loop conf base =
  try let _ = (Util.create_topological_sort conf base) in false
  with (Consang.TopologicalSortError _) -> true


(* Lecture et écriture des dates, directement empruntées à gwcomp/gwu *)

let string_of_dmy d =
  let soy y = if y = 0 then "-0" else string_of_int y in
  let prec =
    match d.prec with
    | About -> "~"
    | Maybe -> "?"
    | Before -> "<"
    | After -> ">"
    | _ -> ""
  in
  let date =
    if (*d.day = 0 &&*) d.month = 0 then Printf.sprintf "%s" (soy d.year)
    else if d.day = 0 then Printf.sprintf "%d/%s" d.month (soy d.year)
    else Printf.sprintf "%d/%d/%s" d.day d.month (soy d.year)
  in
  let delta =
    match d.prec with
    | OrYear d2 -> Printf.sprintf "|%s" (soy d2.year2)
    | YearInt d2 -> Printf.sprintf "..%s" (soy d2.year2)
    | _ -> ""
  in
  prec ^ date ^ delta


(* ********************************************************************* *)
(*  [Fonc] string_of_date2 : string -> Def.date option                   *)
(** [Description] : Renvoie la string d'une date. Directement emprunté
                    de gwu.
    [Args] :
      - date : date convertir en string
    [Retour] :
      - string : renvoie une date au format GeneWeb.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let string_of_date2 date =
  let spaces_to_underscore s =
    String.init (String.length s)
      (fun i ->
         match s.[i] with
         | ' ' -> '_'
         | x -> x)
  in
  match date with
  | Dgreg (d, Dgregorian) -> string_of_dmy d
  | Dgreg (d, Djulian) -> string_of_dmy (Calendar.julian_of_gregorian d) ^ "J"
  | Dgreg (d, Dfrench) -> string_of_dmy (Calendar.french_of_gregorian d) ^ "F"
  | Dgreg (d, Dhebrew) -> string_of_dmy (Calendar.hebrew_of_gregorian d) ^ "H"
  | Dtext t -> Printf.sprintf "0(%s)" (spaces_to_underscore t)


let string_of_date_option date =
  match date with
  | Some d -> string_of_date2 d
  | None -> ""


let date_of_string s = Gwcomp.date_of_string s 0


(**/**) (* Convertion d'une date. *)

(* ********************************************************************* *)
(*  [Fonc] piqi_date_of_date : Def.date -> Mapp.date                     *)
(** [Description] : Converti une date GeneWeb en date piqi.
    [Args] :
      - date : date GeneWeb à convertir
    [Retour] :
      - piqi date : renvoie une date piqi.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let piqi_date_of_date date =
  match date with
  | Dgreg (dmy, cal) ->
      let cal =
        match cal with
        | Dgregorian -> `gregorian
        | Djulian -> `julian
        | Dfrench -> `french
        | Dhebrew -> `hebrew
      in
      let (prec, dmy, dmy2) =
        let (d, m, y, delta) =
          (Int32.of_int dmy.day, Int32.of_int dmy.month,
           Int32.of_int dmy.year, Int32.of_int dmy.delta)
        in
        let dmy1 = {Mapp.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear d2 ->
              let dmy2 =
                {
                  Mapp.Dmy.day = Int32.of_int 0;
                  month = Int32.of_int 0;
                  year = Int32.of_int d2.year2;
                  delta = Int32.of_int 0;
                }
              in
              (`oryear, Some dmy2)
          | YearInt d2 ->
              let dmy2 =
                {
                  Mapp.Dmy.day = Int32.of_int 0;
                  month = Int32.of_int 0;
                  year = Int32.of_int d2.year2;
                  delta = Int32.of_int 0;
                }
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        Mapp.Date.cal = Some cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Dtext txt ->
      {
        Mapp.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }


(* ********************************************************************* *)
(*  [Fonc] date_of_piqi_date : Mapp.date -> Def.date                     *)
(** [Description] : Converti une date piqi en date GeneWeb.
    [Args] :
      - date : date piqi à convertir
    [Retour] :
      - Def.date : renvoie une date GeneWeb.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let date_of_piqi_date date =
  match date.Mapp.Date.text with
  | Some txt -> Dtext txt
  | _ ->
      let cal =
        match date.Mapp.Date.cal with
        | Some `julian -> Djulian
        | Some `french -> Dfrench
        | Some `hebrew -> Dhebrew
        | _ -> Dgregorian
      in
      let prec =
        match date.Mapp.Date.prec with
        | Some `about -> About
        | Some `maybe -> Maybe
        | Some `before -> Before
        | Some `after -> After
        | Some `oryear ->
            (match date.Mapp.Date.dmy2 with
            | Some dmy ->
                let y = Int32.to_int dmy.Mapp.Dmy.year in
                let dmy2 = {day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
                OrYear dmy2
            | None -> OrYear {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | Some `yearint ->
            (match date.Mapp.Date.dmy2 with
            | Some dmy ->
                let y = Int32.to_int dmy.Mapp.Dmy.year in
                let dmy2 = {day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
                YearInt dmy2
            | None -> YearInt {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | _ -> Sure
      in
      let dmy =
        match date.Mapp.Date.dmy with
        | Some dmy ->
            let day = Int32.to_int dmy.Mapp.Dmy.day in
            let month = Int32.to_int dmy.Mapp.Dmy.month in
            let year = Int32.to_int dmy.Mapp.Dmy.year in
            let delta = Int32.to_int dmy.Mapp.Dmy.delta in
            {day = day; month = month; year = year; prec = prec; delta = delta}
        | None -> (* erreur*)
            {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
      in
      Dgreg (dmy, cal)


let p_publicname base p =
  let public_name = Mutil.nominative (sou base (get_public_name p)) in
  if public_name = "" then None
  else Some public_name

let parent_has_title conf base p =
  match get_parents p with
  | Some ifam ->
      let cpl = foi base ifam in
      let fath = pget conf base (get_father cpl) in
      let moth = pget conf base (get_mother cpl) in
      get_access fath <> Private && nobtit conf base fath <> [] ||
      get_access moth <> Private && nobtit conf base moth <> []
  | _ -> false


(* ********************************************************************* *)
(*  [Fonc] date_included : dmy -> dmy -> dmy -> bool                     *)
(** [Description] : d1 <= d <= d2
    [Args] :
      - d  : date
      - d1 : date min
      - d2 : date max
    [Retour] :
      - bool : renvoie d1 <= d <= d2.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let date_included d d1 d2 =
  (* Fonction générique de test: y <= x <= z *)
  (* Le paramètre max permet de tester par   *)
  (* rapport au nombre max de jour ou mois.  *)
  let comp x y z max =
    if y <= z then (y <= x) && (x <= z)
    else if max > 0 then ((y <= x) && (x <= max)) || ((1 <= x) && (x <= z))
    else false
  in
  let (d, m, y) = (d.day, d.month, d.year) in
  let { day = d2 ; month = m2 ; year = y2 } = d2 in
  match d1 with
  | {day = 0; month = 0; year = 0} -> false
  | {day = d1; month = 0; year = 0} ->
    d2 <> 0 && m2 = 0 && y2 = 0 && d > 0 && comp d d1 d2 31
  | {day = 0; month = m1; year = 0} ->
    m2 <> 0 && d2 = 0 && y2 = 0 && m > 0 && comp m m1 m2 12
  | {day = 0; month = 0; year = y1} ->
    y2 <> 0 && d2 = 0 && m2 = 0 && comp y y1 y2 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = m1; year = 0} ->
    d2 <> 0 && m2 <> 0 && y2 = 0
    && d > 0
    && m > 0
    && comp (m * 100 + d) (m1 * 100 + d1) (m2 * 100 + d2) (12 * 100 + 31)
  | {day = 0; month = m1; year = y1} ->
    m2 <> 0 && y2 <> 0 && d2 = 0
    && m > 0 && comp (y * 100 + m) (y1 * 100 + m1) (y2 * 100 + m2) 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = 0; year = y1} ->
    d2 <> 0 && y2 <> 0 && m2 = 0
    && d > 0 && y1 = y2 && comp d d1 d2 31
  | {day = d1; month = m1; year = y1} ->
    y2 <> 0 &&
    comp
      (y * 10000 + m * 100 + d)
      (y1 * 10000 + m1 * 100 + d1)
      (y2 * 10000 + m2 * 100 + d2)
      0

(**/**) (* Divers filtres possibles. *)

(* ********************************************************************* *)
(*  [Fonc] reduce_to_sosa : person list -> person list                   *)
(** [Description] : Renvoie la liste des personnes ayant un numéro sosa.
    [Args] :
      - l    : liste de personnes
    [Retour] :
      - person list : Retourne la liste des personnes avec un sosa.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let reduce_to_sosa compute_sosa l =
  let rec loop l accu =
    match l with
    | [] -> accu
    | p :: l ->
        let sosa = compute_sosa p in
        if Sosa.gt sosa Sosa.zero then loop l (p :: accu)
        else loop l accu
  in loop l []


(* ********************************************************************* *)
(*  [Fonc] reduce_to_recent : config -> person list -> person list       *)
(** [Description] : Renvoie la liste des contemporains.
    [Args] :
      - conf : configuration de la base
      - l    : liste de personnes
    [Retour] :
      - person list : Retourne la liste des contemporains.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let reduce_to_recent conf l =
  let tmp_conf = {(conf) with private_years = max 85 conf.private_years} in
  let rec loop l accu =
    match l with
    | [] -> accu
    | p :: l ->
        if Util.is_old_person tmp_conf (gen_person_of_person p) then
          loop l accu
        else
          loop l (p :: accu)
  in loop l []


(* *********************************************************************** *)
(*  [Fonc] is_visible : config -> base -> person -> bool                   *)
(** [Description] : Renvoie vrai si l'on peut afficher les informations
                    d'une personne. Une personne est visible si elle n'est
                    pas privée OU si elle n'est plus contemporaine.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : person
    [Retour] : string
    [Rem] : Exporté en clair hors de ce module.                            *)
(* *********************************************************************** *)
let is_visible conf base p =
  let tmp_conf = {(conf) with wizard = false; friend = false} in
  Util.authorized_age tmp_conf base p


(* ********************************************************************* *)
(*  [Fonc] is_sosa : (person -> Sosa.t) -> person -> bool            *)
(** [Description] : Test si la personne est un sosa.
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_sosa compute_sosa p =
  Sosa.gt (compute_sosa p) Sosa.zero


(* ********************************************************************* *)
(*  [Fonc] is_recent : config -> person -> bool                          *)
(** [Description] : Test si la personne est un contemporain.
    [Args] :
      - conf : configuration de la base
      - p    : person
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_recent conf p =
  let tmp_conf =
    {(conf) with private_years = max 85 conf.private_years;
      (* !!! Si on n'a pas de dates, on considère qu'on est contemporain.
         (Mantis 1327) *)
      public_if_no_date = false}
  in
  not (Util.is_old_person tmp_conf (gen_person_of_person p))


(* ********************************************************************* *)
(*  [Fonc] check_sex : person -> Def.sex -> bool                         *)
(** [Description] : Test si la personne est du même sexe que sex.
    [Args] :
      - conf : configuration de la base
      - p    : person
      - sex  : sexe que l'on cherche
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let check_sex p sex = get_sex p = sex


(* ********************************************************************* *)
(*  [Fonc] is_date_included : bool -> date -> date -> date -> bool       *)
(** [Description] : Test si d1 <= d <= d2.
    [Args] :
      - prec   : booléen pour savoir si l'on veut tester une date précise
                 (par exemple Octobre 1800 n'est pas une date précise)
      - d      : date que l'on cherche
      - d1, d2 : interval de date
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_date_included prec d d1 d2 =
  match d with
  | Some (Dgreg (d, _)) ->
      ((prec && d.prec = Sure) || not prec) && date_included d d1 d2
  | _ -> false

(** [apply_filters_p conf filters compute_sosa p]
    Test if [p] satisfies every filters *)
let apply_filters_p conf filters compute_sosa p =
  begin match filters.filter_sex with
    | Some sex -> check_sex p sex
    | None -> true
  end
  && begin not filters.only_sosa || is_sosa compute_sosa p end
  && begin not filters.only_recent || is_recent conf p end
  && begin match filters.date_birth with
    | Some (start, stop, prec) ->
      is_date_included prec (Adef.od_of_cdate (get_birth p)) start stop
    | None -> true
  end
  && begin match filters.date_death with
    | Some (start, stop, prec) ->
      let death = match get_death p with
        | Death (_, cd) -> Some (Adef.date_of_cdate cd)
        | _ -> None
      in
      is_date_included prec death start stop
    | None -> true
  end
(**/**) (* Fonctions IO *)


(* ********************************************************************* *)
(*  [Fonc] get_params :
      config -> (string -> [> `json | `pb | `xml ] -> 'a) -> 'a          *)
(** [Description] : Récupère les paramètres passés dans la requête.
    [Args] :
      - conf  : configuration de la base
      - parse : la fonction de parser qui permet de récupérer les
                paramètres
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let get_params conf parse =
  match (p_getenvbin conf.env "data", p_getenvbin conf.env "input") with
  | (Some d, Some "pb") -> parse d `pb
  | (Some d, Some "json") -> parse d `json
  | (Some d, Some "xml") -> parse d `xml
  | _ -> exit (-2)


(* ********************************************************************* *)
(*  [Fonc] get_filters : config -> Api_def.filters                       *)
(** [Description] : Récupère les filtres passés dans la requête.
    [Args] :
      - conf : configuration de la base
    [Retour] : Api_def.filters
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let get_filters conf =
  let filters =
    match (p_getenvbin conf.env "filters", p_getenvbin conf.env "input") with
    | (Some d, Some "pb") -> Mext.parse_filters d `pb
    | (Some d, Some "json") -> Mext.parse_filters d `json
    | (Some d, Some "xml") -> Mext.parse_filters d `xml
    | _ -> Mext.parse_filters "" `pb (* aucun filtre passé *)
  in
  { only_sosa = filters.M.Filters.only_sosa;
    only_recent = filters.M.Filters.only_recent;
    filter_sex =
      (match filters.M.Filters.sex with
      | Some `male -> Some Male
      | Some `female -> Some Female
      | Some `unknown -> Some Neuter
      | _ -> None);
    nb_results = filters.M.Filters.nb_results;
    date_birth =
      (match filters.M.Filters.date_birth with
      | Some range ->
          let date_begin = range.M.Filter_date_range.date_begin in
          let dmy1 =
            { day = Int32.to_int date_begin.M.Filter_date.day;
              month = Int32.to_int date_begin.M.Filter_date.month;
              year = Int32.to_int date_begin.M.Filter_date.year;
              prec = Sure; delta = 0 }
          in
          let date_end = range.M.Filter_date_range.date_end in
          let dmy2 =
            { day = Int32.to_int date_end.M.Filter_date.day;
              month = Int32.to_int date_end.M.Filter_date.month;
              year = Int32.to_int date_end.M.Filter_date.year;
              prec = Sure; delta = 0 }
          in
          let prec = range.M.Filter_date_range.only_exact in
          Some (dmy1, dmy2, prec)
      | None -> None);
    date_death =
      (match filters.M.Filters.date_death with
      | Some range ->
          let date_begin = range.M.Filter_date_range.date_begin in
          let dmy1 =
            { day = Int32.to_int date_begin.M.Filter_date.day;
              month = Int32.to_int date_begin.M.Filter_date.month;
              year = Int32.to_int date_begin.M.Filter_date.year;
              prec = Sure; delta = 0 }
          in
          let date_end = range.M.Filter_date_range.date_end in
          let dmy2 =
            { day = Int32.to_int date_end.M.Filter_date.day;
              month = Int32.to_int date_end.M.Filter_date.month;
              year = Int32.to_int date_end.M.Filter_date.year;
              prec = Sure; delta = 0 }
          in
          let prec = range.M.Filter_date_range.only_exact in
          Some (dmy1, dmy2, prec)
      | None -> None);
  }


(* ********************************************************************* *)
(*  [Fonc] print_result : config -> (fun output_format -> string -> unit *)
(** [Description] : Transforme un type piqi en fonction de son format de
                    sortie puis appelle la fonction print du serveur pour
                    afficher le résultat.
    [Args] :
      - conf : configuration de la base
      - Piqirun.OBuf.t : le résultat de la requête
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let print_result conf data =
  let (content_type, output) =
    match p_getenvbin conf.env "output" with
     | Some "pb" -> ("application/octet-stream", `pb)
     | Some "json" -> ("application/json", `json)
     | Some "xml" -> ("application/xml", `xml)
     | _ -> exit (-2)
  in
  let data = data output in
  Util.html ~content_type conf ;
  Wserver.printf "%s" data


(**/**) (* Fonctions de transformation person <=> piqi person *)

(* ********************************************************************* *)
(*  [Fonc] piqi_ref_person_to_person :
      base ->  Reference_person -> option person                         *)
(** [Description] : Renvoie une option personne à partir d'une référence
                    piqi person.
    [Args] :
      - base : base de donnée
      - ref_person : Reference_person
    [Retour] :
      - option person : Retourne une option personne.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
(* FIXME: use pget *)
let piqi_ref_person_to_person base ref_person =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
  | Some ip -> Some (poi base ip)
  | None -> None

(** Only [n], [p] and [oc] get filled *)
let empty_piqi_person_light conf ref_person base_loop =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  {
    M.Person.sosa = if base_loop then "-1" else "0";
    n = sn;
    p = fn;
    oc = occ;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    image = "";
    birth_date = "";
    birth_place = "";
    baptism_date = "";
    baptism_place = "";
    death_date = "";
    death_place = "";
    death_type = `not_dead;
    burial_date = "";
    burial_place = "";
    spouses = [];
    ascend = false;
    descend = false;
    visible_for_visitors = false;
    baseprefix = conf.command;
  }

(** Only [n], [p] and [oc] get filled *)
let empty_piqi_person_full conf ref_person base_loop =
  let sn = ref_person.M.Reference_person.n in
  let fn = ref_person.M.Reference_person.p in
  let occ = ref_person.M.Reference_person.oc in
  {
    M.Full_person.sosa = if base_loop then "-1" else "0";
    n = sn;
    p = fn;
    oc = occ;
    index = -1l;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    aliases = [];
    qualifiers = [];
    firstname_aliases = [];
    surname_aliases = [];
    image = None;
    birth_date = None;
    birth_place = None;
    birth_src = None;
    baptism_date = None;
    baptism_place = None;
    baptism_src = None;
    death_date = None;
    death_place = None;
    death_src = None;
    death_type = `not_dead;
    burial_date = None;
    burial_place = None;
    burial_src = None;
    occupation = None;
    psources = None;
    titles = [];
    related = [];
    rparents = [];
    visible_for_visitors = false;
    parents = None;
    families = [];
    baseprefix = conf.command;
  }

(** Only [n], [p] and [oc] get filled *)
let empty_piqi_person conf ref_person base_loop =
(* TODO => peut être un enum piqi ?
  match p_getenv conf.env "type_person" with
  | Some "light" -> Light (empty_piqi_person_light conf ref_person base_loop)
  | Some "full" -> Full (empty_piqi_person_full conf ref_person base_loop)
  | _ -> exit (-2)
*)
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull (empty_piqi_person_full conf ref_person base_loop)
  else
    PLight (empty_piqi_person_light conf ref_person base_loop)

(** No security check *)
let spouse_to_piqi_spouse conf base p fam base_loop compute_sosa =
  let sosa_p =
    if base_loop then "-1"
    else Sosa.to_string (compute_sosa p)
  in
  let sex = to_piqi_sex p in
  let surname = to_piqi_surname base p in
  let first_name = to_piqi_firstname base p in
  let sn = to_piqi_sn surname in
  let fn = to_piqi_fn first_name in
  let occ = to_piqi_occ p in
  let publicname = to_piqi_publicname base p in
  let image = to_piqi_image base p in
  let birth = to_piqi_birth p in
  let birth_place = to_piqi_birth_place base p in
  let baptism = to_piqi_baptism p in
  let baptism_place = to_piqi_baptism_place base p in
  let (death_type, death) = to_piqi_death_type_n_date p in
  let death_place = to_piqi_death_place base p in
  let burial = to_piqi_burial p in
  let burial_place = to_piqi_burial_place base p in
  let marriage_date = to_piqi_marriage fam in
  let marriage_place = to_piqi_marriage_place base fam in
  let divorce_type = to_piqi_divorce_type fam in
  let visible = is_visible conf base p in
  {
    M.Spouse.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    image = image;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_date = death;
    death_place = death_place;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    marriage_date = marriage_date;
    marriage_place = marriage_place;
    divorce_type = divorce_type;
    visible_for_visitors = visible;
  }

(** No security check done here  *)
let pers_to_piqi_person_light conf base p base_loop compute_sosa =
  let sosa_p =
    if base_loop then "-1"
    else Sosa.to_string (compute_sosa p)
  in
  let lastname = to_piqi_surname base p in
  let firstname = to_piqi_firstname base p in
  let sn = to_piqi_sn lastname in
  let fn = to_piqi_fn firstname in
  let (death_type, death_date) = to_piqi_death_type_n_date p in
  let fams = Array.map (fget conf base) (get_family p) in
  let sl =
    Array.map
      (fun fam ->
         let p = pget conf base (Gutil.spouse (get_key_index p) fam) in
         spouse_to_piqi_spouse conf base p fam base_loop compute_sosa)
      fams
  in
  let descend =
    Array.exists (fun c -> Array.length (get_children c) > 0) fams
  in
  { M.Person.sosa = sosa_p
  ; n = sn
  ; p = fn
  ; oc = to_piqi_occ p
  ; sex = to_piqi_sex p
  ; lastname
  ; firstname
  ; public_name = to_piqi_publicname base p
  ; image = to_piqi_image base p
  ; birth_date = to_piqi_birth p
  ; birth_place = to_piqi_birth_place base p
  ; baptism_date = to_piqi_baptism p
  ; baptism_place = to_piqi_baptism_place base p
  ; death_date
  ; death_place = to_piqi_death_place base p
  ; death_type
  ; burial_date = to_piqi_burial p
  ; burial_place = to_piqi_burial_place base p
  ; spouses = Array.to_list sl
  ; ascend = get_parents p <> None
  ; descend = descend
  ; visible_for_visitors = is_visible conf base p
  ; baseprefix = conf.command
  }

(** No security check *)
let pers_to_piqi_person_full conf base p base_loop compute_sosa =
  let sosa_p =
    if base_loop then "-1"
    else Sosa.to_string (compute_sosa p)
  in
  let surname = to_piqi_surname base p in
  let first_name = to_piqi_firstname base p in
  let sn = to_piqi_sn surname in
  let fn = to_piqi_fn first_name in
  let (death_type, death) = to_piqi_death_type_n_date_opt p in
  let titles =
    let fn ~title_type ~name ~title ~fief ~date_begin ~date_end ~nth =
       { M.Title.title_type ; name ; title ; fief ; nth
       ; date_begin = Opt.map string_of_date date_begin
       ; date_end = Opt.map string_of_date date_end
       }
    in
    to_piqi_titles fn base p
  in
  let rparents =
    let fn ~father ~mother ~source ~rpt_type =
      { M.Relation_parent.father ; mother ; source ; rpt_type }
    in
    to_piqi_rparents fn base p
  in
  {
    M.Full_person.sosa = sosa_p
    ; n = sn
    ; p = fn
    ; oc = to_piqi_occ p
    ; index = to_piqi_index p
    ; sex = to_piqi_sex p
    ; lastname = surname
    ; firstname = first_name
    ; public_name = to_piqi_publicname base p
    ; aliases = to_piqi_aliases base p
    ; qualifiers = to_piqi_qualifiers base p
    ; firstname_aliases = to_piqi_firstname_aliases base p
    ; surname_aliases = to_piqi_surname_aliases base p
    ; image = to_piqi_image_opt base p
    ; birth_date = to_piqi_birth_opt p
    ; birth_place = to_piqi_birth_place_opt base p
    ; birth_src = to_piqi_birth_src_opt base p
    ; baptism_date = to_piqi_baptism_opt p
    ; baptism_place = to_piqi_baptism_place_opt base p
    ; baptism_src = to_piqi_baptism_src_opt base p
    ; death_date = death
    ; death_place = to_piqi_death_place_opt base p
    ; death_src = to_piqi_death_src_opt base p
    ; death_type = death_type
    ; burial_date = to_piqi_burial_opt p
    ; burial_place = to_piqi_burial_place_opt base p
    ; burial_src = to_piqi_burial_src_opt base p
    ; occupation = to_piqi_occupation_opt base p
    ; psources = to_piqi_psources_opt base p
    ; titles
    ; related = to_piqi_related p
    ; rparents
    ; visible_for_visitors = is_visible conf base p
    ; parents = to_piqi_parents p
    ; families = to_piqi_families p
    ; baseprefix = conf.command
    }

let pers_to_piqi_person conf base p base_loop compute_sosa =
(* TODO
  match p_getenv conf.env "type_person" with
  | Some "light" -> Light (empty_piqi_person_light conf ref_person base_loop)
  | Some "full" -> Full (empty_piqi_person_full conf ref_person base_loop)
  | _ -> exit (-2)
*)
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull (pers_to_piqi_person_full conf base p base_loop compute_sosa)
  else
    PLight (pers_to_piqi_person_light conf base p base_loop compute_sosa)

let fam_to_piqi_family conf base ifam =
  let f = fget conf base ifam in
  let (divorce_type, divorce_date) =
    to_piqi_divorce_type_n_date_aux string_of_date f
  in
  { M.Full_family.fsources = to_piqi_fsources_opt base f
  ; marriage_date = to_piqi_marriage_opt f
  ; marriage_place = to_piqi_marriage_place_opt base f
  ; marriage_src = to_piqi_marriage_src_opt base f
  ; marriage_type = to_piqi_marriage_type f
  ; divorce_type
  ; divorce_date
  ; witnesses = Array.to_list @@ to_piqi_witnesses f
  ; father = to_piqi_father f
  ; mother = to_piqi_mother f
  ; children = Array.to_list @@ to_piqi_children f
  ; index = to_piqi_findex f
  }

let fam_to_piqi_family_link base (ifath, imoth) ifam f =
  let (divorce_type, divorce_date) =
    to_piqi_divorce_type_n_date_aux string_of_date f
  in
  { M.Full_family.fsources = None
  ; marriage_date = to_piqi_marriage_opt f
  ; marriage_place = to_piqi_marriage_place_opt base f
  ; marriage_src = to_piqi_marriage_src_opt base f
  ; marriage_type = to_piqi_marriage_type f
  ; divorce_type = divorce_type
  ; divorce_date = divorce_date
  ; witnesses = []
  ; father = to_piqi_iper ifath
  ; mother = to_piqi_iper imoth
  ; children = []
  ; index = to_piqi_ifam ifam
  }

(**/**)
(** No security check  *)
let pers_to_piqi_app_person conf base p =
  let events =
    List.map
      (fun (name, date, place, note, src, w, isp) ->
        let (name, text) =
          match name with
          | Perso.Pevent (Epers_Name s) -> (None, Some (sou base s))
          | Perso.Pevent name -> (Some (to_piqi_pevent_aux name), None)
          | Perso.Fevent (Efam_Name s) -> (None, Some (sou base s))
          | Perso.Fevent name -> (Some (to_piqi_fevent_aux name), None)
        in
        let witnesses =
          Mutil.array_to_list_map
            (fun (ip, wk) ->
               let witness_type =
                 match wk with
                 | Witness -> `witness
                 | Witness_GodParent -> `witness_godparent
               in
               let index = Int32.of_int (Adef.int_of_iper ip) in
               Mapp.Witness_event.({
                 witness_type = witness_type;
                 witness = index;
               }))
            w
        in
        { Mapp.Event.name
        ; text
        ; date = Opt.map piqi_date_of_date (Adef.od_of_cdate date)
        ; place = to_piqi_string_opt_aux base place
        ; reason = None
        ; note = to_piqi_string_opt_aux base note
        ; src = to_piqi_string_opt_aux base src
        ; witnesses
        ; index_spouse = Opt.map to_piqi_iper isp
        })
      (Perso.events_list conf base p)
  in
  let death_type, death_date =
    to_piqi_death_type_n_date_aux (Opt.map piqi_date_of_date) p
  in
  let titles =
    let fn ~title_type ~name ~title ~fief ~date_begin ~date_end ~nth =
       { Mapp.Title.title_type ; name ; title ; fief
       ; date_begin = Opt.map piqi_date_of_date date_begin
       ; date_end = Opt.map piqi_date_of_date date_end
       ; nth }
    in
    to_piqi_titles fn base p
  in
  let rparents =
    let fn ~father ~mother ~source ~rpt_type =
      { Mapp.Relation_parent.father ; mother ; source ; rpt_type }
    in
    to_piqi_rparents fn base p
  in
  { Mapp.Person.index = to_piqi_index p
  ; sex = to_piqi_sex p
  ; lastname = to_piqi_surname base p
  ; firstname = to_piqi_firstname base p
  ; occ = to_piqi_occ p
  ; public_name = to_piqi_publicname base p
  ; aliases = to_piqi_aliases base p
  ; qualifiers = to_piqi_qualifiers base p
  ; firstname_aliases = to_piqi_firstname_aliases base p
  ; surname_aliases = to_piqi_surname_aliases base p
  ; image = to_piqi_image base p <> ""
  ; birth_date = Opt.map piqi_date_of_date (Adef.od_of_cdate @@ get_birth p)
  ; birth_place = to_piqi_birth_place_opt base p
  ; birth_src = to_piqi_birth_src_opt base p
  ; baptism_date = Opt.map piqi_date_of_date (Adef.od_of_cdate @@ get_baptism p)
  ; baptism_place = to_piqi_baptism_place_opt base p
  ; baptism_src = to_piqi_baptism_src_opt base p
  ; death_date
  ; death_place = to_piqi_death_place_opt base p
  ; death_src = to_piqi_death_src_opt base p
  ; death_type
  ; burial_date = to_piqi_burial_aux (Opt.map piqi_date_of_date) p
  ; burial_place = to_piqi_burial_place_opt base p
  ; burial_src = to_piqi_burial_src_opt base p
  ; occupation = to_piqi_occupation_opt base p
  ; psources = to_piqi_psources_opt base p
  ; titles
  ; related = to_piqi_related_unboxed p
  ; rparents
  ; access = to_piqi_access p
  ; parents = to_piqi_parents p
  ; families = to_piqi_families_unboxed p
  ; events = events
  }

let fam_to_piqi_app_family conf base ifam =
  let f = fget conf base ifam in
  let index = to_piqi_ifam (get_ifam f) in
  let (divorce_type, divorce_date) =
    to_piqi_divorce_type_n_date_aux piqi_date_of_date f
  in
  { Mapp.Family.index = index
  ; marriage_date = Opt.map piqi_date_of_date @@ Adef.od_of_cdate (get_marriage f)
  ; marriage_place = to_piqi_marriage_place_opt base f
  ; marriage_src = to_piqi_marriage_src_opt base f
  ; marriage_type = to_piqi_marriage_type f
  ; divorce_type
  ; divorce_date
  ; witnesses = Array.to_list @@ to_piqi_witnesses_unboxed f
  ; fsources = to_piqi_fsources_opt base f
  ; father = to_piqi_iper (get_father f)
  ; mother = to_piqi_iper (get_mother f)
  ; children = Array.to_list @@ to_piqi_children_unboxed f
  }


(**/**) (* Fonctions de conversion *)


let data_person p =
  match p with
  | PLight p -> Mext.gen_person p
  | PFull p -> Mext.gen_full_person p

let person_map conf base l compute_sosa =
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.map
         (fun p -> pers_to_piqi_person_full conf base p base_loop compute_sosa)
         l)
  else
    PLight
      (List.map
         (fun p -> pers_to_piqi_person_light conf base p base_loop compute_sosa)
         l)

let conv_data_list_person conf base filters l =
  if filters.nb_results then
    let len = M.Internal_int32.({value = Int32.of_int (List.length l)}) in
    Mext.gen_internal_int32 len
  else
    let compute_sosa =
      if List.length l > 1 then
        let () = Perso.build_sosa_ht conf base in
        Perso.get_sosa_person
      else (Perso.get_single_sosa conf base)
    in
    let l = person_map conf base l compute_sosa in
    match l with
    | PLight pl ->
        let list = M.List_persons.({list_persons = pl}) in
        Mext.gen_list_persons list
    | PFull pl ->
        let list = M.List_full_persons.({persons = pl}) in
        Mext.gen_list_full_persons list

let data_list_person conf base filters l =
  let compute_sosa =
    if List.length l > 1 then
      let () = Perso.build_sosa_ht conf base in
      Perso.get_sosa_person
    else (Perso.get_single_sosa conf base)
  in
  let l = List.filter (apply_filters_p conf filters compute_sosa) l in
  if filters.nb_results then
    let len = M.Internal_int32.({value = Int32.of_int (List.length l)}) in
    Mext.gen_internal_int32 len
  else
    let l = person_map conf base l compute_sosa in
    match l with
    | PLight pl ->
        let list = M.List_persons.({list_persons = pl}) in
        Mext.gen_list_persons list
    | PFull pl ->
        let list = M.List_full_persons.({persons = pl}) in
        Mext.gen_list_full_persons list

let data_list_person_option conf base filters l =
  let compute_sosa =
    if List.length l > 1 then
      let () = Perso.build_sosa_ht conf base in
      Perso.get_sosa_person
    else (Perso.get_single_sosa conf base)
  in
  if filters.nb_results then
    let len = M.Internal_int32.({value = Int32.of_int (List.length l)}) in
    Mext.gen_internal_int32 len
  else
    let base_loop = has_base_loop conf base in
    let l =
      if p_getenvbin conf.env "full_infos" = Some "1" then
        PFull
          (List.map
            (fun p ->
              match p with
              | PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_full conf base p base_loop compute_sosa
                  else
                    let fn = Name.lower (sou base (get_first_name p)) in
                    let sn = Name.lower (sou base (get_surname p)) in
                    let occ = Int32.of_int (get_occ p) in
                    let ref_p =
                      M.Reference_person.({
                        n = sn;
                        p = fn;
                        oc = occ;
                      })
                    in
                    empty_piqi_person_full conf ref_p base_loop
              | PLight ref_p -> empty_piqi_person_full conf ref_p base_loop )
            l)
      else
        PLight
          (List.map
            (fun p ->
              match p with
              | PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_light conf base p base_loop compute_sosa
                  else
                    let fn = Name.lower (sou base (get_first_name p)) in
                    let sn = Name.lower (sou base (get_surname p)) in
                    let occ = Int32.of_int (get_occ p) in
                    let ref_p =
                      M.Reference_person.({
                        n = sn;
                        p = fn;
                        oc = occ;
                      })
                    in
                    empty_piqi_person_light conf ref_p base_loop
              | PLight ref_p -> empty_piqi_person_light conf ref_p base_loop )
            l)
    in
    match l with
    | PLight pl ->
        let list = M.List_persons.({list_persons = pl}) in
        Mext.gen_list_persons list
    | PFull pl ->
        let list = M.List_full_persons.({persons = pl}) in
        Mext.gen_list_full_persons list


let person_node_map conf base l =
  let compute_sosa =
    if List.length l > 1 then
      let () = Perso.build_sosa_ht conf base in
      Perso.get_sosa_person
    else (Perso.get_single_sosa conf base)
  in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.rev_map
         (fun p ->
           let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
           let p =
             pers_to_piqi_person_full conf base p base_loop compute_sosa
           in
           M.Full_node.({
             id = id;
             person = p;
           }))
         l)
  else
    PLight
      (List.rev_map
         (fun p ->
           let id = Int64.of_int (Adef.int_of_iper (get_key_index p)) in
           let p =
             pers_to_piqi_person_light conf base p base_loop compute_sosa
           in
           M.Node.({
             id = id;
             person = p;
           }))
         l)

let person_node_map_lia conf base l =
  let compute_sosa = (fun _ -> Sosa.zero) in
  let base_loop = has_base_loop conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    PFull
      (List.rev_map
         (fun (id, p) ->
           let p = pers_to_piqi_person_full conf base p base_loop compute_sosa in
           M.Full_node.({
             id = id;
             person = p;
           }))
         l)
  else
    PLight
      (List.rev_map
         (fun (id, p) ->
           let p = pers_to_piqi_person_light conf base p base_loop compute_sosa in
           M.Node.({
             id = id;
             person = p;
           }))
         l)

#endif
