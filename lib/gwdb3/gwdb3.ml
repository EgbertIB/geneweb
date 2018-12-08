(**
   This module is meant to be used in a javascript application,
   so int are expected to be 32 bits (javascript).

   On 32 bits system, OCaml has these upper bounds:
   - 1 073 741 823 is [max_int] for OCaml on 32 bits platform.
   - 4 194 303 is [max_array_length]


   As it is used in a cordova app, this storage system needs to
   load all ressources in memory when starting, so the key here
   is to keep the memory needed as low as possible.

*)

open Adef

type iper = int
type istr = int
type ifam = int

type person =
  { p : (iper, istr) gen_person
  ; a : ifam gen_ascend
  ; u : ifam gen_union }

type family =
  { f : (iper, dsk_istr) gen_family
  ; c : iper gen_couple
  ; d : iper gen_descend }

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

type string_person_index =
  { cursor : int
  ; data : istr array
  }

type base =
  { mutable persons : person array
  ; mutable families : family array
  ; mutable strings : string array
  }

let index_of a x =
  if a = [||] then raise Not_found
  else
    let rec aux i j =
      if i = j then
        if Array.unsafe_get a i = x then i
        else raise Not_found
      else let k = (i + j) / 2 in
        if Array.unsafe_get a k > x then aux i (k - 1)
        else if Array.unsafe_get a k > x then aux (k + 1) j
        else k
    in
    aux 0 0

let open_base bname : base =
  let fname = bname in
  let ic = open_in_bin fname in
  let base : base = Marshal.from_channel ic in
  let () = close_in ic in
  (fname, base)

let close_close _base = ()

let eq_istr = (=)
let is_empty_string = (=) 0
let is_quest_string = is_empty_string

let no_person =
  { first_name = 0
  ; surname = 0
  ; occ = 0
  ; image = 0
  ; first_names_aliases = []
  ; surnames_aliases = []
  ; public_name = 0
  ; qualifiers = []
  ; titles = []
  ; rparents = []
  ; related = []
  ; aliases = []
  ; occupation = 0
  ; sex = Neuter
  ; access = Private
  ; birth = Adef.cdate_None
  ; birth_place = 0
  ; birth_note = 0
  ; birth_src = 0
  ; baptism = Adef.cdate_None
  ; baptism_place = 0
  ; baptism_note = 0
  ; baptism_src = 0
  ; death = DontKnowIfDead
  ; death_place = 0
  ; death_note = 0
  ; death_src = 0
  ; burial = UnknownBurial
  ; burial_place = 0
  ; burial_note = 0
  ; burial_src = 0
  ; pevents = []
  ; notes = 0
  ; psources = 0
  ; key_index = -1
  }
let no_ascend =
  { parents = None
  ; consang = Adef.no_consang
  }
let no_union =
  { family = [||]
  }

let empty_person i =
  { p = { no_person with key_index = i }
  ; a = no_ascend
  ; u = no_union
  }

let get_access = fun p -> p.Def.access
let get_aliases = fun p -> p.Def.aliases
let get_baptism = fun p -> p.Def.baptism
let get_baptism_note = fun p -> p.Def.baptism_note
let get_baptism_place = fun p -> p.Def.baptism_place
let get_baptism_src = fun p -> p.Def.baptism_src
let get_birth = fun p -> p.Def.birth
let get_birth_note = fun p -> p.Def.birth_note
let get_birth_place = fun p -> p.Def.birth_place
let get_birth_src = fun p -> p.Def.birth_src
let get_burial = fun p -> p.Def.burial
let get_burial_note = fun p -> p.Def.burial_note
let get_burial_place = fun p -> p.Def.burial_place
let get_burial_src = fun p -> p.Def.burial_src
let get_consang = fun a -> a.Def.consang
let get_death = fun p -> p.Def.death
let get_death_note = fun p -> p.Def.death_note
let get_death_place = fun p -> p.Def.death_place
let get_death_src = fun p -> p.Def.death_src
let get_family = fun u -> u.Def.family
let get_first_name = fun p -> p.Def.first_name
let get_first_names_aliases = fun p -> p.Def.first_names_aliases
let get_image = fun p -> p.Def.image
let get_key_index = fun p -> p.Def.key_index
let get_notes = fun p -> p.Def.notes
let get_occ = fun p -> p.Def.occ
let get_occupation = fun p -> p.Def.occupation
let get_parents = fun a -> a.Def.parents
let get_pevents = fun p -> p.Def.pevents
let get_psources = fun p -> p.Def.psources
let get_public_name = fun p -> p.Def.public_name
let get_qualifiers = fun p -> p.Def.qualifiers
let get_related = fun p -> p.Def.related
let get_rparents = fun p -> p.Def.rparents
let get_sex = fun p -> p.Def.sex
let get_surname = fun p -> p.Def.surname
let get_surnames_aliases = fun p -> p.Def.surnames_aliases
let get_titles = fun p -> p.Def.titles

let gen_person_of_person = (fun p -> p)
let person_of_gen_person _base (p, a, u) = { p ; a ; u }

let get_children = fun d -> d.Def.children
let get_comment = fun f -> f.Def.comment
let get_divorce = fun f -> f.Def.divorce
let get_father = fun c -> Adef.father c
let get_fevents = fun f -> f.Def.fevents
let get_fsources = fun f -> f.Def.fsources
let get_marriage = fun f -> f.Def.marriage
let get_marriage_note = fun f -> f.Def.marriage_note
let get_marriage_place = fun f -> f.Def.marriage_place
let get_marriage_src = fun f -> f.Def.marriage_src
let get_mother = fun c -> Adef.mother c
let get_origin_file = fun f -> f.Def.origin_file
let get_parent_array = fun c -> Adef.parent_array c
let get_relation = fun f -> f.Def.relation
let get_witnesses = fun f -> f.Def.witnesses
let is_deleted_family = fun f -> f.Def.fam_index = Adef.ifam_of_int (-1)

let family_of_gen_family _base (f, c, d) = { f ; c ; d }

let poi (_, base) i = Array.get base.persons i
let foi (_, base) i = Array.get base.families i
let sou (_, base) i = Array.get base.strings i

let nb_of_persons (_, base) = base.persons.length
let nb_of_families (_, base) = base.families.length

let patch_person (_, base) i p =
  Array.set base.persons i { (Array.get base.persons i) with p = p }
let patch_ascend (_, base) i a =
  Array.set base.persons i { (Array.get base.persons i) with a = a }
let patch_union (_, base) i u =
  Array.set base.persons i { (Array.get base.persons i) with u = u }

let patch_family (_, base) i f =
  Array.set base.families i { (Array.get base.families i) with f = f }
let patch_descend (_, base) i d =
  Array.set base.families i { (Array.get base.families i) with d = d }
let patch_couple (_, base) i c =
  Array.set base.families i { (Array.get base.families i) with c = c }

(* FIXME: optimize this *)
let insert_string (_, base) s =
  let strings = base.strings in
  try index_of base.strings s
  with Not_found ->
    base.strings <- Array.append [| s |] base.string ;
    Array.fast_sort compare base.string ;
    index_of base.strings s

let commit_patches (bname, base) =
  let oc = open_out_bin bname in
  Marshal.to_channel oc base [ Marshal.No_sharing ; Marshal.Compat_32 ] ;
  close_out oc

(* FIXME *)
let commit_notes _ _ _ = ()
let is_patched_person _ _ = false
let patched_ascends _ = []
let patch_name _base _s _ip = () (* FIXME? *)
let patch_key _base _iper _string _string _int = ()
let delete_key _base _string _string _int = ()

let delete_family _base _ifam = failwith "TODO"
let is_deleted_family = fun f -> f.Def.fam_index = Adef.ifam_of_int (-1)

(* FIXME: Use the nominative stuff? *)
(* FIXME: Use quick normalized comparison? *)
let person_of_key ((base, _) as b) fn sn oc =
  try
    let fn = index_of base.strings fn in
    let sn = index_of base.strings sn in
    let len = Array.length base.persons in
    let rec loop i =
      if i = len then None
      else
        let p = Array.unsafe_get base.persons i in
        if p.first_name = fn && p.surname = sn && p.occ = occ
        then Some (poi b i)
        else loop (i + 1)
    in
    loop 0
  with _ -> None

let rev_find_all a fn =
  let len = Array.length a in
  let rec loop acc i =
    if i = len then acc
    else if fn (Array.unsafe_get a i) then loop (i :: acc) (i + 1)
    else loop acc (i + 1)
  in
  loop [] 0

let persons_of_name ((_, base) as b) n =
  rev_find_all base.persons (fun p -> sou b p.first_name ^ " " ^ sou b p.surname = n)
let persons_of_first_name ((_, base) as b) fn =
  { data = Array.of_list @@ rev_find_all base.persons (fun p -> sou b p.first_name = fn)
  ; cursor = 0 }
let persons_of_surname ((_, base) as b) sn =
  { data = Array.of_list @@ rev_find_all base.persons (fun p -> sou b p.surname = fn)
  ; cursor = 0 }

let spi_first _spi _str = failwith __LOC__
let spi_next _ _ _ = failwith __LOC__
let spi_find _ _ _ = failwith __LOC__

val base_visible_get : base -> (person -> bool) -> int -> bool
val base_visible_write : base -> unit
val base_particles : base -> string list
val base_strings_of_first_name : base -> string -> istr list
val base_strings_of_surname : base -> string -> istr list

let load_ascends_array _ = ()
let load_unions_array _ = ()
let load_couples_array _ = ()
let load_descends_array _ = ()
let load_strings_array _ = ()
let load_persons_array _ = ()
let load_families_array _ = ()
let clear_ascends_array _ = ()
let clear_unions_array _ = ()
let clear_couples_array _ = ()
let clear_descends_array _ = ()
let clear_strings_array _ = ()
let clear_persons_array _ = ()
let clear_families_array _ = ()

let base_notes_read _ _ = failwith __LOC__
let base_notes_read_first_line _ _ = failwith __LOC__
let base_notes_are_empty _ _ = failwith __LOC__
let base_notes_origin_file _ = failwith __LOC__
let base_notes_dir _ = failwith __LOC__
let base_wiznotes_dir _ = failwith __LOC__

(* TODO *)
let nobtit _ _ _ _ = []
let p_first_name base p = Mutil.nominative (sou base (get_first_name p))
let p_surname base p = Mutil.nominative (sou base (get_surname p))

(* TODO *)
let date_of_last_change _ = 0.

let husbands base gp =
  let p = poi base gp.key_index in
  Array.map
    (fun ifam ->
       let fam = foi base ifam in
       let husband = poi base (get_father fam) in
       let husband_surname = p_surname base husband in
       let husband_surnames_aliases =
         List.map (sou base) (get_surnames_aliases husband)
       in
       husband_surname, husband_surnames_aliases)
    (get_family p)

let father_titles_places base p nobtit =
  match get_parents (poi base p.key_index) with
  | Some ifam ->
    let fam = foi base ifam in
    let fath = poi base (get_father fam) in
    List.map (fun t -> sou base t.t_place) (nobtit fath)
  | None -> []

let gen_gen_person_misc_names base p nobtit nobtit_fun =
  let sou = sou base in
  Futil.gen_person_misc_names (sou p.first_name) (sou p.surname)
    (sou p.public_name) (List.map sou p.qualifiers) (List.map sou p.aliases)
    (List.map sou p.first_names_aliases) (List.map sou p.surnames_aliases)
    (List.map (Futil.map_title_strings sou) nobtit)
    (if p.sex = Female then Array.to_list (husbands base p) else [])
    (father_titles_places base p nobtit_fun)

let gen_person_misc_names base p nobtit =
  gen_gen_person_misc_names base p (nobtit p)
    (fun p -> nobtit (gen_person_of_person p))

let person_misc_names base p nobtit =
  gen_gen_person_misc_names base (gen_person_of_person p) (nobtit p) nobtit
