(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

(** Coerce [dmy2] into [dmy] in order to reuse functions. *)
val dmy_of_dmy2 : dmy2 -> dmy

(** [compare_dmy dmy1 dmy2]
    Return [-1] if [dmy1] represents a date before [dmy2],
    [1] if it represents a date after [dmy2].
    Otherwise, return [0].
 *)
val compare_dmy : dmy -> dmy -> int

(** [compare_date d1 d2] *)
val compare_date : date -> date -> int

(** [time_elapsed d1 d2] *)
val time_elapsed : dmy -> dmy -> dmy

(** Extract a date from a death *)
val date_of_death : death -> date option

(** [get_birth_death p]
    Return [(birth_date, death_date, approx)], which are
    birth and dath dates of [p], or an apporoximation of it based on
    other events. If date are determined from other events, [approx]
    is [true].
*)
val get_birth_death_date : person -> date option * date option * bool

(** [day_after d] is the day after [d]. *)
val day_after : dmy -> dmy

(** [leap_year y] test if [y] is a leap year (366 days). *)
val leap_year : int -> bool

(** [nb_days_in_month m] is the number of days in a month (in a not leap year). *)
val nb_days_in_month : int -> int -> int
