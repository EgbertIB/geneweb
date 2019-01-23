open Config
open Def
open Gwdb

val code_dmy : config -> dmy -> string
val string_of_ondate : config -> date -> string
val string_of_ondate_aux : config -> date -> string
val string_of_date : config -> date -> string
val string_of_date_sep : config -> string -> date -> string
val string_slash_of_date : config -> date -> string
val string_of_age : config -> dmy -> string
val prec_year_text : config -> dmy -> string
val prec_text : config -> dmy -> string
val day_text : dmy -> string
val month_text : dmy -> string
val year_text : dmy -> string
val short_dates_text : config -> base -> person -> string
val short_marriage_date_text : config -> base -> family -> person -> person -> string
val print_dates : config -> base -> person -> unit

(* return the day of the week given the date as parameter *)
val get_wday : config -> date -> string
val death_symbol : config -> string
val string_of_dmy : config -> dmy -> string
val string_of_on_french_dmy : config -> dmy -> string
val string_of_on_hebrew_dmy : config -> dmy -> string
val string_of_prec_dmy : config -> string -> string -> dmy -> string
val gregorian_precision : config -> dmy -> string
val french_month : config -> int -> string
val code_french_year : config -> int -> string
val code_hebrew_date : config -> int -> int -> int -> string
