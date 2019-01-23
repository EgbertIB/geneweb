(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

let dmy_of_dmy2 dmy2 =
  { day = dmy2.day2
  ; month = dmy2.month2
  ; year = dmy2.year2
  ; prec = Sure
  ; delta = dmy2.delta2 }

let rec compare_dmy dmy1 dmy2 =
  match Pervasives.compare dmy1.year dmy2.year with
  | 0 -> begin match Pervasives.compare dmy1.month dmy2.month with
      | 0 -> begin match Pervasives.compare dmy1.day dmy2.day with
          | 0 -> begin match dmy1.prec, dmy2.prec with
              | (Sure, Sure) | (About, About) | (Maybe, Maybe) | (Before, Before) | (After, After) -> 0
              | (OrYear d1, OrYear d2) | (YearInt d1, YearInt d2) -> compare_dmy (dmy_of_dmy2 d1) (dmy_of_dmy2 d2)
              | ((Sure|About|Maybe|Before), After) | (Before, (Sure|About|Maybe)) -> -1
              | (After, (Sure|About|Maybe|Before)) | ((Sure|About|Maybe), Before) -> 1
              | _ -> 0
            end
          | x -> x
        end
      | x -> x
    end
  | x -> x

let compare_date d1 d2 =
  match d1, d2 with
  | Dgreg (dmy1, _), Dgreg (dmy2, _) -> compare_dmy dmy1 dmy2
  | Dgreg (_, _), Dtext _ -> 1
  | Dtext _, Dgreg (_, _) -> -1
  | Dtext _, Dtext _ -> 0

let leap_year a = if a mod 100 = 0 then a / 100 mod 4 = 0 else a mod 4 = 0

let nb_days_in_month =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a ->
    if m = 2 && leap_year a then 29
    else if m >= 1 && m <= 12 then tb.(m-1)
    else 0

let time_elapsed d1 d2 =
  let prec =
    if d1.prec = d2.prec then d1.prec
    else match d1.prec, d2.prec with
      | (Sure, p) | (p, Sure) -> p
      | _ -> Maybe
  in
  match d1 with
  | {day = 0; month = 0; year = a1} ->
    {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
    begin match d2 with
        {day = 0; month = 0; year = a2} ->
        {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
        let r = 0 in
        let (month, r) =
          if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {month = m2; year = a2} ->
        let r = 0 in
        let (month, r) =
          if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
        in
        let year = a2 - a1 - r in
        {day = 0; month = month; year = year; prec = prec; delta = 0}
    end
  | {day = j1; month = m1; year = a1} ->
    match d2 with
      {day = 0; month = 0; year = a2} ->
      {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
    | {day = 0; month = m2; year = a2} ->
      let r = 0 in
      let (month, r) =
        if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
      in
      let year = a2 - a1 - r in
      {day = 0; month = month; year = year; prec = prec; delta = 0}
    | {day = j2; month = m2; year = a2} ->
      let (day, r) =
        if j1 <= j2 then j2 - j1, 0
        else j2 - j1 + nb_days_in_month m1 a1, 1
      in
      let (month, r) =
        if m1 + r <= m2 then m2 - m1 - r, 0 else m2 - m1 - r + 12, 1
      in
      let year = a2 - a1 - r in
      {day = day; month = month; year = year; prec = prec; delta = 0}

let date_of_death = function
  | Death (_, cd) -> Some (Adef.date_of_cdate cd)
  | _ -> None

let get_birth_death_date p =
  let (birth_date, approx) =
    match Adef.od_of_cdate (get_birth p) with
    | None -> Adef.od_of_cdate (get_baptism p), true
    | x -> x, false
  in
  let (death_date, approx) =
    match date_of_death (get_death p) with
    | Some d -> Some d, approx
    | _ ->
      match get_burial p with
      | Buried cd -> Adef.od_of_cdate cd, true
      | Cremated cd -> Adef.od_of_cdate cd, true
      | _ -> None, approx
  in
  birth_date, death_date, approx

let day_after d =
  let (day, r) =
    if d.day >= nb_days_in_month d.month d.year then 1, 1
    else succ d.day, 0
  in
  let (month, r) = if d.month + r > 12 then 1, 1 else d.month + r, 0 in
  let year = d.year + r in
  { day = day
  ; month = month
  ; year = year
  ; prec = d.prec
  ; delta = d.delta
  }
