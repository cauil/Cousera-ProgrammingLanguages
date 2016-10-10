fun is_date(p: (int*int*int)) =
  #1 p > 0 andalso #2 p > 0 andalso #2 p < 13 andalso #3 p > 0 andalso #3 p < 32

(*return x > y*)
fun g(x: int, y: int) =
  x > y

(*return x < y*)
fun s(x: int, y: int) =
  x < y

(*return x = y*)
fun e(x: int, y: int) =
  x = y

fun is_older(xp: (int*int*int), yp:(int*int*int)) =
  if not (is_date(xp) andalso is_date(yp))
  then false
  else
    s(#1 xp, #1 yp)
    orelse
    (e(#1 xp, #1 yp) andalso s(#2 xp, #2 yp))
    orelse
    (e(#1 xp, #1 yp) andalso e(#2 xp, #2 yp) andalso s(#3 xp,#3 yp))

fun number_in_month(ds: (int*int*int) list, m: int) =
  if null ds
  then 0
  else
    let
      val next = number_in_month(tl(ds), m)
    in
      if e(#2 (hd ds), m) andalso is_date(hd ds)
      then
        1 + next
      else
        next
    end

fun number_in_months(ds: (int * int * int) list, ms: int list) =
    if null ms
    then 0
    else
      number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month(ds: (int*int*int) list, m: int) =
  if null ds
  then []
  else
    let val next = dates_in_month(tl ds, m)
    in
      if e(#2 (hd ds), m) andalso is_date(hd ds)
      then (hd ds) :: next
      else next
    end

fun dates_in_months(ds: (int * int * int) list, ms: int list) =
  if null ms
  then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth(ss: string list, n: int) =
  if n = 1
  then hd(ss)
  else
    get_nth(tl ss, n-1)


fun date_to_string(d: (int*int*int)) =
  let
    val st_arr = ["January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "Norvember", "December"];
  in
    if not(is_date(d))
    then "not correct date"
    else
      get_nth(st_arr, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

fun number_before_reaching_sum(sum: int, sl: int list) =
  if sum <= hd sl
  then 0
  else 1 + number_before_reaching_sum(sum - hd sl, tl sl)

fun what_month(d: int) =
  let
    val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    1 + number_before_reaching_sum(d, month_lengths)
  end

fun month_range(dx: int, dy: int) =
  if dx > dy
  then []
  else what_month(dx) :: month_range(dx + 1, dy)

fun oldest(ds: (int*int*int) list) =
  if null ds
    then NONE
  else if null (tl(ds))
    then SOME (hd(ds))
  else
    let
      val old = hd(ds)
      val next_old = 
        if isSome (oldest(tl(ds)))
        then valOf (oldest(tl(ds)))
        else old
    in
      if is_older(old, next_old)
      then SOME old
      else SOME next_old
    end



