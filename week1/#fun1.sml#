fun sum_list(sx: int list) =
  if null sx
  then 0
  else hd(sx) + sum_list(tl(sx))

fun countdown(x: int) =
  if x < 1
  then []
  else x :: countdown(x-1)

fun append(x: int list, y: int list) =
  if null x
  then y
  else hd(x) :: append(tl(x), y)

fun sum_pair_list(x: (int * int) list) =
  if null x
  then 0
  else #1 (hd(x)) + #2 (hd(x)) + sum_pair_list(tl(x))

fun firsts(x: (int * int) list) =
  if null x
  then []
  else #1 (hd(x)) :: firsts(tl(x))

fun seconds(x: (int * int) list) =
  if null x
  then []
  else #2 (hd(x)) :: seconds(tl(x))

fun sum_pair_list2(x: (int * int) list) =
  sum_list(firsts(x)) + sum_list(seconds(x))
