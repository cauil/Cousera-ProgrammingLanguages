(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let
        val r = g f1 f2
    in
        case p of
            Wildcard          => f1 ()
          | Variable x        => f2 x
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | ConstructorP(_,p) => r p
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
val only_capitals = 
  List.filter (fn str => Char.isUpper(String.sub(str, 0)))

(* 2 *)
val longest_string1 =
  foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) ""

(* 3 *)
val longest_string2 =
  foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) ""

(* 4 *)
fun longest_string_helper f =
  foldl (fn(s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) ""

val longest_string3 = longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn (s1, s2) => s1 >= s2)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | head::tail => case f head of
                      SOME v => v
                    | _ => first_answer f tail

(* 8 *)
fun all_answers f xs =
  let fun helper(xs, acc) =
    case xs of
         [] => SOME acc
       | head::tail => case f head of
                        NONE => NONE
                      | SOME v => helper(tail, acc @ v)
  in
    helper(xs, [])
  end

(* 9-a *)
val count_wildcards = g (fn () => 1) (fn str => 0)
(* 9-a recursive ver *)
fun count_wildcards1 p =
  let 
    fun helper (p, acc) =
      case p of
           Wildcard => acc + 1
         | TupleP ps => List.foldl (fn (foop, i) => (helper (foop, 0)) + i) acc ps
         | ConstructorP(_, foop) => helper(foop, acc)
         | _ => acc
  in
    helper(p, 0)
  end

(* 9-b *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
(* 9-b recursive ver *)
fun count_wild_and_variable_lengths1 p =
  let 
    fun helper (p, acc) =
      case p of
           Wildcard => acc + 1
         | Variable x => acc + String.size x
         | TupleP ps => List.foldl (fn (foop, i) => (helper (foop, 0)) + i) acc ps
         | ConstructorP(_, foop) => helper(foop, acc)
         | _ => acc
  in
    helper(p, 0)
  end

(* 9-c *)
fun count_some_var (str, ptn) =
    g (fn () => 0) (fn s => if s = str then 1 else 0) ptn
(* 9-c recursive ver *)
fun count_some_var1 (str, p) =
  let 
    fun helper (p, acc) =
      case p of
         Variable x => if x=str then acc+1 else acc
         | TupleP ps => List.foldl (fn (foop, i) => (helper (foop, 0)) + i) acc ps
         | ConstructorP(_, foop) => helper(foop, acc)
         | _ => acc
  in
    helper(p, 0)
  end

(* 10 *)
fun check_pat p =
  let
    fun helper sl = case sl of
                          [] => true
                        | head::tail => (not (List.exists (fn s => s = head) tail))
                                        andalso (helper tail)
    fun getl p =
      case p of
         Variable x => [x]
         | TupleP ps => List.foldl (fn (foop, i) => i @ (getl foop)) [] ps
         | ConstructorP(_, foop) => getl foop
         | _ => []
  in
    helper(getl p)
  end

(* 11 *)
fun match (va, pa) =
  case (va, pa) of
       (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const i, ConstP j) => if i = j then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if length vs = length ps
                                then all_answers match (ListPair.zip(vs, ps))
                                else NONE
     | (Constructor(s2, v), ConstructorP(s1, p)) => if s1=s2
                                                    then match(v, p)
                                                    else NONE
     | (_, _) => NONE

(* 12 *)
fun first_match va plst =
    SOME (first_answer (fn x => match(va, x)) plst) handle NoAnswer => NONE
