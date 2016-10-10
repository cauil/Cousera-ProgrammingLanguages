(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1-a *)
fun all_except_option(s, sl) =
        case sl of
             [] => NONE
           | x::xs => if same_string(s, x)
                      then SOME xs
                      else case all_except_option(s, xs) of
                                NONE => NONE
                              | SOME lxs => SOME (x::lxs)

(* 1-b *)
fun get_substitutions1(sll, s) =
    case sll of
         [] => []
       | sl::xsl => case all_except_option(s, sl) of
                         NONE => get_substitutions1(xsl, s)
                       | SOME l => l @ get_substitutions1(xsl, s)

(* 1-c *)
fun get_substitutions2(sll, s) =
    let fun helper(sll, accl) =
        case sll of
             [] => accl
             | sl::xsl => case all_except_option(s, sl) of
                              NONE => helper(xsl, accl)
                              | SOME l => helper(xsl, accl @ l)
    in
      helper(sll, [])
    end

(* 1-d *)
fun similar_names(xss, {first=x, middle=y, last=z}) =
    let val fool = x::get_substitutions1(xss, x)
        fun helper(sl, accl) = 
            case sl of
               [] => accl
               | m::ml => helper(ml, accl @ [{first=m, middle=y, last=z}])
    in
        helper(fool, [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2-a *)
fun card_color(c: card) =
    case c of
         (Spades, _) => Black
       | (Clubs, _) => Black
       | (Diamonds, _) => Red
       | (Hearts, _) => Red

(* 2-b *)
fun card_value(c: card) =
    case c of
         (_, Num i) => i
       | (_, Ace) => 11
       | _ => 10

(* 2-c *)
fun remove_card(cs: card list, c: card, IllegalMove) =
    case cs of
         [] => raise IllegalMove
       | m::ms => if m = c
                  then ms
                  else m::remove_card(ms, c, IllegalMove)

(* 2-d *)
fun all_same_color(cs) =
    case cs of
         hd::next::tail => card_color(hd) = card_color(next) andalso all_same_color(next::tail)
       | _ => true

(* 2-e *)
fun sum_cards(cs) =
    let fun helper(cs, css) =
        case cs of
             [] => css
           | m::ms => helper(ms, css+card_value(m))
    in
      helper(cs, 0)
    end

(* 2-f *)
fun score(cs, i) =
  (case sum_cards(cs) > i of
        true => (sum_cards(cs)-i)*3
      | _ => (i-sum_cards(cs)))
  div
  (case all_same_color(cs) of
        true => 2
      | _ => 1)

(* 2-g *)
fun officiate(cards, plays, goal) =
    let fun helper(cards, plays, held) =
        case plays of
             [] => score(held, goal)
           | (Discard c)::plays_left => helper(cards,plays_left,remove_card(held, c, IllegalMove))
           | Draw::plays_left => case cards of
                                       [] => score(held, goal)
                                     | x::xs => case sum_cards(x::held)>=goal of
                                                     true => score(x::held, goal)
                                                   | false => helper(xs,plays_left, x::held)
    in
        helper(cards, plays, [])
    end

