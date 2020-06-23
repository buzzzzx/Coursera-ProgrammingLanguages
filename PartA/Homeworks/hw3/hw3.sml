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

(* Q1 - only_capitals *)
(* string list -> string list *)
val only_capitals = List.filter (fn str => Char.isUpper (String.sub (str, 0)))


(* Q2 - longest_string1 *)
(* string list -> string *)
val longest_string1 = List.foldl (fn (x, y) => if (String.size x) > (String.size y) then x else y) ""


(* Q3 - longest_string2 *)
(* string list -> string *)
val longest_string2 = List.foldl (fn (x, y) => if (String.size x) >= (String.size y) then x else y) ""

(* Q4 - longest_string_helper, longest_string3, longest_string4 *)

(* longest_string_helper : (int * int -> bool) -> string list -> string *)
fun longest_string_helper f str_lst = 
    List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) "" str_lst

(* longest_string3 : string list -> string *)
val longest_string3 = longest_string_helper (fn (x, y) => x > y)

(* longest_string4 : string list -> string *)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)


(* Q5 - longest_capitalized *)
(* string list -> string *)
val longest_capitalized = longest_string1 o only_capitals


(* Q6 - rev_string *)
(* string -> string *)
val rev_string = String.implode o List.rev o String.explode


(* Q7 - first_answer *)
fun first_answer f lst = 
    case lst of 
        [] => raise NoAnswer
      | x::xs => case (f x) of 
                    NONE => first_answer f xs
                  | SOME v => v


(* Q8 - all_answers *)
fun all_answers f lst = 
    let fun loop(xs, acc) = 
            case xs of
                [] => SOME acc
              | x::xs' => case (f x) of
                            NONE => NONE
                          | SOME v => loop (xs', acc @ v)
    in 
        loop (lst, [])
    end


(* Q9 - count_wildcards, count_wild_and_variable_lengths, count_some_var *)

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn x => if s = x then 1 else 0) p


(* Q10 - check_pat *)
fun check_pat pat = 
    let
        fun make_list p =
            case p of
                Wildcard          => []
              | Variable x        => [x]
              | TupleP ps         => List.foldl (fn (x,acc) => (make_list x) @ acc) [] ps
              | ConstructorP(_,ps) => make_list ps
              | _                 => []

        fun check_duplicated str_lst = 
            case str_lst of
                [] => true 
              | s::xs' => (not (List.exists (fn x => x = s) xs')) andalso check_duplicated xs' 
    in
        check_duplicated (make_list pat)
    end


(* Q11 - match *)
fun match (va, pat) = 
    case (va, pat) of
        (_, Wildcard) => SOME []
      | (_, Variable (s)) => SOME [(s, va)]
      | (Unit, UnitP) => SOME []
      | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
      | (Tuple (vs), TupleP (ps)) => if List.length vs = List.length ps 
                                     then all_answers match (ListPair.zip (vs, ps)) 
                                     else NONE
      | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match (v, p) else NONE
      | _ => NONE


(* Q12 - first_match *)
fun first_match va pat_lst = SOME (first_answer (fn pat => match (va, pat)) pat_lst)
                             handle NoAnswer => NONE
