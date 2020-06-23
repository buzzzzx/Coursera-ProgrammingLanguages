(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* string * string list -> string list option *)
fun all_except_option (input_string, string_lst) = 
    case string_lst of 
        [] => NONE
      | x::xs => if same_string(input_string, x)
                 then SOME xs
                 else
                    case all_except_option(input_string, xs) of
                        NONE => NONE
                      | SOME y => SOME (x::y)
    

(* string list list * string -> string list *)
fun get_substitutions1(substitutions, s) = 
    case substitutions of 
        [] => []
      | x::xs => case all_except_option(s, x) of
                    NONE => get_substitutions1(xs, s)
                  | SOME y => y @ get_substitutions1(xs, s)


(* string list list * string -> string list *)
fun get_substitutions2(substitutions, s) = 
    let 
        fun helper(lst, acc) =
            case lst of
                [] => acc
              | x::xs => case all_except_option(s, x) of
                            NONE => helper(xs, acc)
                          | SOME y => helper(xs, acc@y)
    in 
        helper(substitutions, [])
    end


(*  string list list * {first:string,middle:string,last:string} -> {first:string,middle:string,last:string} list *)
fun similar_names(substitutions, fullname) = 
    let 
        fun substitute_helper(first_lst, middle_name, last_name) = 
            case first_lst of
                [] => []
              | x::xs => {first=x, middle=middle_name, last=last_name} :: substitute_helper(xs, middle_name, last_name)
    in 
        case fullname of
            {first, middle, last} => substitute_helper(first::get_substitutions2(substitutions, first), middle, last)
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
(* suit * rank -> color *)
fun card_color(suit, rank) = 
    case suit of
        Spades => Black
      | Clubs => Black
      | _ => Red

(* suit * rank -> int *)
fun card_value(suit, rank) = 
    case rank of 
        Num i => i
      | Ace => 11
      | _ => 10

(* (suit * rank) list * (suit * rank) * exception -> (suit * rank) list *)
fun remove_card(cs, c, e) =
    case cs of 
        [] => raise e
      | x::xs => if x=c then xs else x::remove_card(xs, c, e)


(* (suit * rank) list -> bool *)
fun all_same_color(cs) = 
    case cs of 
        [] => true
      | _::[] => true
      | c1::c2::rest => card_color(c1) = card_color(c2) andalso all_same_color(c2::rest)


(* (suit * rank) list -> int *)
fun sum_cards(cs) = 
    let
        fun calc_helper(cards, acc) = 
            case cards of 
                [] => acc
              | x::xs => calc_helper(xs, acc+card_value(x))
    in 
        calc_helper(cs, 0)
    end


(* (suit * rank) list * int -> int *)
fun score(held_cs, goal) = 
    let 
        val sum_val = sum_cards(held_cs)
        val pre_score = if sum_val > goal then 3 * (sum_val - goal) else goal - sum_val
    in 
        if all_same_color(held_cs) then pre_score div 2 else pre_score
    end


(* (suit * rank) list * move list * int -> int *)
fun officiate(card_lst, move_lst, goal) =
    let 
        fun play(cs, ms, hs) = 
            case (cs, ms) of
                (_, []) => score(hs, goal) (* no moves *)
              | ([], _) => score(hs, goal) (* no cards *)
              | (cs, (Discard c)::xs) => play(cs, xs, remove_card(hs, c, IllegalMove))
              | (c::cs, Draw::xs) => if sum_cards(c::hs) > goal 
                                     then score(c::hs, goal)
                                     else play(cs, xs, c::hs)
    in
        play(card_lst, move_lst, [])
    end

    
