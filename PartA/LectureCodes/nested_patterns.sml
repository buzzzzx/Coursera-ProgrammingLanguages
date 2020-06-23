(* Examples *)

exception ListLengthMismatch

(* ([], [], []) -> [(), (), ()] *)
fun zip3 list_triple = 
    case list_triple of
        ([], [], []) => []
      | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3) :: zip3(tl1, tl2, tl3)
      | _ => raise ListLengthMismatch

(* [(), (), ()] -> ([], [], []) *)
fun unzip3 triple_list = 
    case triple_list of
        [] => ([], [], [])
      | (a, b, c) :: tl => let val (l1, l2, l3) = unzip3(tl)
                           in 
                               (a::l1, b::l2, c::l3)
                           end

val x = zip3 ([1, 2, 3], [4, 5, 6], [7, 8, 9])
val y = unzip3 [(1, 2, 3), (4, 5, 6), (7, 8, 9)]

(* int list -> bool *)
fun nondecreasing lst = 
    case lst of 
        [] => true 
      | _::[] =>true
      | head::neck::rest => head <= neck andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

(* int * int -> sng *)
fun multi_sign (x, y) = 
    let fun sgn_helper m = if m < 0 then N else if m > 0 then P else Z
    in 
        case (sgn_helper x, sgn_helper y) of
            (_, Z) => Z
          | (Z, _) => Z
          | (P, P) => P
          | (N, N) => P
          | _ => N
    end