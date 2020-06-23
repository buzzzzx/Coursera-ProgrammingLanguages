datatype suit = Club | Heart | Diamend | Spade
datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

type name_record = { 
                     stu_id : int option,
                     first: string,
                     middle: string option,
                     last: string
                   }

val c1 : card = (Heart, Num 2)
val c2 : suit * rank = (Spade, Ace)
val c3 = (Club, King)

fun is_spade_ace1(c : card) = 
    (#1 c = Spade) andalso (#2 c = Ace)

fun is_spade_ace2 c = 
    case c of 
        (Spade, Ace) => true