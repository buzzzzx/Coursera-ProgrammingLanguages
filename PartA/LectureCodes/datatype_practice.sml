datatype suit = Heart | Spade | Diamend | Club

datatype rank = Jack | Queen | King | Ace | Num of int=

datatype exp = Constant of int
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e = 
    case e of 
        Constant i => i
      | Negate e1 => ~ (eval e1)
      | Add (e1, e2) => (eval e1) + (eval e2)
      | Multiply (e1, e2) => (eval e1) * (eval e2)

val example1 = Add (Constant 7, Negate (Constant 4))
val ans1 = eval example1

fun max_constant e = 
    case e of 
        Constant i => i
      | Negate e1 => max_constant e1
      | Add (e1, e2) => Int.max(max_constant e1, max_constant e2)
      | Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)

val example2 = Add (Constant 7, Negate (Constant 4))
val ans2 = max_constant example2 = 7