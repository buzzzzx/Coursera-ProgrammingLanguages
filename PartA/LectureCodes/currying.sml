val sorted1 = fn x => fn y => fn z => z >= y andalso y >= x

fun sorted2 x = fn y => fn z => z >= y andalso y >= x

fun sorted3 x y z = z >= y andalso y >= x

val ans = sorted2 1 2 3

fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y 

fun curry_trans_argu f x y = f y x

fun example1 i s = Int.toString(i) ^ s

val e1 = curry_trans_argu example1 "a"