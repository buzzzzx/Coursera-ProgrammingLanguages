exception DivException

fun my_div (x, y) =
    if y = 0
    then raise DivException
    else x div y

val x = my_div(1, 0) handle DivException => 1