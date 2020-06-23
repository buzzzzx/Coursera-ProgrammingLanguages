datatype my_type = TwoInts of int * int
                 | Str of string
                 | Jedi

fun f x = 
    case x of
          Jedi => "Give you Saber"
        | Str s => "Hi" ^ s
        | TwoInts (i1, i2) => "TwoInts"

