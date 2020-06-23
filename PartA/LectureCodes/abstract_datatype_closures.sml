datatype set = S of {
                        insert : int -> set,
                        member : int -> bool,
                        size : unit -> int
                    }

val empty_set = 
    let 
        fun make_set xs = 
            let
                fun contain_item i = List.exists (fn j => i = j) xs            
            in 
                S {
                    insert = fn i => if contain_item i
                                     then make_set xs
                                     else make_set (i::xs),
                    member = contain_item,
                    size = fn () => List.length xs
                  }
            end
    in 
        make_set []
    end

fun use_sets () =
    let 
        val S s1 = empty_set
        val S s2 = (#insert s1) 34
        val S s3 = (#insert s1) 34
        val S s4 = (#insert s1) 19
    in 
        if (#member s4) 10
        then 99
        else if (#member s4) 19
        then (#size s3)() + 17
        else 0
    end
