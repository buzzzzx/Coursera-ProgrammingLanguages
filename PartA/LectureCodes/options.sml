(* int list -> int option *)
fun max1(xs : int list) = 
    if null xs
    then NONE
    else
        let
            val tl_ans = max1(tl xs)
        in
            if isSome tl_ans andalso hd xs < valOf(tl_ans)
            then tl_ans
            else SOME(hd xs)
        end

fun max2(xs : int list) = 
    if null xs
    then NONE
    else 
    
        let
            fun max_none_empty(xs : int list) =
                if null (tl xs)
                then hd xs
                else 
                    let
                        val tl_ans = max_none_empty(tl xs)
                    in 
                        if hd xs > tl_ans
                        then hd xs
                        else tl_ans
                    end 
        in 
            SOME (max_none_empty xs)

        end 

