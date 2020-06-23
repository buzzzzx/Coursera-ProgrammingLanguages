(* This is the solution for hw1 *)
(* 02.25.2020 Remie Choo *)


(* Function 1 - is_older *)
(* (int * int * int) * (int * int * int) -> bool *)
fun is_older(fd : int * int * int, sd : int * int * int) = 
    if (#1 fd < #1 sd) orelse ((#1 fd = #1 sd) andalso (#2 fd < #2 sd)) orelse ((#1 fd = #1 sd) andalso (#2 fd = #2 sd) andalso (#3 fd < #3 sd))
    then true
    else false


(* Function 2 - number_in_month *)
(* (int * int * int) list * int -> int *)
fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else 
        let 
            val tl_ans = number_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then 1 + tl_ans
            else tl_ans
        end


(* Function 3 - number_in_months *)
(* (int * int * int) list * (int list) -> int *)
fun number_in_months(dates : (int * int * int) list, months : int list) = 
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
        

(* Function 4 - dates_in_month *)
(* (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month(dates : (int * int * int) list, month : int) = 
    if null dates
    then []
    else
        let val tl_ans = dates_in_month(tl dates, month)
        in 
            if #2 (hd dates) = month
            then hd dates :: tl_ans
            else tl_ans
        end


(* Function 5 - dates_in_months *)
(* (int * int * int) list * (int list) -> (int * int * int) list *)
fun dates_in_months(dates : (int * int * int) list, months : int list) = 
    if null months
    then []
    else 
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* Function 6 - get_nth *)
(* string list * int -> string *)
fun get_nth(words : string list, n : int) = 
    if n = 1
    then hd words
    else get_nth(tl words, n-1)


(* Function 7 - date_to_string *)
(* (int * int * int) -> string *)
fun date_to_string(date : int * int * int) = 
    let 
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val year = Int.toString(#1 date)
        val month = get_nth(months, #2 date)
        val day = Int.toString(#3 date)
    in 
        month ^ " " ^ day ^ ", " ^ year
    end


(* Function 8 - number_before_reaching_sum *)
(* int * int list -> int *)
fun number_before_reaching_sum(sum : int, numbers : int list) = 
    if hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)


(* Function 9 - what_month *)
(* int -> int *)
fun what_month(day : int) = 
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        number_before_reaching_sum(day, days_in_months) + 1
    end 

(* Function 10 - month_range *)
(* int * int -> int list *)
fun month_range(day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)


(* Function 11 - oldest *)
(* (int * int * int) list -> (int * int * int) option *)
fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else 
        let 
            fun oldest_none_empty(dates : (int * int * int) list) = 
                if null (tl dates)
                then hd dates
                else 
                    let 
                        val tl_ans = oldest_none_empty(tl dates)
                    in 
                        if is_older(hd dates, tl_ans)
                        then hd dates
                        else tl_ans
                    end
        in 
            SOME (oldest_none_empty(dates))
        end


(* Support function - remove_duplicates *)
(* int list -> int list *)
fun remove_duplicates(xs : int list) = 
    if null xs
    then []
    else if null (tl xs)
    then xs
    else 
        let val tl_ans = remove_duplicates(tl xs)
        in 
            if hd xs = hd (tl xs)
            then tl_ans
            else hd xs :: tl_ans
        end

(* Support function - sort_list *)
(* int list -> int list *)
fun sort_list(xs : int list) =
    if null xs
    then []
    else
        let
            fun compare(x : int, y : int) = x > y
        in
            ListMergeSort.sort(compare) xs
        end



(* Challenge Problem 1.1 - number_in_months_challenge *)
(* (int * int * int) list * (int list) -> int *)
fun number_in_months_challenge(dates : (int * int * int) list, months : int list) = 
    number_in_months(dates, remove_duplicates(sort_list(months)))



(* Challenge Problem 1.2 - dates_in_months_challenge *)
(* (int * int * int) list * (int list) -> (int * int * int) list *)
fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) = 
    dates_in_months(dates, remove_duplicates(sort_list(months)))


(* Challenge Problem 2 - reasonable_date *)
(* int * int * int -> bool *)
fun reasonable_date(date : int * int * int) = 
    let 
        val year = #1 date
        val month = #2 date
        val day = #3 date

        val leap_year = ((year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0)))
        val day_in_feb = if leap_year then 29 else 28
        val day_lst = [31, day_in_feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        fun get_day(lst : int list, month : int) =  (* int list -> int *)
            if month = 1
            then hd lst
            else get_day(tl lst, month-1)
    in 
        year > 0 andalso month >= 1 andalso month <=12 andalso day >= 1 andalso day <= get_day(day_lst, month)
    end