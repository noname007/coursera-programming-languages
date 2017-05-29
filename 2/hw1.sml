
(* 1 *)
fun is_older ( first:(int * int * int), second:(int * int * int)) =
  let val y1 = #1 first;
      val y2 = #1 second;
  in
      if y1  < y2 then true
      else if y1 > y2 then false
      else
	  let val m1 =  #2 first
	      val m2 =  #2 second
	  in
	      if m1 < m2 then true
	      else
		  if m1 > m2 then false
		  else
		      let val d1 = #3 first	      
			  val d2 = #3 second
		      in
			  if d1 < d2 then true
			  else
			      false
		  end
		      
	  end
  end;

(* 2 *)
fun number_in_month(date_list: (int * int * int) list, month: int ) =
  if null date_list then 0
  else
      let
	  val m = #2(hd date_list)
	  val c = number_in_month(tl date_list, month)
	 
      in
	  if m = month then
	      1 + c
	  else
	      c
	      
      end;
(* 3 *)
fun number_in_months(date_list: (int * int * int) list, month_list: int list) =
  if null month_list  then 0
  else
      number_in_month(date_list, hd(month_list) ) + number_in_months(date_list, tl(month_list) );

fun dates_in_month(date_list: (int * int * int) list , month: int) =
  if null date_list then []
  else
      let val d  = (hd date_list)
	  val date_list =  dates_in_month(tl date_list, month)
      in
	  if #2(d) = month then d :: date_list
	  else date_list
      end

fun dates_in_months(date_list: (int * int * int) list, month_list: int list) =
  if null month_list then []
  else
      dates_in_month(date_list, hd month_list) @ dates_in_months(date_list, tl month_list);

fun get_nth(list: string list, nth: int) =
  if nth = 1 then hd list
  else
      get_nth(tl list, nth - 1);

fun date_to_string(date:(int * int *int)) =
  let val month_words = ["January", "February", "March", "April",
			 "May", "June", "July", "August",
			 "September", "October", "November", "December"];
      val m = #2 date;
  in
      get_nth(month_words, m) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end


(* 8 *)
fun number_before_reaching_sum(sum: int, list: int list) =
  let fun calc_nth(sum: int, list: int list, nth: int) =
	let
	    val h = hd list;
	in
	    if sum <= h then nth
	    else
		calc_nth(sum - h, tl list, nth + 1)
	end
  in
      calc_nth(sum, list, 0)
  end;

(* 9 *)
fun what_month(day: int) =
  let val  days_per_month = [31, 28, 31, 30,
			     31, 30, 31, 31,
			     30, 31, 30, 30];
  in
      number_before_reaching_sum(day, days_per_month) + 1
  end;

(* 10 *)
fun month_range(day1:int, day2:int) =
  if day1 > day2 then []
  else
      what_month(day1) :: month_range(day1 + 1, day2);


(* 11 *)

fun oldest(date_list: (int * int * int) list) =
  if null date_list then NONE
  else
      let fun calc_older_nonempty(date_list: (int * int * int) list) =
	    let val head = hd date_list
	    in
		if null(tl date_list) then
		    head
		else
		    let val older = calc_older_nonempty(tl date_list);
		    in
			if is_older(head, older) then
			    head
			else
			    older
		    end
	    end

      in
	  SOME(calc_older_nonempty date_list)
      end;

(* 12 *)

(* urgly implemention *)
fun remove_duplicate(list: int list) =
  let fun not_in(v: int, list:int list) =
	if null list then true
	else if v = hd list then false
	else not_in(v, tl list)
  in
      if null list then []
      else
	  let val no_dup = remove_duplicate(tl list);
	      val v = hd list
	  in
	      if not_in(v,no_dup) then v::no_dup
	      else no_dup
	  end
  end

(* real used in fact *)
fun remove_duplicate2(list: int list) =
  if null list then []
  else let
      fun rm(v: int, from: int list) =
	if null from then []
	else let
	    val  cmp_num  = hd list
	    val  rmed_list = rm(v, tl list)
	in
	    if v = cmp_num then rmed_list
	    else cmp_num :: rmed_list
	end
		 
      val h = hd list;
  in
      rm(h, tl list)
  end;

fun number_in_months_challenge(date_list: (int * int * int) list, month_list: int list) =
  number_in_months(date_list, remove_duplicate2(month_list))
		  
fun dates_in_months_challenge(date_list: (int * int * int) list, month_list: int list) =
  dates_in_months(date_list, remove_duplicate2(month_list))

(* 13 *)
fun reasonable_date(date: (int * int * int)) =
  let val year = #1 date;
      val month = #2  date;
      val day = #3 date;
  in
      if year <= 0 then false
      else if month <  1 orelse  month > 12 then  false
      else let
	  fun is_leap(year: int) =
	    if year mod 400 = 0 then true
	    else if year mod 4 = 0 andalso year mod 100 <> 0 then true
	    else false
	  fun get_days_of_month(month: int) =
	    let val days_per_month = [31, 28, 31, 30,
		       31, 30, 31, 31,
		       30, 31, 30, 31];
		fun get(days: int list, month: int) =
		  if month = 1 then hd days
		  else
		      get(tl days, month - 1)
	    in
		if month = 2 andalso is_leap(year) then 29
		else
		    get(days_per_month, month)
	    end		
      in
	  if day < 1 orelse day > get_days_of_month(month) then false
	  else true
      end
  end
      
