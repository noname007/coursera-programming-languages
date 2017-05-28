fun sum_list(xs: int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs);




val empty_list = sum_list([]);
val y = sum_list([4,5,6]);
val z = sum_list([2]);


fun list_product(xs: int list) =
  if null xs
  then 1
  else
      hd xs * list_product(tl xs);

val px = list_product([]);
val py = list_product [5];
val pz = list_product [2, 4, 2]; (* 16 *)


			 

		       
		      
