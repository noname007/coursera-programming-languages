(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2
	   
(* put your solutions for problem 1 here *)
	     
(* 1a *)
fun all_except_option(s: string, lst: string list) =
  
  let fun del_ele(tlst: string list, accu: string list) =
	case tlst of
	    [] => NONE
	  | h :: tl => if same_string(s,h)then SOME(accu @ tl)
		       else del_ele(tl, accu @ [h] )
  in
      del_ele(lst, [])
  end;


(* 1b *)
fun  get_substitutions1 (strlistlist: string list list, s: string) =
  case strlistlist of
      [] => []
    | l :: tl => case  all_except_option(s, l) of
		     NONE => get_substitutions1(tl, s)
		   | SOME r  => r @ get_substitutions1(tl, s)
						     
						      
(* 1c *)

fun get_substitutions2 (l: string list list, s: string) =
  let fun inner(l: string list list,  accu: string list) =
	case l of
	    [] => accu
	  | h :: t => case all_except_option(s, h) of
			  NONE => inner(t, accu)
			| SOME r => inner(t,accu @ r)
  in
      inner(l, [])
  end;

(* 1d *)
fun similar_names (l: string list list, full_name: {first: string, middle: string, last: string}) =
  let 
      val {first=first_name, middle=middle_name, last=last} = full_name;
      val first_name_sets = get_substitutions2(l, first_name);
      fun gen(l: string list, accu) =
	  case l of
	      [] => accu
	    | h :: t => gen(t, accu @ [{first = h, middle = middle_name, last = last }] )
  in
      gen(first_name_sets, [])
  end;

  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
