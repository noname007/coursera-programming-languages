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
(* 2a *)
fun card_color(card:card) =
  case card of
      (Clubs, _) => Red
    | (Spades, _) => Red
    | _  => Black

fun card_value (card:card)=
  case card of
      (_, Ace) => 11
    | (_, Num n) => n
    | _  =>  10
		 
fun remove_card(cs: card list, c: card, e) =
  let fun remove (cs, accu) =
	case cs of
	    [] => raise e
	  | h :: t => if h = c  then accu @ t
		      else remove(t, accu @ [h])
  in
      remove(cs, [])
  end;

(*
fun all_same_color (cs: card)  =
  let fun same_color(cs: card, color: color) =
	case cs of
	    [] => true
	  | h :: t =>let val (c,_) = h;
		     in
			 if c = color then same_color(t, c)  else false
		     end;
  in
      case cs of
	  [] => true
	| (color:color, _) :: t =>  same_color(t, color)
  end;		    
*)

(*
may be need to refact ...
*)
fun all_same_color (cs: card list)  =
  case  cs of
      [] => true
    | h1::[] => true
    | h1 :: h2 :: t => let val c1 = card_color(h1);
			   val c2 = card_color(h2);
		       in
			   if c1 = c2  then all_same_color(h2 :: t)  else false
		       end
		    
      


fun sum_cards (cs: card list) =
  let fun sum(cs: card list, accu) =
	case cs of
	    [] => accu
	  | h :: t  => sum(t, accu + card_value(h))
  in
      sum(cs, 0)
  end;


fun score (cs: card list, goal: int) =
  let val sum = sum_cards(cs);
      val pre_score = if sum > goal then 3 *  ( sum - goal ) else goal - sum;
  in
      if all_same_color (cs) then pre_score div 2 else pre_score
  end;


fun officiate(cs: card list, move: move list, goal: int) =
  let val held_cards = [];
      
			   
