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
      full_name :: gen(first_name_sets, [])
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
      (Clubs, _) => Black
    | (Spades, _) => Black
    | _  => Red
(* 2b *)
fun card_value (card:card)=
  case card of
      (_, Ace) => 11
    | (_, Num n) => n
    | _  =>  10
(* 2c *)		 
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
(* 2d *)
fun all_same_color (cs: card list)  =
  case  cs of
      [] => true
    | h1::[] => true
    | h1 :: h2 :: t => let val c1 = card_color(h1);
			   val c2 = card_color(h2);
		       in
			   if c1 = c2  then all_same_color(h2 :: t)  else false
		       end
(* 2e *)
fun sum_cards (cs: card list) =
  let fun sum(cs: card list, accu) =
	case cs of
	    [] => accu
	  | h :: t  => sum(t, accu + card_value(h))
  in
      sum(cs, 0)
  end;

(* 2f *)
fun score (cs: card list, goal: int) =
  let val sum = sum_cards(cs);
      val pre_score = if sum > goal then 3 *  ( sum - goal ) else goal - sum;
  in
      if all_same_color (cs) then pre_score div 2 else pre_score
  end;

(* 2g *)
fun officiate(cs: card list, move: move list, goal: int) =
  let fun play(cs, move, held_cards: card list) =
	let val now_score = score(held_cards, goal);
	in
	    if now_score > goal then now_score
	    else
		case move of
		    [] => now_score
		  | Discard(card) :: move_lists =>
		    play(cs, move_lists, remove_card(held_cards, card,  IllegalMove))
		  | Draw :: move_lists =>
		    case cs of
			[] => now_score
		      | first :: card_lists =>
			play(card_lists, move_lists,held_cards @ [first])
	end;
  in
      play(cs, move, [])
  end;

(* 3a challenge *)
(* 
solve hints
refer : 
1. https://www.coursera.org/learn/programming-languages/discussions/weeks/3/threads/loOF3rsjEeaIRw7T1E5tHA
2. https://www.coursera.org/learn/programming-languages/discussions/weeks/3/threads/Z9KVbGHOEearWApJMKuNaw/replies/v2za_2IbEeaGXQq5jeF-9Q?sort=createdAtAsc&page=1

enumberate the suituations that each Ace may have the value 1 ,so likly add the -10 card to the card list,keep the color of the card with the ace at the same time

*)
fun score_challenge(cs: card list, goal: int) =
  let  fun count_ace_nums (cs, aces) = 
	 case cs of
	     [] => aces
	   | (c, Ace) :: t  =>  count_ace_nums(t, aces @ [(c, Ace)])
	   | _ :: t =>  count_ace_nums(t, aces)
				      
       val aces = count_ace_nums(cs, []);
       
       fun least_sum(cs, aces, least_score) =
	 case aces of
	     [] => least_score
	   | (c, _) :: t  => let val tmp_cs = cs @ [(c, Num( ~10))];
				 val tmp_sum = score(tmp_cs, goal);
			     in
				 if tmp_sum < least_score then 
				     least_sum(tmp_cs, t , tmp_sum)
				 else
				     least_sum(tmp_cs, t, least_score)
			     end;	      
  in
      least_sum(cs, aces, score(cs, goal))
  end;



(*
copy from the fun *officiate* and modify the fun *score* to the *score_challenge* only
*)
fun officiate_challenge(cs: card list, move: move list, goal: int) =
  let fun play(cs, move, held_cards: card list) =
	let val now_score = score_challenge(held_cards, goal);
	in
	    if now_score > goal then now_score
	    else
		case move of
		    [] => now_score
		  | Discard(card) :: move_lists =>
		    play(cs, move_lists, remove_card(held_cards, card,  IllegalMove))
		  | Draw :: move_lists =>
		    case cs of
			[] => now_score
		      | first :: card_lists =>
			play(card_lists, move_lists,held_cards @ [first])
	end;
  in
      play(cs, move, [])
  end
      
      
(* 3b *)
fun careful_player(cs: card list, goal: int) =
  
  let fun play(cs: card list, held: card list, moves: move list) =
	
	let fun tmp_helds_score(card) =
	      (held @ [card], score_challenge(held @ [card], goal))
	in
	    case cs of
		[] => moves	    
	      | card1 ::  t1 =>
		let val tmp_held = held @ [card1];
		    val tmp_score = score_challenge(tmp_held, goal)
		in
		    if tmp_score <= goal - 10  then
			play(t1, tmp_held, moves @ [Draw])
		    else  if tmp_score > goal then
			moves
		    else
			case t1 of
			    [] => moves
			  | card2 :: t2 =>
			    case tmp_helds_score(card2) of
				(tmp_held2 , tmp_score2) =>
				if tmp_score2 <>  0 then
				    play(t1, tmp_held, moves @  [Draw])
					
				else
				    moves  @ [ Discard  card1] @ [Draw]
		end
		    
	end
  in
      play(cs, [], [])
  end
      
