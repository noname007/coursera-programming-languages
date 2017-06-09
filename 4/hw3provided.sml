(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals sl =
  List.filter (fn s => Char.isUpper (String.sub (s, 0))) sl
(* 2 *)			  
fun longest_string1 sl =
  foldl (fn (s, accu) => if String.size s > String.size accu then s else accu ) ""  sl
(* 3 *)	
fun longest_string2 sl =
  foldl (fn (s, accu) => if String.size s >=  String.size accu then s else accu ) ""  sl
(* 4 *)
fun longest_string_helper func sl =
  foldl (fn (s, accu) => if func(String.size s, String.size accu) then s else accu) "" sl

val longest_string3 = longest_string_helper (fn (i1, i2) => i1 > i2 )
val longest_string4 = longest_string_helper (fn (i1, i2) => i1 >= i2)

(* 5 *)
val  longest_capitalized  = longest_string1 o only_capitals
  
(* 6 *)
fun rev_string s =
  (String.implode o rev o String.explode)  s

(* 7 *)
fun first_answer func ls =
  case List.filter (fn l => case func l  of
				NONE => false
			      | SOME _ => true) ls of
      [] => raise NoAnswer
    | x ::_ => x
			   
		   
(************ 
 List.filter (fn l => case func(l) of
			   NONE => 
	      ) ls 
	     ***********)
(* 8 *)

fun all_answers func ls =
  if null ls then SOME []
  else case foldl (fn (l, accu)  =>
		      case func l of
			  NONE => accu 
			| SOME x => accu @ x)  []   ls of
	   [] => NONE
	 | x => SOME x
		     
(* 8  more better solutions for this questions*)		     
fun all_answers2 func ls =
  let fun foldf (l, accu) =
	case (func l, accu) of
	    (NONE, _) => accu
	  | (SOME x, NONE) => SOME x
	  | (SOME x, SOME a) =>  SOME (a @ x)
  in
      foldl foldf  (SOME [])  ls       
  end
		

	
