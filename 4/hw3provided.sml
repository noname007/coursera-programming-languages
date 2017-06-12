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
  case ls
   of [] => raise NoAnswer
    | l :: ls' => case func l
		   of NONE => first_answer func ls
		    | SOME x => x
				    
		       (*
      
  case List.filter (fn l =>
		       case func l
			of NONE => false
			 | SOME x => x
		   ) ls
   of [] => raise NoAnswer
    | x ::_ => x
			  *)	   
		   
(************ 
 List.filter (fn l => case func(l) of
			   NONE => 
	      ) ls 
	     ***********)
(* 8 *)

fun all_answers1 func ls =
  if null ls then SOME []
  else case foldl (fn (l, accu)  =>
		      case func l of
			  NONE => accu 
			| SOME x => accu @ x)  []   ls of
	   [] => NONE
	 | x => SOME x
		     
(* 8  more better solutions for this questions*)		     
fun all_answers func ls =
  let fun foldf (l, accu) =
	case (func l, accu) of
	    (NONE, SOME []) => NONE
	  | (NONE, accu) => accu
	  | (SOME x, NONE) => SOME x
	  | (SOME x, SOME a) =>  SOME (a @ x)
  in
      foldl foldf  (SOME [])  ls       
  end
		

	
(* 9a 9b 9c *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)
			
val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size(s))

val count_some_var = fn (x,p) =>  g (fn _ => 0) (fn s => if s = x then 1 else 0 ) p

(* 10 *)
fun check_pat p =
  let fun get_all_name p =
	case p of
	    Variable s => [s]
	  | TupleP ps  => List.foldl (fn (p, accu)=> accu @ get_all_name p) [] ps
	  | ConstructorP (_, p) => get_all_name p
	  | _  =>  []
      fun var_names names =
	case names of
	    [] =>  true
	  | x :: xs' =>  case (List.exists (fn e => x = e) xs') of
			     true => false
			   | false => var_names xs'

	
  in
      (var_names o get_all_name) p
  end

(* 11 *)
fun match (v, p) =
  let fun inner_match (v, p) = 
	case (p, v)
	 of (Wildcard, _) =>  []
	  | (Variable s, v) => [(s, v)]
	  | (UnitP, Unit) => []
	  | (ConstP c1, Const c2) =>
	    if c1 = c2  then [] else [("",Unit)]
	  | (TupleP ps, v) =>
	    List.foldl (fn (p, accu) => accu @  inner_match(v, p)) [] ps
	  | (ConstructorP(s1, p), Constructor(s2, v)) =>
	    if s1 = s2 then inner_match(v, p)
	    else [("", Unit)]
	  | _  => [("", Unit)]
		       
  in
      (*
inner_match(v, p)
      *)
      all_answers (fn l =>
		      case l
		       of ("", Unit) => NONE
			| _  => SOME [l] )  (inner_match(v, p))
		  
  end
      

      
(* 12 *)
fun first_match v ps =
  SOME (first_answer (fn p => match(v, p)) ps)
  handle NoAnswer => NONE
 
  
  

      
