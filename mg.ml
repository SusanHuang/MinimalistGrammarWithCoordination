(* file:      mg.ml *)
(* author:    Meaghan Fowlie *)
(* purpose:   implements a minimalist grammar Merge and move are defined in terms of 'check' and 'combine' *)


(* Types *)

type sel = string;;

type lic = string;;

(* type cat = string;; *)

type feat = Pos of lic | Neg of lic | Cat of sel | Sel of sel ;;

type sf = string list * feat list;;

type expr = sf list;;


(* example grammar *)

(* Lexicon *)
let (slept:expr) =  [(["slept"],[Sel "D"; Cat ("V")])];; 

let (john:expr) =  [(["John"],[Cat ("D")])];; 

let (who:expr) =  [(["who"],[Cat ("D"); Neg "wh"])];; 

let (wh:expr) = [([],[Sel "V"; Pos "wh"; Cat ("C")])];;

let (wolf:expr) = [(["wolf"],[Cat ("N")])];;

let (the:expr) = [(["the"],[Sel "N";Cat ("D")])];;

let (which:expr) = [(["which"],[Sel "N";Cat ("D") ; Neg "wh"])];;


(* Functions *)

(* removes 'e' from list 'l' *)
let rec listRemove e l = match l with
    []-> l
  |x::xs-> 
     if x = e 
     then xs
     else x::(listRemove e xs);;

let rec listRemove_2 e l newlist=
  match l with
    []->newlist
  |x::xs-> if x=e
	   then xs@newlist
	   else listRemove_2 e xs (x::newlist);;

2::[3;4];;

[2;3;4;5]@[4;8];;

(* (List.concat [2] [3;4]) = [2]@[3;4];; *)

(* shortest move constraint: true if movers contains no mover headed by f *)
let rec smc (f:feat) (movers:expr) = match movers with
    [] -> true
  | (_,g::_)::more -> 
      if f=g 
      then false
      else smc f more
  |_-> failwith "smc error: mover has no features";;

(* gets features of a string *)
let getFeats t = match t with
    (_,fs)->fs;;

(* gets the first feature of a string *)
let getHeadFeat t = match t with
    (_,f::_)->f
  |_-> failwith "getHeadFeat error: no features?";;

(* Given a whole expression, returns the features of the head element *)
let getExprFeats exp = match exp with
    (_,fs)::_ -> fs
  |_-> failwith "getExprFeats error: no features?";;


(* gets string for a (string,features) pair *)
let getStr t = match t with 
    (s,_) -> s;;

(* gets string of top (string,features) pair in an expression *)
let getExprStr exp = match exp with 
    (s,_)::_ -> s
  |_-> failwith "getExprStr error: empty expression";;

(* checks features. fails with errors, otherwise returns new feature stacks with tops popped *)
let check (feats1:feat list) feats2 = 
  match (feats1,feats2) with 
      ([],_)->failwith "Check error: empty feature list 1"
    | (_,[])->failwith "Check error: empty feature list 2"
    |(f1::fs1, f2::fs2) ->
       match (f1,f2) with
	   (Pos x, Neg y) -> 		(* Move *)
	     if x=y
	     then (fs1,fs2)
	     else failwith "Check error: lic features don't match (Move)"
	 | (Sel x, Cat y) ->	(* Merge *)
	     if x=y
	     then (fs1,fs2)
	     else failwith "Check error: sel features don't match (Merge)"
	 | _ -> failwith "Check error: features are probably not both sel or not both lic or both positive or both negative";;


(*putting-together function for strings. Can replace with trees etc. Just be sure to call it com too.  This includes left and right, but i'm not using them in the later functions yet.*)
let com side s1 s2 fs =
  match side with
      "r" -> (s1@s2, fs)
    | "l" -> (s2@s1, fs)
    | _ -> failwith "com error: neither l nor r";;



(*combining function for both merge and move*)
let combine (t1:sf) (t2:sf) (movers:expr) =
  let (main, beta) = check (getFeats t1) (getFeats t2)
  in
    if beta = [] 			(* non-mover *)
    then (((com "l" (getStr t1) (getStr t2) main)::movers):expr)
    else 				(* mover *)
      if smc (List.hd beta) movers
      then ((getStr t1, main)::((getStr t2, beta)::movers):expr)
      else failwith "SMC violation";;


(*combining function for right merge*)
let combineR (t1:sf) (t2:sf) (movers:expr) =
  let (main, beta) = check (getFeats t1) (getFeats t2)
  in
    if beta = [] 			(* non-mover *)
    then (((com "r" (getStr t1) (getStr t2) main)::movers):expr)
    else 
      if smc (List.hd beta) movers	(* mover *)
      then ((getStr t1, main)::((getStr t2, beta)::movers):expr)
      else failwith "SMC violation";;



      
let merge (exp1:expr) (exp2:expr) = 
  match (exp1,exp2) with 
      ((t1::mvrs1),(t2::mvrs2)) -> combineR t1 t2 (mvrs1@mvrs2)
    |_-> failwith "merge error: empty expression";;


(* (\* manually merge on left *\) *)
(* let mergeL (exp1:expr) (exp2:expr) = *)
(*   match (exp1,exp2) with *)
(*       ((t1::mvrs1),(t2::mvrs2)) -> combine t1 t2 (mvrs1@mvrs2) *)
(*     |_-> failwith "merge error: empty expression";; *)


(* test merge *)
(* merge slept who;; *)

(* looks though mover list for mover headed by -f *)
let rec findMover  f mvrs all_movers  = 
  match mvrs with
      []-> failwith "findMover error: no matching mover"
    | (s,(Neg g)::negs)::rest -> 
	if f=g 
	then ((s,(Neg f)::negs), listRemove (s,(Neg f)::negs) all_movers) (* if you find it, take it out of the mover list and return pair (mover, remaining movers) *)
	else  findMover f  rest all_movers
    | _ -> failwith "findMover error: mover headed by non-move feature";;



let move exp:expr = match exp with
    []-> failwith "move error: empty expression"
  |pr1::rest -> match getFeats pr1 with 
	(Pos f)::_ -> 
	let (pr2,movers) = findMover f rest rest
	in 
	combine pr1 pr2 movers
	|_->failwith "move error: no positive licensing feature on top";;

(* test move *)
(* move (merge wh (merge slept who));; *)

(* tests *)

move (merge wh (merge slept (merge which wolf)));;

merge slept wh;;


let a:expr = [(["a"],[Cat "A";Neg "a"])];;

let b:expr = [(["b"],[Sel "A";Cat "B";Neg "b"])];;

let c:expr = [(["c"],[Sel "B"; Pos "b"; Cat "C"])];;

let c2:expr = [(["c"],[Sel "B"; Pos "a"; Cat "C"])];;

move (merge c2 (merge b a));;

move (merge c (merge b a));;