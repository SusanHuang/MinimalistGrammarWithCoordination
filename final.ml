(* file:      mg.ml *)
(* author:    Meaghan Fowlie *)
(* edited by: Susan Huang *)
(* purpose:   implements a minimalist grammar Merge and move are defined in terms of 'check' and 'combine' *)


(* Types *)

type sel = string;;

type lic = string;;

(* type cat = string;; *)

type feat = Pos of lic | Neg of lic | Cat of sel | Sel of sel | Coord of sel;;

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

let (which:expr) = [(["which"],[Sel "N"; Cat ("D") ; Neg "wh"])];;

(* Added Lexicon *)

let (saw:expr) =  [(["saw"],[Sel "D"; Sel "D"; Cat ("V")])];;  

let (mary:expr) = [(["Mary"],[Cat "D"])];;

let (mary_nom:expr) = [(["Mary"], [Cat "D"; Neg "nom"])];;

let (e1:expr) = [([], [Sel "V"; Sel "D"; Cat "T"])];;

let (e2:expr) = [([], [Sel "V"; Pos "nom"; Cat "T"])];;

let (e3:expr) = [([], [Cat "D"])];;

let (andC:expr) = [(["and"],[Sel "G"; Sel "G"; Cat "W"])];;

let (orC:expr) = [(["or"],[Sel "G"; Sel "G"; Cat "W"])];;		     

let (ate:expr) = [(["ate"],[Sel "D"; Cat"V"])];;

let (ate_it:expr) = [(["ate"],[Cat"V"])];;
		    
let (cheese:expr) = [(["cheese"],[Cat ("N")])];;

let (chased:expr) = [(["chased"],[Sel "D"; Cat"V"])];;

let (mouse:expr) = [(["mouse"],[Cat ("N")])];;		       

let (cat:expr) = [(["cat"],[Cat ("N")])];;		       

let (walked:expr) = [(["walked"],[Cat"V"])];;

let (ran:expr) = [(["ran"],[Cat"V"])];;
					
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

(* ---------- andCoord Method 1 Functions ---------------- *)
let lookForCat a = 
match a with
| Cat x -> x
| _ -> "";;

let rec getCat a =
match a with 
| [] -> failwith "emptyCat"
| h1::t1 -> 
let catFound = (lookForCat h1)in 
if catFound = "" then getCat t1 else catFound;;

let getCatCheck a b =
if ((getCat a) = (getCat b)) then true
else false;;
(*--------------End of andCoord Functions ----------*)

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
else if x = "G" then (fs1,f2::fs2)
	     else failwith "Check error: sel features don't match (Merge)"
(* Coordination Left *)

| (Cat x, Sel y) -> if (y = "G") then (f1::fs1, f2::fs2)
else failwith "Check error: Selectional Category on the right hand side that is not 'and' "

| _ -> failwith "Check error: features are probably not both sel or not both lic or both positive or both negative";;


(*putting-together function for strings. Can replace with trees etc. Just be sure to call it com too.  This includes left and right, but i'm not using them in the later functions yet.*)
let com side s1 s2 fs =
  match side with
      "r" -> (s1@s2, fs)
    | "l" -> (s2@s1, fs)
    | _ -> failwith "com error: neither l nor r";;


(* Check for Sel G *)
let selGCheck a =
match (List.hd a) with
| (Sel x) -> if (x = "G") then true else false
| _ -> false;;

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
(* Coordination Check 1 *)
(* example: merge and john -> ["and"; "john"] *)
else if (selGCheck main) then (((com "r" (getStr t1) (getStr t2) ((List.hd main)::beta))::movers):expr)
(* Coordination Check 2 *)
(* example: merge john and mary -> [] *)
(* Must check the categories of left and right of the 'and' *)
else if (selGCheck beta) then 
(if ((getCat main) = (getCat beta)) then(((com "r" (getStr t1) (getStr t2) main)::movers):expr) else failwith "andCoordination Failure: Categories do not match")
else 
      if smc (List.hd beta) movers	(* mover *)
      then ((getStr t1, main)::((getStr t2, beta)::movers):expr)
      else failwith "SMC violation";;

      
let merge (exp1:expr) (exp2:expr) = 
  match (exp1,exp2) with 
      ((t1::mvrs1),(t2::mvrs2)) -> combineR t1 t2 (mvrs1@mvrs2)
    |_-> failwith "merge error: empty expression";;


(* manually merge on left *)
let mergeL (exp1:expr) (exp2:expr) =
match (exp1,exp2) with
((t1::mvrs1),(t2::mvrs2)) -> combine t1 t2 (mvrs1@mvrs2)
|_-> failwith "merge error: empty expression";;


(* looks though mover list for mover headed by -f *)
let rec findMover f mvrs  = 
  match mvrs with
      []-> failwith "findMover error: no matching mover"
    | (s,(Neg g)::negs)::rest -> 
	if f=g 
	then ((s,(Neg f)::negs), listRemove (s,(Neg f)::negs) mvrs) (* if you find it, take it out of the mover list and return pair (mover, remaining movers) *)
	else  findMover f rest
    | _ -> failwith "findMover error: mover headed by non-move feature";;



let move exp:expr = match exp with
    []-> failwith "move error: empty expression"
  |pr1::rest -> match getFeats pr1 with 
	(Pos f)::_ -> 
	let (pr2,movers) = findMover f rest 
	in 
	combine pr1 pr2 movers
	|_->failwith "move error: no positive licensing feature on top";;

(* ----------- Beginning of andCoord Method 1 ------------- *)

let andCoordFunc cat1 cat2 =
if (getCatCheck (getFeats cat1) (getFeats cat2) = true) then
let rightOfAnd = (com "r" ["and"] (getStr cat2) (getFeats cat2))
in (com "r" (getStr cat1) (getStr rightOfAnd) (getFeats rightOfAnd))
else failwith "and Coordination error: non-matching categories";;

let andCoord a b = andCoordFunc (List.hd a) (List.hd b);;

(* Test And Coordination *)

print_string "\n ---- And Coordination Method 1 ---- \n";;
andCoord john (merge the wolf);;
(* Test: Bad Coordination *)
(* andCoord john slept ;; *)


(* ---------- End of andCoord Method 1 -------- *)

(* Test Merge on andCoordination Method 2*)
(* andC coordination of DPs *)
print_string "\n ---- And Coordination Method 2 ---- \n";;
merge mary (merge andC john);;
merge john (merge andC (merge the wolf));;
merge (merge the wolf)(merge andC john);;

(* Test: Bad Coordination*)
(* merge slept(merge andC john);; *)

(* VP test *)
merge walked (merge andC ran);;
merge (merge chased (merge the mouse))(merge andC (merge ate (merge the cheese)));;
(* Intransitive verb and Coordination *)
merge ate_it (merge andC walked);;
(*Verb and Coordination *)
merge (merge ate (merge the cheese)) (merge andC walked);;
 
(* Bad Coordination Transitive ate needs DP *)
(* merge ate (merge andC walked);; *)


(* Test Merge on orCoordination using andCoordination Method 2 *)
(* orC coordination of DPs *)
print_string "\n --- Or Coordination using And Coordination Method 2 --- \n";;
merge mary (merge orC john);;
merge john (merge orC (merge the wolf));;
merge (merge the wolf) (merge orC john);;
(* Test: Bad Coordination *)
(* merge slept (merge orC john);; *)
   
(* --- Below tests are tests from the homework --- *)

print_string "\n ---- Homework Test Cases Below ---- \n\n";;

(* test move *)
(* move (merge wh (merge slept who));; *)

(* Grammaticality Tests *)

merge slept john;;
merge slept (merge the wolf);;
move (merge wh (merge slept (merge which wolf)));;
merge slept (merge the wolf);;

(* Generate Expression Tests *)
merge slept mary;;
merge (merge saw john) mary;;
(* Method 1: Mary saw John *)
mergeL (merge e1 (merge (merge saw john) e3)) mary;;
(* Method 2: Mary saw John *)
move (merge e2 (merge (merge saw john) mary_nom));;
move (merge e2 (merge slept mary_nom));;
move (merge wh (merge (merge saw (merge the wolf)) who));;
move (merge wh (merge (merge saw mary) (merge which wolf)));;

(* merge slept wh;; *)
