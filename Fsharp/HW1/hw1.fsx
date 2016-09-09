
(*
   CS:5810 Formal Methods in Software Engineering
   Fall 2015
   
   Homework 1

   Team: Kyle Kloberdanz, Max Riley
*)

(*** List Functions from Lecture ***)
//type 'a list = E | L of 'a * 'a list
(*
let head l = 
    match l with 
    | L (h, _) -> h
    | E -> failwith "list is empty"

let tail l = 
    match l with 
    | L (_, t) -> t
    | E -> failwith "list is empty"

let rec last l = 
    match l with
    | L (h, E) -> h
    | L (h, t) -> last t
    | E -> failwith "list is empty"

let rec length l =
    match l with
    | E -> 0
    | L(_, t) -> 1 + length t 
*)

(* Problem 1 *)

type ilist = 
    E 
  | L of int * ilist


(* sum : ilist -> int *)
let rec sum l =
  match l with
  | E -> 0
  | L(h, t) -> h + sum t

(* *** Testing *** *)
let l1 = L(1, L(2, L(3, E)))
printfn "%A" l1
printfn "%i" (sum l1)
printfn "%i" (sum E)
(* *** Testing *** *)


(* elem : int -> ilist -> int *)

let rec elem n l =
   match l with
   | L(h,t) when n = 1 -> h
   | L(h,t) -> elem (n-1) t
   | E -> failwith "Index out of bound"

(* *** Testing *** *)
//printfn "%i" (elem 2 l1)
(* *** Testing *** *)



(* isIn : int -> ilist -> bool *)
let rec isIn x l = 
  match l with
  | E -> false
  | L(h, t) -> 
          if (h = x) then
            true
          else
            isIn x t

(* *** Testing *** *)
printfn "2 is in the list: %b" (isIn 2 l1)
printfn "but 9 is not in the list: %b" (isIn 9 l1)
(* *** Testing *** *)

(* remove: int -> ilist -> ilist *)

let rec remove x l =
   match l with
   | L(h,t) when x=h -> remove x t
   | L(h,E) -> L(h,E)
   | L(h,t) -> L(h,remove x t)
   | E -> failwith "Empty List"

(* move : ilist -> ilist -> ilist *)

(*
let rec move l1 l2 =
    match l1 with
    | E -> l1 := l2
    | L(h, t) -> move t l2
//let l1 = L(1, L(2, L(3, E)))
let l2 = L(4, (L(5, L(6, E))))
printfn "here"
printfn "move l1 l2: %A" (move l1 l2)
*)
(* reverse : ilist -> ilist *)




(* Problem 2 *)


type expr = 
  | CstI of int
  | Prim of string * expr * expr

let num1 = CstI 3
let num2 = CstI 12

let test4 = Prim("+",num1,num2)
let test5 = Prim("==",num1,num2)
let test6 = Prim("max",num1,num2)


let rec eval (e : expr) : int =
    match e with
    | CstI i -> i
    | Prim("+", e1, e2) -> eval e1 + eval e2
    | Prim("*", e1, e2) -> eval e1 * eval e2
    | Prim("-", e1, e2) -> eval e1 - eval e2

    | Prim _ -> failwith "unknown primitive"


(* eval1 : expr -> int *)
let rec eval1 (e : expr) : int =
    match e with
    | CstI i -> i
    | Prim("+", e1, e2) -> eval1 e1 + eval1 e2
    | Prim("*", e1, e2) -> eval1 e1 * eval1 e2
    | Prim("-", e1, e2) -> eval1 e1 - eval1 e2

    | Prim("<", e1, e2) -> 
            if ((eval1 e1) < (eval1 e2)) then
                    1
            else
                    0

    | Prim(">", e1, e2) -> 
            if ((eval1 e1) > (eval1 e2)) then
                    1
            else
                    0
                    
    | Prim("==", e1, e2) -> 
            if ((eval1 e1) = (eval e2)) then
                    1
            else
                    0
    | Prim("max", e1, e2) ->
            if ( (eval1 e1) > (eval1 e2)) then
                    eval1 e1
            else
                    eval1 e2

    | Prim("min", e1, e2) ->
            if ( (eval1 e1) < (eval1 e2)) then
                    eval1 e1
            else
                    eval1 e2

    | Prim _ -> failwith "unknown primitive"
    
(* type expr2 *)


(* eval2 : expr2 -> int *)




(* Problem 3 *)


(* type aexpr *)


(* expressions e1 e2 e2 *)


(* toString : aexpr -> string *)


(* simplify : aexpr -> aexpr *)










