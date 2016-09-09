
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

(*
let rec elem l n =
  match l with
  | E -> failwith "Index out of bounds"
  | L(h,t) ->
        if (n = 0) then 
          h
        else 
          elem n-1 t
*)

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


(* move : ilist -> ilist -> ilist *)


(* reverse : ilist -> ilist *)




(* Problem 2 *)


type expr = 
  | CstI of int
  | Prim of string * expr * expr


let rec eval (e : expr) : int =
    match e with
    | CstI i -> i
    | Prim("+", e1, e2) -> eval e1 + eval e2
    | Prim("*", e1, e2) -> eval e1 * eval e2
    | Prim("-", e1, e2) -> eval e1 - eval e2
    | Prim _ -> failwith "unknown primitive"


(* eval1 : expr -> int *)


(* type expr2 *)


(* eval2 : expr2 -> int *)




(* Problem 3 *)


(* type aexpr *)


(* expressions e1 e2 e2 *)


(* toString : aexpr -> string *)


(* simplify : aexpr -> aexpr *)










