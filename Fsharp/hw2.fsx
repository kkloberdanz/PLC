(*
   CS:3820 Programing Language Concepts
   
   Homework 2

   Team:  Max Riley, Kyle Klobberdanz
*)


(* Problem Set 1 *)

type oper = Neg | Not | Add | Mul | Sub | Less | Eq | And

type expr = 
  | C of int
  | Op1 of oper * expr
  | Op2 of oper * expr * expr
  | If of expr * expr * expr


// size : expr -> int

let rec size x =
    match x with
    | C j -> 1
    | Op1 (j,k) -> 1 + size k
    | Op2 (j,k,l) -> 1 + size k + size l
    | If (j,k,l) -> 1 + size j + size k + size l


let example =  (If (C 4, Op2 (Add, C 1, C 2), C 9))
let example2 = (C 4)

// subexpressions : expr -> expr list

let rec subexpressions x =
    match x with
    | C j -> []
    | Op1 (j,k) -> [k] @ subexpressions k
    | Op2 (j,k,l) -> [k;l] @ subexpressions k @ subexpressions l
    | If (j,k,l) -> [j;k;l] @ subexpressions j @ subexpressions k @ subexpressions l

// normAdd : expr -> expr

let rec normAdd x =
    match x with
    | C j -> C j
    | Op2 (Add,Op2(Add,m,n),l) -> Op2(Add, m , Op2(Add, normAdd n, normAdd l))
    | Op2 (x,y,z) -> Op2(x, normAdd y, normAdd z)
    | Op1 (x,y) -> Op1(x, normAdd y)
    | If (x,y,z) -> If(normAdd x, normAdd y, normAdd z)

// some examples to try
let e1 = Op2 (Add, Op2 (Add, C 1, C 2), C 3)
let e2 = Op2 (Add, Op2 (Add, C 1, C 2), Op2 (Add, C 3, C 4))
let e3 = Op2 (Add, Op2 (Add, C 1, C 2), Op2 (Mul, C 3, Op2 (Add, C 4, C 5)))



(* Problem Set 2 *)

type sInstr =
  | SC of int
  | SAdd 
  | SSub
  | SMul 
  | SNeg
  | SLess
  | SIfze of int
  | SJump of int

(*type expr = 
  | C of int
  | Op1 of oper * expr
  | Op2 of oper * expr * expr
  | If of expr * expr * expr
  
  type oper = Neg | Not | Add | Mul | Sub | Less | Eq | And*)

// scomp : expr -> sInstr list

let rec scomp x =
    match x with
    | C j -> [SC j]
    | Op1 (Neg,y) -> [SC -1] @ scomp y @ [SMul]
    | Op2 (Add, x, y) -> scomp x @ scomp y @ [SAdd]
    | Op2 (Mul, x, y) -> scomp x @ scomp y @ [SMul]
    | Op2 (Sub, x, y) -> scomp x @ scomp y @ [SSub]
    | Op2 (Less, x, y) -> scomp x @ scomp y @ [SLess]
    | Op2 (Eq, x, y) -> scomp x @ scomp y @ [SSub] @ [SIfze 2] @ [SC 0] @ [SJump 1]  @ [SC 1]
    | Op1 (Not, x) -> scomp x @ [SIfze 2] @ [SC 0] @ [SJump 1] @ [SC 1]
    | If (x, y, z) -> scomp x @ [SIfze 2] @ scomp y @ [SJump 1] @ scomp z
    | Op2 (And, x, y) -> scomp x @ [SIfze 4] @ scomp y @ [SIfze 2] @ [SC 1] @ [SJump 1] @ [SC 0]
    | _ -> failwith "Input expression is ill-formed"

// drop : int -> 'a list -> 'a list

let rec drop x j =
    match j with
    | H::T when x > 0 -> drop (x-1) T
    | j when List.length(j) <= 0 && x > 0 -> failwith "Index out of bound"
    | j -> j

// seval : sInstr list -> int list -> int

let rec seval stack intList =
    match (stack, intList) with
    | SC x:: T, s -> seval T (x :: s)
    | SAdd:: T, n2::n1::s -> seval T ((n1+n2)::s)
    | SSub:: T, n2::n1::s -> seval T ((n1-n2)::s)
    | SMul::T, n2::n1::s -> seval T ((n1*n2)::s)
    | SNeg:: T, n1::s -> seval T ((-n1)::s)
    | SLess::T, n2::n1::s -> if (n1 < n2) then seval T (1::s) else seval T (0::s)
    | SIfze p::T, n1::s -> if (n1 = 0) then seval (drop p T) (s) else seval T (s)
    | SJump p:: T, s -> seval (drop p T) s
    | [], n::_ -> n
    | [], [] -> failwith "no result on stack!"
    | _ -> failwith "too few operands on stack!"


// run : sInstr list -> int
let run (p : sInstr list) : int = seval p []


// byteCode : sInstr list -> string

let rec byteCode stack =
    match stack with
    | [] -> ""
    | (SC x):: T -> "0 " + string x + " " + byteCode T
    | (SAdd):: T -> "1 " + byteCode T
    | (SSub):: T -> "2 " + byteCode T
    | (SMul):: T -> "3 " + byteCode T
    | (SNeg):: T -> "4 " + byteCode T
    | (SLess):: T -> "5 " + byteCode T
    | (SIfze x):: T -> "6 " + string x + " " + byteCode T
    | (SJump x):: T -> "7 " + string x + " " + byteCode T

// beval : int list -> int list -> int



// parse : string -> int list
let parse (p : string) : int list =  
  let l = Seq.toList (p.Split ' ') in
  List.map System.Int32.Parse l


