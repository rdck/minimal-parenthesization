(*******************************************************************************
SIMPLY TYPED LAMBDA CALCULUS

OPERATION     PRECEDENCE  ASSOCIATIVITY
lambda        1           n/a
+             2           left
-             2           left
*             3           left
^             4           right
application   5           left
*******************************************************************************)

open Core

type ty =
  | Z64                 (* machine integers *)
  | Arrow of ty * ty    (* functions        *)

type identifier = string

(* associate an identifier with some value *)
type 'a binding = {
  name : identifier ;
  value : 'a ;
}

let show_binding f { name ; value } =
  Format.asprintf "%s := %a" name f value

let pp_binding af f b =
  Format.fprintf f "%s" (show_binding af b)

type binop =
  | Add
  | Sub
  | Mul
  | Exp

let show_binop : binop -> string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Exp -> "^"

let pp_binop f op = Format.fprintf f "%s" (show_binop op)

type expression =
  | Lit of int                                  (* integer literal      *)
  | Bin of binop * expression * expression      (* integer arithmetic   *)
  | Var of identifier                           (* variable             *)
  | App of expression * expression              (* function application *)
  | Abs of ty binding * expression              (* function abstraction *)

let print = Printf.sprintf

let rec naive_show_ty : ty -> string = function
  | Z64 -> "Z"
  | Arrow (domain, codomain) ->
      print "(%s -> %s)" (naive_show_ty domain) (naive_show_ty codomain)

let show_ty : ty -> string =
  let rec show (left : bool) = function
    | Z64 -> "Z"
    | Arrow (domain, codomain) ->
        let representation = print "%s -> %s" (show true domain) (show false codomain) in
        if left then print "(%s)" representation else representation in
  show false

let pp_ty f t = Format.fprintf f "%s" (show_ty t)

type associativity =
  | Left
  | Right

type precedence = int

type operation =
  | Nullary
  | Unary   of precedence * expression
  (* associativity is only applicable in the case of binary operations *)
  | Binary  of precedence * associativity * expression * expression

let structure : expression -> operation = function
  | Lit _ -> Nullary
  | Bin (op, lhs, rhs) ->
      begin match op with
      | Add -> Binary (2, Left , lhs, rhs)
      | Sub -> Binary (2, Left , lhs, rhs)
      | Mul -> Binary (3, Left , lhs, rhs)
      | Exp -> Binary (4, Right, lhs, rhs)
      end
  | Var _ -> Nullary
  | App (f, x) -> Binary (5, Left, f, x)
  | Abs (_, body) -> Unary (1, body)

let node_text : expression -> string = function
  | Lit i -> print "%d" i
  | Bin (Add, _, _) -> " + "
  | Bin (Sub, _, _) -> " - "
  | Bin (Mul, _, _) -> " * "
  | Bin (Exp, _, _) -> " ^ "
  | Var id -> id
  | App _ -> " "
  | Abs ({ name ; value = domain }, _) ->
      print "λ %s : %s . " name (show_ty domain)

let show_expression : expression -> string =

  (* wrap a string in parentheses when a condition is met *)
  let wrap (s : string) (condition : bool) =
    if condition then print "(%s)" s else s in

  let rec show (p : precedence) (expr : expression) =
    let atom = node_text expr in
    match structure expr with
    | Nullary -> atom
    | Unary (p', e) ->
        let s = print "%s%s" atom (show (p' - 1) e) in
        wrap s (p' <= p)
    | Binary (p', assoc, lhs, rhs) ->
        let (left, right) = match assoc with
        | Left  -> (p' - 1, p')
        | Right -> (p', p' - 1) in
        let s = print "%s%s%s" (show left lhs) atom (show right rhs) in
        wrap s (p' <= p) in

  show 0

let pp_expression f e = Format.fprintf f "%s" (show_expression e)
