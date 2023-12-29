(* let precedence_of = 
  let open Lexer in function 
  | Integer _ | Float _ | Identifier _ -> 0
  | Add | Sub -> 1
  | Mul | Div -> 2
  | Assign -> 3
  | Keyword _ -> 4
  | _ -> 128 *)

type ast = 
| PrimaryAST of ast
| IntegerAST of int
| FloatAST of float
| VariableAST
| BinaryAST of string * ast * ast (* X ?+ Y *)
| LetAST of ast * ast (* let X = Y *)

type func_seq = Lexer.token -> Lexer.token list
