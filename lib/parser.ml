open Lexer

let precedence_of () = match !current_token with
| Integer _ | Float _ | Identifier _ -> 200
| BinaryOp x -> 
  (match x with
  | "*" | "/" -> 150
  | "+" | "-" -> 149
  | "=" -> 148
  | _ -> failwith "Unknown binary operator")
| Keyword _ -> 10
| _ -> -1

type ast = 
| AreaAST      of ast                     (* { ... } *)
| IntegerAST   of int
| FloatAST     of float
| VariableAST  of string
| CallAST      of string * ast list       (* X(arg1, arg2, ...) *)
| PrototypeAST of string * ast * ast list (* X(arg1, arg2, ...) {  }*)
| BinaryAST    of token * ast * ast      (* X ?+ Y *)
| LetAST       of ast * ast               (* let X = Y *)
| IfAST        of ast * ast               (* if X { Y }*)
| IfElseAST    of ast * ast * ast         (* if X Y else Z*)
| StrictLetAST of ast * ast * ast         (* let X:Y = Z *)
| UnknownAST

let rec parse_primary () =
  let r =  match !current_token with
  | Integer i -> let t = IntegerAST i in next_token (); t
  | Float i -> let t = FloatAST i in next_token (); t
  | ParenL -> parse_paren ()
  | Identifier _ -> parse_id ()
  | _ -> UnknownAST in 
  r 
  
and parse_expression () = 
  let lhs = parse_primary () in 
  match lhs with UnknownAST -> UnknownAST | _ -> 
    parse_binary_operator 0 lhs

and parse_binary_operator exp_prec lhs = 
  let rec aux lhs' = 
    let tok_prec = precedence_of () in 
    if tok_prec < exp_prec then lhs'
    else
      let bin_op = !current_token in 
      next_token ();
      let rhs = parse_primary () in 
      match rhs with UnknownAST -> UnknownAST | _ ->
        let next_prec = precedence_of () in 
        if tok_prec < next_prec then 
          let rhs' = parse_binary_operator (tok_prec + 1) rhs in 
          (
            match rhs' with UnknownAST -> UnknownAST | _ ->
            aux (BinaryAST (bin_op, lhs', rhs'))
          )
        else aux (BinaryAST (bin_op, lhs', rhs))
  in aux lhs

and parse_paren () = 
  next_token ();
  match parse_expression () with
  | UnknownAST -> failwith "todo"
  | _ as x -> next_token (); x

and parse_id () = 
  match !current_token with
  | Identifier name ->
    next_token ();
    (match !current_token with 
    | ParenL -> 
      next_token ();
      let args = (match !current_token with ParenR -> []
      | _ -> 
        let rec aux l = (match !current_token with
        | ParenR -> next_token () (* eat ) *);  l
        | Comma -> next_token (); aux l
        | _ -> aux (parse_expression () :: l))
        in aux []
      )
      in CallAST (name, args)
    | _ -> VariableAST name
    )
  | _ -> UnknownAST

