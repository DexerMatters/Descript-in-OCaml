open Stream

type keyword = Let | If | Else | Return | Break| Fun | Flow | Match
type token = 
  Keyword of keyword
| Identifier of string
| Str of string
| Integer of int
| Float of float
| Bool of bool

(* () *)| ParenL | ParenR
(* [] *)| BlanketL | BlanketR
(* {} *)| BraceL | BraceR
(* . *) | Dot
(* , *) | Comma
(* ; *) | Semicomma

| BinaryOp of string

| Undefined
| EOF

let keyword_of s = match s with
  "let" -> Some Let
| "if" -> Some If
| "else" -> Some Else
| "return" -> Some Return
| "break" -> Some Break
| "fun" -> Some Fun
| "flow" -> Some Flow
| "match" -> Some Match
| _ -> None

type database = { code : string; tokens: token}


let (@|) g f = fun c -> g c || f c
let (@&) g f = fun c -> g c && f c
let (%) f c =  if f c then 1 else 0
let (=>) a b = !a.(b)

let contains l e = List.exists ((=) e) l

type condition = {start: char -> bool; resume: char -> int -> int; finish: string -> int -> token}

let num c = match c with '0' .. '9' -> true | _ -> false

let alphabet c = match c with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let ignorable c = match c with '\n' | '\r' | ' ' -> true | _ -> false

let float_dot c = match c with '.' -> true | _ -> false


let condition_list = [
  (*Identifier & Keyword*) 
  {start = alphabet; 
  resume = (fun c p -> print_int p;(alphabet) % c);
  finish = (fun s _c -> match keyword_of s with Some key -> Keyword key | None -> Identifier s)
  };
  (*Integer*) 
  {start = num; 
  resume = (fun c p -> print_int p; num % c);
  finish = (fun s _c -> Integer (int_of_string s))
  }; 
  (*Binary Operator*)
  {start = contains ['+'; '-'; '*'; '/'; '='];
  resume = (fun _c p -> print_int p; if p = 1 then 0 else 1);
  finish = (fun s c -> print_int c; BinaryOp s)
  }

]

let string_of_char c = String.make 1 c

let print t = match t with
| Identifier x -> Printf.printf "Id : %s\n" x
| Integer x -> Printf.printf "Int: %d\n" x
| BinaryOp x -> Printf.printf "BinOp : %s\n" x
| _ -> ()

let current_token = ref Undefined
let current_cond = ref (List.nth condition_list 0)
let rest = ref Utils.Nils
let init code = rest := Utils.explode code
let next_token = fun () ->
  let rec aux strm s p cse = 
    match strm with 
    | Utils.Cons x ->
      let case = !current_cond.resume x.head p in
      if case = 0 then
        let () = current_token := !current_cond.finish s cse in 
        let () = current_cond := List.find (fun e -> e.start x.head) condition_list in
        Utils.Cons x
      else
        aux x.rest (s ^ string_of_char x.head) (p + 1) cse

    | Utils.Nils -> 
      if s = "" then current_token := EOF else
      current_token := !current_cond.finish s cse; Utils.Nils
    
  in
  let r = aux !rest "" 0 0 in
  rest := r
