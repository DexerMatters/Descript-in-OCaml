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
let (<<) l n =  String.get l n

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

let split code = 
  let rec aux strm cond s cse p = match strm with 
  | Utils.Nils -> print (cond.finish s cse)
  | Utils.Cons r -> 
    
    let c = r.head and re = r.rest in
    let case = cond.resume c p in
    if case = 0 then 
      (* finish the previous *)
      let t = cond.finish s cse in
      print t;
      (* resume the next *)
      let opt = condition_list |> List.find_opt (fun ele -> ele.start c) in
      match opt with
      | None -> failwith "Unexpected character"
      | Some cond' -> aux strm cond' "" 0 0
    else 
      (* consume *)
      aux re cond (s ^ string_of_char c) case (p + 1)
  in
  (code |> Utils.explode |> aux) (List.nth condition_list 0) "" 0 0
