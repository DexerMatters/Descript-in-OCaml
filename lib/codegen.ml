open Lexer
open Parser

let ctx = Llvm.create_context ()

let modul = Llvm.create_module ctx "Demo"

let builder = Llvm.builder ctx

let pm_function = Llvm.PassManager.create_function modul

let named_values = []


let codegen = function 
| IntegerAST i -> Llvm.const_int (Llvm.i32_type ctx) i
| FloatAST f -> Llvm.const_float (Llvm.float_type ctx) f
| VariableAST name -> Option.get (List.assoc_opt name named_values) (* todo : error handle *)
| _ -> failwith "Can't find codegen"