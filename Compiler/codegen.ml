(* ===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
(* Change to Sast *)
open Ast

exception Error of string

let context = global_context ()
let the_module = create_module context "Dice Codegen"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let i32_t = i32_type context;;
let i8_t = i8_type context;;

let rec codegen_expr llbuilder = function
      Int_Lit i           -> build_global_stringptr "Hi" "" llbuilder
  |   Boolean_Lit b       -> build_global_stringptr "Hi" "" llbuilder
  |   Float_Lit f         -> build_global_stringptr "Hi" "" llbuilder
  |   String_Lit s        -> build_global_stringptr s "" llbuilder
  |   Char_Lit c          -> build_global_stringptr "Hi" "" llbuilder
  |   This                -> build_global_stringptr "Hi" "" llbuilder
  |   Id s                -> build_global_stringptr "Hi" "" llbuilder
  |   Binop(e1, op, e2)  -> build_global_stringptr "Hi" "" llbuilder
  |   Assign(e1, e2)     -> build_global_stringptr "Hi" "" llbuilder
  |   Noexpr              -> build_global_stringptr "Hi" "" llbuilder
  |   ArrayOp(e1, el)    -> build_global_stringptr "Hi" "" llbuilder
  |   ObjAccess(e1, e2)  -> build_global_stringptr "Hi" "" llbuilder
  |   Call(fname, el)    -> (function
        "print" -> 
          let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
          let printf = declare_function "printf" printf_ty the_module in
          let s = codegen_expr llbuilder (List.hd el) in
          let zero = const_int i32_t 0 in
          let s = build_in_bounds_gep s [| zero |] "" llbuilder in
          build_call printf [| s |] "" llbuilder
        | _       -> build_global_stringptr "Hi" "" llbuilder) fname
  |   ArrayPrimitive el   -> build_global_stringptr "Hi" "" llbuilder
  |   Null                -> build_global_stringptr "Hi" "" llbuilder

let codegen_stmt llbuilder = function
      Block sl        -> build_global_stringptr "Hi" "" llbuilder
  |   Expr e          -> codegen_expr llbuilder e
  |   Return e        -> build_global_stringptr "Hi" "" llbuilder
  |   If (e, s1, s2)       -> build_global_stringptr "Hi" "" llbuilder
  |   For (e1, e2, e3 ,s)  -> build_global_stringptr "Hi" "" llbuilder
  |   While (e, s)    -> build_global_stringptr "Hi" "" llbuilder
  |   Break           -> build_global_stringptr "Hi" "" llbuilder    
  |   Continue        -> build_global_stringptr "Hi" "" llbuilder

let codegen_func fdecl = 
    let handle_func = function
      FName "main" -> 
        let fty = function_type i32_t [| |] in
        let f = define_function "main" fty the_module in
        let llbuilder = builder_at_end context (entry_block f) in
        let _ = codegen_stmt llbuilder (List.hd fdecl.body) in
        build_ret (const_int i32_t 0) llbuilder 
      | _ -> build_global_stringptr "Hi" "" builder 
    in
    handle_func fdecl.fname

let codegen_cdecls cdecls = 
    let handle_cdecl cdecl = 
      codegen_func (List.hd cdecl.cbody.methods)
    in
    let rec handle_cdecls = function
      [] -> the_module
      | h :: t -> ignore(handle_cdecl h); handle_cdecls t
    in 
    handle_cdecls cdecls