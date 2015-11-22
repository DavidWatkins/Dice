open Sast
open Ast
open Processor
open Utils

module Includes = Map.Make(String)
module Env = Map.Make(String)
module StringMap = Map.Make (String)

type global_map = {
    field_map       : Ast.field StringMap.t;
    func_map        : Ast.func_decl StringMap.t;
    constructor_map : Ast.func_decl StringMap.t;
}


let process_includes filename (includes, classes) =
  (* Bring in each include  *)
  let processInclude include_statement = 
    let file_in = open_in filename in
    let lexbuf = Lexing.from_channel file_in in
    let token_list = Processor.build_token_list lexbuf in
    let program = Processor.parser include_statement token_list in
    program
  in
  let rec iterate_includes classes m = function
      [] -> classes
    | (Include h) :: t -> 
      (* Check each include against the map *)
      let result = processInclude h in 
      if StringMap.mem h m then 
        iterate_includes (classes) (StringMap.add h 1 m) (t)
      else 
        (function Program(i, c) -> iterate_includes (classes @ c) (StringMap.add h 1 m) (i @ t) ) result
  in
  iterate_includes classes (StringMap.add filename 1 StringMap.empty) includes

let get_name fdecl = 
  let params = List.fold_left (fun s -> (fun Formal(t, _) -> s ^ ", " ^ Utils.string_of_datatype t)) "" fdecl.formals in
  let name = (function FName x -> x | Constructor -> "constructor") fdecl.fname in
  name ^ " " ^ params


let build_global_map cdecls =
    (* helper global_obj cdecls *)
    let rec helper m = function
        
        [] -> m
      | cdecl :: t -> helper   
        (StringMap.add cdecl.cname 
            { field_map = List.fold_left (fun m -> (function Field(s, d, n) -> (StringMap.add n (Field(s, d, n)) m))) StringMap.empty cdecl.cbody.fields; 
              func_map = List.fold_left (fun m fdecl -> (StringMap.add (get_name fdecl) fdecl m)) StringMap.empty cdecl.cbody.methods;
              constructor_map = List.fold_left (fun m fdecl -> (StringMap.add (get_name fdecl) fdecl m)) StringMap.empty cdecl.cbody.constructors } 
                       m) t in
    helper StringMap.empty cdecls




(* let rec get_expr_type env = function
      Int_Lit i             -> Int_t
  |   Boolean_Lit b         -> Bool_t
  |   Float_Lit f          -> Float_t
  |   String_Lit s          -> Arraytype(Char_t, 1)
  |   Char_Lit c            -> Char_t
  |   This                -> Objecttype(env.name)
  (* This doesn't work, we need to know when this is an object or a type *)
  |   Id s                -> Objecttype(s)
  (* This is a weird case, what does the LRM say? *)
  |   Binop (e1, op, e2)  -> get_expr_type(e1)
  |   Assign (e1, e2)     -> get_expr_type(e1)
  |   Noexpr              -> Void_t
  (* Is this valid? What about if this were an array access? *)
  |   ArrayOp (e1, el)    -> Arraytype(get_expr_type(expr), length exprlist)
  (* Need to create a table to look up field types *)
  |   ObjAccess (e1, e2)  -> get_expr_type(e2)
  (* Need to create a table to look up function types *)
  |   Call (s, el)        -> Void_t
  (* Need a function to affirm array primitives are valid *)
  |   ArrayPrimitive h::t -> get_expr_type(h)
  |   Null                -> Void_t *)

let rec convert_cdecl_to_sast cdecl = 
    (* {
      cname = cdecl.cname;
      fields = cdecl.body.fields;
      methods = cdecl.body.constructors @ cdecl.body.methods;
    }  *)
    cdecl

let convert_cdecls_to_sast cdecls = 
  let rec iterate_cdecls sast = function
     []          -> sast
  |  cdecl::tail -> iterate_cdecls ((convert_cdecl_to_sast cdecl)::sast) tail 
  in iterate_cdecls [] cdecls

(* Main method for analyzer *)
let analyze filename (includes, classes) =
  let cdecls = process_includes filename (includes, classes) in
  let sast = convert_cdecls_to_sast cdecls in
  sast
