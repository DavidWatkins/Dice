open Llvm
open Llvm_X86

let add_target_triple triple llm =
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in
  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm ;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm ;
  ()
let _ =
  let llctx = global_context () in
  let llm = create_module llctx "mymodule" in

  (* Types *)
  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let float_t = double_type llctx in
  let struct_t = named_struct_type llctx "test" in
  let ptr_t = pointer_type struct_t in
  let str_t = pointer_type i8_t in
  let _ = struct_set_body struct_t [| i32_t |] true in

    (* Printf *)
  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in
  let malloc_ty = function_type (str_t) [| i32_t |] in
  let malloc = declare_function "malloc" malloc_ty llm in

(* Func *)
  let fty = function_type ptr_t [| ptr_t |] in
  let test_type = define_function "test_type" fty llm in
  let llbuilder = builder_at_end llctx (entry_block test_type) in
  let this = param test_type 0 in
  let _ = set_value_name "this" this in

  (* Func Body *)
  (* let obj = build_alloca struct_t "" llbuilder in *)
  let i = build_struct_gep this 0 "" llbuilder in
  let i = build_load i "" llbuilder in
  let s = build_global_stringptr "Hello, world! %d\n" "" llbuilder in
  let _ = build_call printf [| s; i |] "" llbuilder in

  (* Build obj *)
  let size = build_bitcast (size_of struct_t) i32_t "" llbuilder in
  let obj = build_call malloc [| size |] "" llbuilder in
  let obj = build_bitcast obj ptr_t "" llbuilder in
  let i = build_struct_gep obj 0 "" llbuilder in
  ignore(build_store (const_int i32_t 0) i llbuilder);
  let _ = build_ret obj llbuilder in

  (* Main *)
  let fty = function_type i32_t [| |] in
  let f = define_function "main" fty llm in
  let llbuilder = builder_at_end llctx (entry_block f) in

  (* Code *)
  let s = build_global_stringptr "Hello, world! %d %d\n" "" llbuilder in

  (* Struct code *)
  let obj = build_alloca struct_t "" llbuilder in
  let i = build_struct_gep obj 0 "" llbuilder in
  ignore(build_store (const_int i32_t 12) i llbuilder);
  let obj2 = build_call test_type [| obj |] "" llbuilder in
  let i = build_struct_gep obj 0 "" llbuilder in
  let i2 = build_struct_gep obj2 0 "" llbuilder in
  let i = build_load i "" llbuilder in
  let i2 = build_load i2 "" llbuilder in
  let _ = build_call printf [| s; i; i2 |] "" llbuilder in
(* (*   let i = build_struct_gep obj 0 "" llbuilder in
  let _ = build_store x i llbuilder in *)
  let i = build_struct_gep obj 0 "" llbuilder in
  let i = build_load i "" llbuilder in
  let _ = build_call printf [| s; i |] "" llbuilder in *)

(*   let cast = const_uitofp i float_t in
  let a = build_fadd f cast "test" llbuilder in
  (* try commenting these two lines and compare the result *)
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in
  let _ = build_call printf [| s; a |] "" llbuilder in *)
  let _ = build_ret (const_int i32_t 0) llbuilder in
  (* Llvm_analysis.assert_valid_module llm ; *)
  let _ =
    if Array.length Sys.argv > 1
    then Llvm_bitwriter.write_bitcode_file llm Sys.argv.(1) |> ignore
    else dump_module llm
  in
  ()