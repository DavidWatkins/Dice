open Sast
open Processor

module Includes = Map.Make(String)
module Env = Map.Make(String)
module StringMap = Map.Make (String)

let process_includes filename (includes, classes) =
  (* Bring in each include  *)
  let processInclude include_statement = 
    let f = open(include_statement) in
    let lexbuf = Lexing.from_channel file_in f in
    let token_list = Processor.build_token_list lexbuf in
    let program = Processor.parser include_statement token_list in
    program
  in
  let rec iterate_includes classes m = function
      [] -> classes
    | h :: t -> 
      (* Check each include against the map *)
      let x = if StringMap.mem h m then ([], []) else processInclude h in
      iterate_includes (classes @ fst x) (StringMap.add h true) (snd x @ t)
  in
  iterate_includes classes (StringMap.add filename true) includes