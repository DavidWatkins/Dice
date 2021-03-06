open Filename
open Unix

exception Safe_exception of (string * string list ref)

let raise_safe fmt =
  let do_raise msg = raise @@ Safe_exception (msg, ref []) in
  Printf.ksprintf do_raise fmt

let reraise_with_context ex fmt =
  let do_raise context =
    let () = match ex with
    | Safe_exception (_, old_contexts) -> old_contexts := context :: !old_contexts
    | _ -> Printf.eprintf "warning: Attempt to add note '%s' to non-Safe_exception!" context
    in
    raise ex
  in Printf.ksprintf do_raise fmt

module StringMap = struct
  include Map.Make(String)
  let find_nf = find
  let find_safe key map = try find key map with Not_found -> raise_safe "BUG: Key '%s' not found in StringMap!" key
  let find key map = try Some (find key map) with Not_found -> None
  let map_bindings fn map = fold (fun key value acc -> fn key value :: acc) map []
end

type path_component =
  | Filename of string  (* foo/ *)
  | ParentDir           (* ../ *)
  | CurrentDir          (* ./ *)
  | EmptyComponent      (* / *)

type filepath = string


let on_windows = Filename.dir_sep <> "/"

let path_is_absolute path = not (Filename.is_relative path)

let string_tail s i =
  let len = String.length s in
  if i > len then failwith ("String '" ^ s ^ "' too short to split at " ^ (string_of_int i))
  else String.sub s i (len - i)

let split_path_str path =
  let l = String.length path in
  let is_sep c = (c = '/' || (on_windows && c = '\\')) in

  (* Skip any leading slashes and return the rest *)
  let rec find_rest i =
    if i < l then (
      if is_sep path.[i] then find_rest (i + 1)
      else string_tail path i
    ) else (
      ""
    ) in

  let rec find_slash i =
    if i < l then (
      if is_sep path.[i] then (String.sub path 0 i, find_rest (i + 1))
      else find_slash (i + 1)
    ) else (
      (path, "")
    )
  in
  find_slash 0

let split_first path =
  if path = "" then
    (CurrentDir, "")
  else (
    let (first, rest) = split_path_str path in
    let parsed =
      if first = Filename.parent_dir_name then ParentDir
      else if first = Filename.current_dir_name then CurrentDir
      else if first = "" then EmptyComponent
      else Filename first in
    (parsed, rest)
  )

let normpath path : filepath =
  let rec explode path =
    match split_first path with
    | CurrentDir, "" -> []
    | CurrentDir, rest -> explode rest
    | first, "" -> [first]
    | first, rest -> first :: explode rest in

  let rec remove_parents = function
    | checked, [] -> checked
    | (Filename _name :: checked), (ParentDir :: rest) -> remove_parents (checked, rest)
    | checked, (first :: rest) -> remove_parents ((first :: checked), rest) in

  let to_string = function
    | Filename name -> name
    | ParentDir -> Filename.parent_dir_name
    | EmptyComponent -> ""
    | CurrentDir -> assert false in
  String.concat Filename.dir_sep @@ List.rev_map to_string @@ remove_parents ([], explode path)


let abspath path =
  let (+/) = Filename.concat in
  normpath (
    if path_is_absolute path then path
    else (Sys.getcwd ()) +/ path
  )

let realpath path =
  let (+/) = Filename.concat in   (* Faster version, since we know the path is relative *)

  (* Based on Python's version *)
  let rec join_realpath path rest seen =
    (* Printf.printf "join_realpath <%s> + <%s>\n" path rest; *)
    (* [path] is already a realpath (no symlinks). [rest] is the bit to join to it. *)
    match split_first rest with
    | Filename name, rest -> (
      (* path + name/rest *)
      let newpath = path +/ name in
      let link = try Some (Unix.readlink newpath) with Unix.Unix_error _ -> None in
      match link with
      | Some target ->
          (* path + symlink/rest *)
          begin match StringMap.find newpath seen with
          | Some (Some cached_path) -> join_realpath cached_path rest seen
          | Some None -> (normpath (newpath +/ rest), false)    (* Loop; give up *)
          | None ->
              (* path + symlink/rest -> realpath(path + target) + rest *)
              match join_realpath path target (StringMap.add newpath None seen) with
              | path, false ->
                  (normpath (path +/ rest), false)   (* Loop; give up *)
              | path, true -> join_realpath path rest (StringMap.add newpath (Some path) seen)
          end
      | None ->
          (* path + name/rest -> path/name + rest (name is not a symlink) *)
          join_realpath newpath rest seen
    )
    | CurrentDir, "" ->
        (path, true)
    | CurrentDir, rest ->
      (* path + ./rest *)
      join_realpath path rest seen
    | ParentDir, rest ->
      (* path + ../rest *)
      if String.length path > 0 then (
        let name = Filename.basename path in
        let path = Filename.dirname path in
        if name = Filename.parent_dir_name then
          join_realpath (path +/ name +/ name) rest seen    (* path/.. +  ../rest -> path/../.. + rest *)
        else
          join_realpath path rest seen                      (* path/name + ../rest -> path + rest *)
      ) else (
        join_realpath Filename.parent_dir_name rest seen    (* "" + ../rest -> .. + rest *)
      )
    | EmptyComponent, rest ->
        (* [rest] is absolute; discard [path] and start again *)
        join_realpath Filename.dir_sep rest seen
  in

  try
    if on_windows then
      abspath path
    else (
      fst @@ join_realpath (Sys.getcwd ()) path StringMap.empty
    )
  with Safe_exception _ as ex -> reraise_with_context ex "... in realpath(%s)" path