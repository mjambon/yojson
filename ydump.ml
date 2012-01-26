(* $Id$ *)

open Printf

(*
let cat std compact stream in_file out_file =
  let ic, fname =
    match in_file with
	`Stdin -> stdin, "<stdin>"
      | `File s -> open_in s, s
  in
  let oc =
    match out_file with
	`Stdout -> stdout
      | `File s -> open_out s
  in
  let finally () =
    if oc != stdout then
      close_out_noerr oc;
    if ic != stdin then
      close_in_noerr ic
  in
  let print x =
    if compact then
      Yojson.Safe.to_channel ~std oc x
    else
      Yojson.Safe.pretty_to_channel ~std oc x;
    output_char oc '\n'
  in
  try
    if stream then
      Stream.iter print (Yojson.Safe.stream_from_channel ~fname ic)
    else
      print (Yojson.Safe.from_channel ~fname ic);
    finally ();
    true
  with e ->
    finally ();
    eprintf "Error:\n";
    (match e with
	 Yojson.Json_error s ->
	   eprintf "%s\n%!" s
       | e ->
	   eprintf "%s\n%!" (Printexc.to_string e)
    );
    false
*)


let polycat write_one streaming in_file out_file =
  let ic, fname =
    match in_file with
	`Stdin -> stdin, "<stdin>"
      | `File s -> open_in s, s
  in
  let oc =
    match out_file with
	`Stdout -> stdout
      | `File s -> open_out s
  in
  let finally () =
    if oc != stdout then
      close_out_noerr oc;
    if ic != stdin then
      close_in_noerr ic
  in
  try
    if streaming then
      Stream.iter (write_one oc) (Yojson.Safe.stream_from_channel ~fname ic)
    else
      write_one oc (Yojson.Safe.from_channel ~fname ic);
    finally ();
    true
  with e ->
    finally ();
    eprintf "Error:\n";
    (match e with
	 Yojson.Json_error s ->
	   eprintf "%s\n%!" s
       | e ->
	   eprintf "%s\n%!" (Printexc.to_string e)
    );
    false


let cat output_biniou std compact streaming in_file out_file =
  if not output_biniou then
    let write_one oc x =
      if compact then
	Yojson.Safe.to_channel ~std oc x
      else
	Yojson.Safe.pretty_to_channel ~std oc x;
      output_char oc '\n'
    in
    polycat write_one streaming in_file out_file
      
  else
    let write_one oc x =
      output_string oc (Bi_io.string_of_tree (Yojson_biniou.biniou_of_json x))
    in
    polycat write_one streaming in_file out_file



let parse_cmdline () =
  let out = ref None in
  let std = ref false in
  let compact = ref false in
  let streaming = ref false in
  let output_biniou = ref false in
  let options = [
    "-o", Arg.String (fun s -> out := Some s), 
    "<file>
          Output file";

    "-std", Arg.Set std,
    "
          Convert tuples and variants into standard JSON,
          refuse to print NaN and infinities,
          require the root node to be either an object or an array.";

    "-c", Arg.Set compact,
    "
          Compact output (default: pretty-printed)";

    "-s", Arg.Set streaming,
    "
          Streaming mode: read and write a sequence of JSON values instead of
          just one.";

    "-ob", Arg.Set output_biniou,
    "     Experimental";
  ]
  in
  let files = ref [] in
  let anon_fun s = 
    files := s :: !files
  in
  let msg =
    sprintf "\
JSON pretty-printer

Usage: %s [input file]" Sys.argv.(0)
  in
  Arg.parse options anon_fun msg;
  let in_file =
    match List.rev !files with
	[] -> `Stdin
      | [x] -> `File x
      | _ ->
	  eprintf "Too many input files\n%!";
	  exit 1
  in
  let out_file =
    match !out with
	None -> `Stdout
      | Some x -> `File x
  in
  !output_biniou, !std, !compact, !streaming, in_file, out_file


let () =
  let output_biniou, std, compact, streaming, in_file, out_file =
    parse_cmdline () in
  let success = cat output_biniou std compact streaming in_file out_file in
  if success then
    exit 0
  else
    exit 1
