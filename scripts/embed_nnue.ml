let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let write_module path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel "let contents = ";
      output_string channel (Printf.sprintf "%S" contents);
      output_char channel '\n')

let () =
  match Sys.argv with
  | [| _; input_path; output_path |] ->
    write_module output_path (read_file input_path)
  | _ ->
    prerr_endline "Usage: embed_nnue INPUT.nnue OUTPUT.ml";
    exit 2