open Libs.Board
open Libs.Fen
open Libs.Bitboards
open Libs.Miscellaneous
open Libs.Uci

let nodes_total = ref 0

type perft_test =
  {mutable fen : string;
  mutable depth : int;
  mutable result : int}

let algoperftime position depth =
  let start_time = Mtime_clock.counter () in
  let fx = algoperft position depth 0 in
  let exec_time =
    let span = Mtime_clock.count start_time in
    Mtime.Span.to_float_ns span /. 1e9
  in fx, exec_time

let perft position depth =
  let nodes, time = algoperftime position depth in
  nodes_total := !nodes_total + nodes;
  print_newline ();
  print_board position.board;
  print_endline (fen position);
  print_endline ("\nPerft " ^ (string_of_int depth));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)));
  nodes

let go_perft perft_test number  =
  let start_time = Mtime_clock.counter () in
  let position = create_position () in
  for i = 0 to number - 1 do
    let test = perft_test.(i) in
    let fen = test.fen in
    let depth = test.depth in
    let result = test.result in
    uninitialized := true;
    position_uci (word_detection ("position fen " ^ fen)) position;
    print_endline (Printf.sprintf "#%i" (i + 1));
    let nodes = perft position depth in if nodes <> result then begin
      print_endline "ERREUR";
      raise Exit
    end;
    print_newline ()
  done;
  let exec_time =
    let span = Mtime_clock.count start_time in
    Mtime.Span.to_float_ns span /. 1e9
  in print_endline ("Total time (s) : " ^ (string_of_float exec_time));
  print_endline ("Nodes searched : " ^ (string_of_int !nodes_total));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int !nodes_total)/. exec_time)));
  print_endline "We are clear"

let process_line perft_list index line depth =
  let normalized = Str.global_replace (Str.regexp "[ \t]+") " " (String.trim line) in
  let tokens = String.split_on_char ' ' normalized in
  match tokens with
  | _num :: f1 :: f2 :: f3 :: f4 :: f5 :: f6 :: rest when rest <> [] ->
      let fen = String.concat " " [f1; f2; f3; f4; f5; f6] in
      let perft = List.nth rest (depth - 1) in
      let test = perft_list.(index) in
      test.fen <- fen;
      test.depth <- depth;
      test.result <- int_of_string perft;
  | _ -> ()

let process_file filename depth number =
  let ic = open_in filename in
  let perft_test = Array.init 960 (fun _ -> {fen = ""; depth = 0; result = 0}) in
  begin try
    let rec aux i =
      let line = input_line ic in
      if String.trim line <> "" then process_line perft_test i line depth;
      aux (i + 1)
    in
    aux 0
  with End_of_file ->
    close_in ic
  end;
  chess_960 := true;
  go_perft perft_test number

let () = process_file "perft960.txt" 5 960