open Libs.Board
open Libs.Fen
open Libs.Translation
open Positions
open Libs.Uci

let algoperftime stack depth ply =
  let t = Sys.time () in
  let fx = algoperft stack depth ply in
  fx, (Sys.time () -. t)

let perft stack move_counter depth =
  let nodes, time = algoperftime stack depth 0 in
  print_newline ();
  print_board position.board;
  print_endline (fen position move_counter);
  print_endline ("\nPerft " ^ (string_of_int depth));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))

let perft_list list depth =
  let t = Sys.time () in
  let rec aux list = match list with
    |[] -> ()
    |fen_chain :: t ->
      position_uci (word_detection ("position fen " ^ fen_chain)) position move_counter;
      make_list (algebric_list_of_san "") position move_counter;
      perft !stacks.(0) !move_counter depth;
      print_newline ();
      aux t
  in aux list;
  print_endline ("Total time (s) : " ^ (string_of_float (Sys.time () -. t)))


let () = perft_list perft_test 5