open Libs.Board
open Libs.Fen
open Positions
open Libs.Bitboards
open Libs.Miscellaneous
open Libs.Uci

let nodes_total = ref 0

let algoperftime position depth =
  let t = Sys.time () in
  let fx = algoperft position depth in
  fx, (Sys.time () -. t)

let perft position move_counter depth =
  let nodes, time = algoperftime position depth in
  nodes_total := !nodes_total + nodes;
  print_newline ();
  print_board position.mailbox;
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
      perft position !move_counter depth;
      print_newline ();
      aux t
  in aux list;
  let total_time = (Sys.time () -. t) in
  print_endline ("Total time (s) : " ^ (string_of_float total_time));
  print_endline ("Nodes searched : " ^ (string_of_int !nodes_total));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int !nodes_total)/. total_time)))


let () = perft_list perft_test 5