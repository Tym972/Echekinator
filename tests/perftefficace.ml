open Libs.Board
open Libs.Fen
open Libs.Uci
open Libs.Transposition
open Config

let table_perft = Array.make transposition_size (0, 0, (-1))

let algoperftime board white_to_move last_move castling_right depth board_record =
  let t = Sys.time () in
  let fx = algoperft board white_to_move last_move castling_right depth true ((List.hd board_record) lxor depth) table_perft in
  fx, (Sys.time () -. t)

let perft depth board =
  let nodes, time = algoperftime board !white_to_move !last_move !castling_right depth !board_record in
  print_newline ();
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_endline ("\nPerft " ^ (string_of_int depth));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft perft_depth board