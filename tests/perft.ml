open Libs.Board
open Libs.Generator
open Libs.Fen
open Config
open Libs.Of_algebraic
open Positions

let rec algoperft board white_to_move last_move castling_right depth =
  if depth = 0 then begin
    1
  end
  else begin
    let king_position = index_array board (king white_to_move) in
    let in_check = threatened board king_position white_to_move in
    let cp = ref (legal_moves board white_to_move last_move castling_right king_position in_check) in
    let nodes = ref 0 in
    while !cp <> [] do
      let coup = List.hd !cp in
      make board coup;
      cp := List.tl !cp;
      nodes := !nodes + (algoperft board (not white_to_move) coup (modification_roque coup castling_right) (depth - 1));
      unmake board coup
    done;
    !nodes 
  end

let algoperftime board white_to_move historique castling_right depth =
  let t = Sys.time () in
  let fx = algoperft board white_to_move historique castling_right depth in
  fx, (Sys.time () -. t)

let perft depth board =
  let nodes, time = algoperftime board !white_to_move !last_move !castling_right depth in
  print_newline ();
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_endline ("\nPerft " ^ (string_of_int depth));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))

let perft_list list =
  let t = Sys.time () in
  let rec aux list = match list with
    |[] -> ()
    |fen_chain :: t ->
      position_of_fen fen_chain start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_move_record initial_board_record;
      reset board white_to_move last_move castling_right king_position in_check move_record board_record start_position !initial_white_to_move !initial_last_move !initial_castling_right !initial_king_position !initial_in_check !initial_move_record !initial_board_record;
      make_list (move_list_of_san move_list !white_to_move !last_move !castling_right board) board last_move move_record board_record castling_right white_to_move;
      perft perft_depth board;
      print_newline ();
      aux t
  in aux list;
  print_endline ("Total time (s) : " ^ (string_of_float (Sys.time () -. t)))


let () = if true then perft perft_depth board else perft_list perft_test