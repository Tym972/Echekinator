open Libs.Board
open Libs.Generator
open Libs.Fen
open Config

let rec algoperft board white_to_move last_move castling_right depth =
  if depth = 0 then begin
    1
  end
  else begin
    let cp = ref (legal_moves board white_to_move last_move castling_right) in
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
 

let () = perft perft_depth board