open Config
open Libs.Board
open Libs.Generator
open To_algebraic
let main =
  print_newline ();
  let position_roi = index_array board (king !white_to_move) in
  let roi_en_echec = threatened board position_roi !white_to_move in
  print_board board;
  let word =
    "Note : " ^ (string_of_int (evaluation board !white_to_move position_roi roi_en_echec (-99999) 99999)) ^ "\n" ^ san_of_move_list !move_record chessboard !last_move !castling_right
  in print_endline word

let () = main