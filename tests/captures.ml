open Config
open Libs.Board
open Libs.Generator

let main =
  print_newline ();
  print_board board;
  let player_legal_moves = legal_moves board !white_to_move !last_move !castling_right in
  print_string "Coups : "; affiche_liste player_legal_moves board player_legal_moves; print_newline ();
  print_string "Prises : "; affiche_liste (captures board !white_to_move !last_move) board player_legal_moves; print_newline ()

let () = main