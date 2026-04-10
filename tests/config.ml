open Libs.Board
open Libs.Generator
open Libs.Zobrist
open Libs.Evaluation
open Libs.Translation
open Libs.Fen
open Positions
open Libs.Uci

let fen_chain = standard
let move_list = ""

let () = position_uci (word_detection ((Printf.sprintf "position fen %s moves %s" fen_chain move_list))) position move_counter

let evaluation = hce
let depth = 8
let perft_depth = 6

let rec affiche_liste liste board player_legal_moves = match liste with
  |h :: t ->
    let coup = algebraic_of_move h board player_legal_moves in
    if coup <> "" then begin
      print_string (coup ^ " ")
    end
    else begin
      print_string ("erreur ")
    end;
    affiche_liste t board player_legal_moves
  |_ -> ()

let uci_of_list liste_coups =
  let rec aux liste = match liste with
    |[] -> ""
    |h::t -> uci_of_mouvement h ^ " " ^ aux t
  in aux liste_coups

let uci_of_san algebric initial_white_to_move initial_last_move initial_castling_right start_position =
  uci_of_list (move_list_of_san algebric initial_white_to_move initial_last_move initial_castling_right start_position)

let san_of_uci uci initial_white_to_move initial_last_move initial_castling_right start_position =
  san_of_move_list (List.rev (move_list_of_san uci initial_white_to_move initial_last_move initial_castling_right start_position) @ (if initial_white_to_move then [] else [Null])) start_position initial_last_move initial_castling_right