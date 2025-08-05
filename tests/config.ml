open Libs.Board
open Libs.Generator
open Libs.Zobrist
open Libs.Evaluation
open Libs.Of_algebraic
open To_algebraic
open Libs.Fen
open Positions
open Libs.Uci

let board = Array.copy chessboard
let white_to_move = ref true
let last_move = ref Null
let castling_right = ref (true, true, true, true)
let king_position = ref 60
let in_check = ref false
let move_record = ref []
let board_record = ref [zobrist board true !last_move !castling_right]
let start_position = Array.copy chessboard
let initial_white_to_move = ref true
let initial_last_move = ref Null
let initial_castling_right = ref (true, true, true, true)
let initial_king_position = ref 60
let initial_in_check = ref false
let initial_move_record = ref []
let initial_board_record = ref [zobrist board true !last_move !castling_right]

let fen_chain = if false then standard else "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 40"
let move_list = ""

let () = 
  position_of_fen fen_chain start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_move_record initial_board_record;
  reset board white_to_move last_move castling_right king_position in_check move_record board_record start_position !initial_white_to_move !initial_last_move !initial_castling_right !initial_king_position !initial_in_check !initial_move_record !initial_board_record;
  make_list (move_list_of_san move_list !white_to_move !last_move !castling_right board) board last_move move_record board_record castling_right white_to_move

let evaluation = eval1_q
let depth = 8
let perft_depth = 5

let player_legal_moves = legal_moves board !white_to_move !last_move !castling_right

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