(*Module implémentant les fonctions qui permettent de traduire les coups de la notation algébrique vers la notation avec le type Mouvement*)

open Board
open Bitboards

let get_move_promotion move =
  match get_move_flag move with
  |8 |12 -> 2
  |9 | 13 -> 3
  |10 | 14 -> 4
  |11 | 15 -> 5
  |_ -> 0

(*Tableau assoicant la valeur des pièces pour le moteur (indice) à leur notation algébrique anglaise*)
let english_pieces_lowercase = [|""; "p"; "n"; "b"; "r"; "q"; "k"|]

let uci_of_mouvement move =
  let from = get_move_from move in
  let to_ = get_move_to move in
  if !chess_960 && (get_move_flag move = 2 || get_move_flag move = 3) then begin
    coord.(from) ^ coord.(get_move_to move)
  end
  else if get_move_flag move land 8 <> 0 then begin
    coord.(from) ^ coord.(to_) ^ english_pieces_lowercase.(get_move_promotion move mod 6)
  end
  else if from + to_ <> 0 then begin
    coord.(from) ^ coord.(to_)
  end
  else begin
    "(none)"
  end

(*Dictionnaire associant une pièce en notation algébrique anglaise à la valeur des pièces pour le moteur*)
let hash_pieces =
  let ht = Hashtbl.create 5 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', 4); ('N', 2); ('B', 3); ('Q', 5); ('K', 6)];
  ht

(* Hash table mapping chessboard coordinates to indices in the coord array *)
let hash_coord =
  let ht = Hashtbl.create 64 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ("a1", 0);  ("b1", 1);  ("c1", 2);  ("d1", 3);  ("e1", 4);  ("f1", 5);  ("g1", 6);  ("h1", 7);
      ("a2", 8);  ("b2", 9);  ("c2", 10); ("d2", 11); ("e2", 12); ("f2", 13); ("g2", 14); ("h2", 15);
      ("a3", 16); ("b3", 17); ("c3", 18); ("d3", 19); ("e3", 20); ("f3", 21); ("g3", 22); ("h3", 23);
      ("a4", 24); ("b4", 25); ("c4", 26); ("d4", 27); ("e4", 28); ("f4", 29); ("g4", 30); ("h4", 31);
      ("a5", 32); ("b5", 33); ("c5", 34); ("d5", 35); ("e5", 36); ("f5", 37); ("g5", 38); ("h5", 39);
      ("a6", 40); ("b6", 41); ("c6", 42); ("d6", 43); ("e6", 44); ("f6", 45); ("g6", 46); ("h6", 47);
      ("a7", 48); ("b7", 49); ("c7", 50); ("d7", 51); ("e7", 52); ("f7", 53); ("g7", 54); ("h7", 55);
      ("a8", 56); ("b8", 57); ("c8", 58); ("d8", 59); ("e8", 60); ("f8", 61); ("g8", 62); ("h8", 63)];
  ht

let move_array_mem move legal_moves number_of_legal_moves =
  let mem = ref false in
  let i = ref 0 in
  while !i < number_of_legal_moves && not !mem do
    if legal_moves.(!i) = move then begin
      mem := true
    end;
    incr i
  done;
  !mem

(*Fonction interprétant la notation UCI*)
let mouvement_of_uci uci position =
  let white_to_move = position.white_to_move in
  let from = Hashtbl.find hash_coord (String.sub uci 0 2) in
  let to_ = ref (Hashtbl.find hash_coord (String.sub uci 2 2)) in


  let piece = (position.board.(from)) in
  let promotion_piece = try Hashtbl.find hash_pieces (Char.uppercase_ascii uci.[4]) with _ -> 0 in
  let capture = if position.board.(!to_) = 0 then 0 else 4 in
  let player_castling_infos = castling_infos.(white_to_move) in
  let flag = 
    if piece = pawn + 6 * white_to_move && (from - !to_) mod 8 <> 0 && capture = 0 then 
      5
    else if piece = pawn + 6 * white_to_move && abs (from - !to_) = 16 then
      1
    else if piece = king + 6 * white_to_move && from = player_castling_infos.from_king && (!to_ = player_castling_infos.to_short_king || (!chess_960 && !to_ = player_castling_infos.from_short_rook)) then
      2
    else if piece = king + 6 * white_to_move && from = player_castling_infos.from_king && (!to_ = player_castling_infos.to_long_king || (!chess_960 && !to_ = player_castling_infos.from_long_rook)) then
      3
    else if promotion_piece <> 0 then
      (promotion_piece + 6) lor capture
    else
      capture
  in
  encode_move from !to_ flag