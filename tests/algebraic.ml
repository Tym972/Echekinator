(*Module implémentant les fonctions qui permettent de traduire les coups de la notation algébrique vers la notation avec le type Mouvement*)

open Libs.Bitboards

(*Tableau assoicant la valeur des pièces pour le moteur (indice) à leur notation algébrique anglaise*)
let english_pieces_lowercase = [|""; "p"; "n"; "b"; "r"; "q"; "k"|]

(*Fonction supprimant les caractères dispensables de la notation algébrique*)
let remove chain =
  let reg = Str.regexp "ep\\|[x()+.?!\"\n]" in
  Str.global_replace reg "" chain

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

(*Fonction traduisant une capture en passant de la notation algébrique vers la notation avec le type Mouvement*)
let ep_origin move white_to_move =
  let rank = if white_to_move = 0 then "5" else "4" in
  let to_ = Hashtbl.find hash_coord (String.sub move 1 2) in
  let from = Hashtbl.find hash_coord ((String.sub move 0 1) ^ rank)
  in encode_move from to_ 5

(*Fonction traduisant le move d'un pawn de la notation algébrique vers la notation avec le type Mouvement*)
let pawn_origin position move =
  let length = String.length move in
  let white_to_move = position.white_to_move in
  let to_ = Hashtbl.find hash_coord (String.sub move (length - 2) 2) in
  let from, flag =
    if length = 2 then begin
      if position.pieces.(pawn + 6 * white_to_move) &&& single_bitboards_tab.(to_ - push_vects.(white_to_move)) <> 0L then begin
        to_ - push_vects.(white_to_move), 0
      end
      else begin
        to_ - 2 * push_vects.(white_to_move), 1
      end
    end
    else begin
      if position.board.(to_) <> 0 then begin
        let dir = if white_to_move = 0 then 1 else (-1) in
        Hashtbl.find hash_coord ((String.sub move 0 1) ^ string_of_int ((int_of_string (String.sub move 2 1)) - dir)), 4
        end
      else begin
        let rank = if white_to_move = 0 then "5" else "4" in
        Hashtbl.find hash_coord ((String.sub move 0 1) ^ rank), 5
      end
    end
  in encode_move from to_ flag

let possible_start piece to_ total_occupancy = match piece with
  |2 | 8 -> generate_knight_attacks to_;
  |3 | 9 -> generate_bishop_attacks to_ total_occupancy
  |4 | 10 -> generate_rook_attacks to_ total_occupancy
  |5 | 11 -> generate_queen_attacks to_ total_occupancy
  |6 | 12-> generate_king_attacks to_
  |_ -> 0L

let is_legal_move position move =
  not (is_attacked (lsb_index position.pieces.(king + 6 * (position.white_to_move))) position.white_to_move ((position.occupancy.(0) ||| position.occupancy.(1) ||| single_bitboards_tab.(get_move_to move)) ^^^ single_bitboards_tab.(get_move_from move)) position.pieces)

(*d*)
let piece_origin position move piece =
  let piece_bitboard = position.pieces.(piece) in
  let total_occupancy = position.occupancy.(0) ||| position.occupancy.(1) in
  let length = String.length move in
  let to_ = Hashtbl.find hash_coord (String.sub move (length - 2) 2) in
  let candidates = piece_bitboard &&& (possible_start piece to_ total_occupancy) in
  let candidates_population = population_count candidates in
  let from =
    if candidates_population = 1 then begin
      lsb_index candidates
    end
    else if String.length move = 5 then begin
      Hashtbl.find hash_coord (String.sub move 1 2)
    end
    else if String.length move = 4 then begin
      let x = (int_of_char move.[1]) in
      if (x > 48 && x < 57) then begin
        lsb_index (candidates &&& ranks.(x - 49))
      end
      else begin
        lsb_index (candidates &&& files.(x - 97))
      end
    end
    else begin
      let bb = ref candidates in
      let real_start = ref 0 in
      while !bb <> 0L do
        let candidate, other_candidates = pop_lsb !bb in
        bb := other_candidates;
        if is_legal_move position (encode_move candidate to_ 0) then begin
          real_start := candidate;
          bb := 0L
        end
      done;
      !real_start
    end
  in let capture =
    if position.occupancy.(position.white_to_move lxor 1) &&& single_bitboards_tab.(to_) = 0L then 0 else 4
  in encode_move from to_ capture

(*Fonction traduisant une promotion en notation algébrique vers la notation avec le type Mouvement*)
let promotion_origin move white_to_move =
  let length = String.length move in
  let to_ = Hashtbl.find hash_coord (String.sub move (length - 4) 2) in
  let promotion_piece = Hashtbl.find hash_pieces (Char.uppercase_ascii move.[length - 1]) in
  let from, capture =
    if length = 4 then begin
      to_ - push_vects.(white_to_move), 0
    end
    else begin
      let dir = if white_to_move = 0 then 1 else (-1) in
      Hashtbl.find hash_coord ((String.sub move 0 1) ^ string_of_int ((int_of_string (String.sub move 2 1)) - dir)), 4
    end
  in encode_move from to_ ((promotion_piece + 6) lor capture)

(*Fonction décomposant une chain de caractère en list de substring correspondants aux mots*)
let word_detection chain =
  Str.split (Str.regexp " +") chain

(*Fonction vérifiant si une chain de caractère représente un entier*)
let is_integer_string chain =
  let i = try int_of_string chain with _ -> (-1) in
  i > 0

(*Fonction convertissant la notation d'un string de coups notés algébriquement, en une list de coups en notation algébrique*)
let algebric_list_of_san algebric =
  let rec remove_move_counter list  = match list with
    |[] -> []
    |h :: t ->
      if is_integer_string h then begin
        (remove_move_counter t)
      end
      else begin
        h :: (remove_move_counter t)
      end
  in remove_move_counter (word_detection (remove algebric))

(*Traduit un move noté en notation algébrique en sa notation avec le type mouvement*)
let move_of_algebric position move =
  let white_to_move = position.white_to_move in
  let player_castling_infos = castling_infos.(white_to_move) in
  let formated_move = remove move in
  let translated_move =
    if List.mem move ["0-0"; "O-O"] then begin
      if white_to_move = 0 then
        encode_move player_castling_infos.from_king player_castling_infos.to_short_king 2
      else
        encode_move player_castling_infos.from_king player_castling_infos.to_short_king 2
    end
    else if List.mem move ["0-0-0"; "O-O-O"] then begin
      if white_to_move = 0 then
        encode_move player_castling_infos.from_king player_castling_infos.to_long_king 3
      else
        encode_move player_castling_infos.from_king player_castling_infos.to_long_king 3
    end
    else if String.contains move '=' then begin
      promotion_origin formated_move position.white_to_move
    end
    else begin match String.length formated_move with
      |2 |3 when move.[0] = Char.lowercase_ascii move.[0] ->
        pawn_origin position formated_move
      |_->
        let piece = 6 * white_to_move + Hashtbl.find hash_pieces move.[0] in
        piece_origin position formated_move piece
    end
  in translated_move