(*Modules implémentant les traductions FEN*)

open Miscellaneous
open Board
open Zobrist
open Translation
open Bitboards

(*Tableau contenant la représentation algébrique des pièces*)
let tabfen_blanc = [|"P"; "N"; "B"; "R"; "Q"; "K"|]

let tabfen_noir = [|"p"; "n"; "b"; "r"; "q"; "k"|]

(*Tableau utilisé pour expliciter la notation des castlings dans la notation FEN en cas d'ambiguïté*)
let castling_fen_tab = [|"q"; "b"; "c"; "d"; "e"; "f"; "g"; "k"|]

let castlings_representations = [|"K"; "Q"; "k"; "q"|]

let is_possible_castling castling_rights castling =
  castling_rights land castling = castling

(*Fonction représentant un board en sa notation FEN*)
let fen position move_counter =
  let state = position.state.(position.ply) in
  let mailbox = position.mailbox in
  let castling_rights = state.castling_rights in
  let fen = ref "" in
  let empties = ref 0 in
  for i = 0 to 63 do
    let square = mailbox.(flip i) in
    if square = 0 then begin
      empties := !empties + 1
    end
    else begin
      if !empties > 0 then begin
        fen := !fen ^ (string_of_int !empties) ^ (if square < 7 then tabfen_blanc.(square - 1) else tabfen_noir.(square - 7));
        empties := 0
      end
      else begin
        fen := !fen ^ (if square < 7 then tabfen_blanc.(square - 1) else tabfen_noir.(square - 7))
      end
    end;
    if (i + 1) mod 8 = 0 then begin
      if !empties <> 0 then begin
        fen := !fen ^ (string_of_int !empties);
        empties := 0
      end;
      if i <> 63 then begin
        fen := !fen ^ "/"
      end
    end
  done;
  if !empties > 0 then begin
    fen := !fen ^ (string_of_int !empties)
  end;
  if position.white_to_move = 0 then
    fen := !fen ^ " w "
  else begin
    fen := !fen ^ " b "
  end;
  if castling_rights = 0 then begin
    fen := !fen ^ "-"
  end
  else begin
    if !chess_960 then begin
      castlings_representations.(0) <- "K";
      castlings_representations.(1) <- "Q";
      castlings_representations.(2) <- "k";
      castlings_representations.(3) <- "q";
      let white_rooks = position.pieces.(white_pieces.(rook)) in
      let black_rooks = position.pieces.(black_pieces.(rook)) in
      if Int64.logand white_rooks ambiguity_masks.(0) <> 0L then begin
        castlings_representations.(0) <- String.uppercase_ascii (castling_fen_tab.(white_castling_info.from_short_rook));
      end;
      if Int64.logand white_rooks ambiguity_masks.(1) <> 0L then begin
        castlings_representations.(1) <- String.uppercase_ascii (castling_fen_tab.(white_castling_info.from_long_rook));
      end;
      if Int64.logand black_rooks ambiguity_masks.(2) <> 0L then begin
        castlings_representations.(2) <- castling_fen_tab.(black_castling_info.from_short_rook);
      end;
      if Int64.logand black_rooks ambiguity_masks.(3) <> 0L then begin
        castlings_representations.(3) <- castling_fen_tab.(black_castling_info.from_long_rook);
      end
    end;
    if is_possible_castling castling_rights castling_infos.(0).short_castling then
      fen := !fen ^ castlings_representations.(0);
    if is_possible_castling castling_rights castling_infos.(0).long_castling then
      fen := !fen ^ castlings_representations.(1);
    if is_possible_castling castling_rights castling_infos.(1).short_castling then
      fen := !fen ^ castlings_representations.(2);
    if is_possible_castling castling_rights castling_infos.(1).long_castling then
      fen := !fen ^ castlings_representations.(3)
  end;
  !fen ^ " " ^ (if state.ep_square <> (-1) then coord.(state.ep_square) ^ " " else "- ") ^ string_of_int state.half_moves ^ " " ^ string_of_int (1 + move_counter / 2)

(*Dictionnaire associant la repsésentation des pièces dans les tableau-échiquier à une chaîne de caractères*)
let hash_fen =
  let ht = Hashtbl.create 13 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [ ('P', 1); ('N', 2); ('B', 3); ('R', 4); ('Q', 5); ('K', 6);  ('p', 7); ('n', 8); ('b', 9); ('r', 10); ('q', 11); ('k', 12);];
  ht

(*Tableau utilisé pour expliciter la notation des castlings dans la notation FEN en cas d'ambiguïté*)
let hash_castling_xfen =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [('q', 0); ('a', 0); ('b', 1); ('c', 2); ('d', 3); ('e', 4); ('f', 5); ('g', 6); ('k', 7); ('h', 7)];
  ht

(*Fonction traduisant une position FEN en l'int array correspondant. Par défaut si non rensigné, le trait est au blancs, il n'y a plus de castlings, pas de capture en passant, aucun coup joué*)
let position_of_fen chain position move_counter =
  position.ply <- 0;
  let state = position.state.(0) in
  let mailbox = position.mailbox in
  let pieces_bitboards = position.pieces in
  state.castling_rights <- 0;
  state.captured_piece <- 0;
  move_counter := 0;
  initial_half_moves := 0;
  position.occupancy.(0) <- 0L;
  position.occupancy.(1) <- 0L;
  for piece = 1 to 12 do
    pieces_bitboards.(piece) <- 0L
  done;
  for square = 0 to 63 do
    mailbox.(square) <- 0
  done;
  let split_fen = ref (word_detection chain) in
  let fen_length = List.length !split_fen in
  let pieces_position = (List.nth !split_fen 0) in
  let split_rows = Str.split (Str.regexp "/") pieces_position in
  for row = 0 to 7 do
    let row_string = List.nth split_rows row in
    let column = ref 0 in
    let row_index = ref 0 in
    while !column < 8 do
      let square = 8 * (7 - row) + !column in
      let elt = row_string.[!row_index] in
      let piece = try Hashtbl.find hash_fen elt with _ ->
        column := !column + (int_of_char elt - 48);
        0
      in mailbox.(square) <- piece;
      if piece <> 0 then begin
        pieces_bitboards.(piece) <- pieces_bitboards.(piece) ||| single_bitboards_tab.(square);
        incr column
      end;
      incr row_index
    done
  done;
  for piece = 1 to 6 do
    position.occupancy.(0) <- position.occupancy.(0) ||| pieces_bitboards.(piece)
  done;
  for piece = 7 to 12 do
    position.occupancy.(1) <- position.occupancy.(1) ||| pieces_bitboards.(piece)
  done;
  let from_white_king = lsb_index pieces_bitboards.(white_pieces.(king)) in
  let from_black_king = lsb_index pieces_bitboards.(black_pieces.(king)) in
  let complete longueur = 
    let rec aux acc longueur = match longueur with
      |5 -> aux ("1" :: acc) 6
      |4 -> aux ("0" :: acc) 5
      |3 | 2 -> aux  ("-" :: acc) (longueur + 1)
      |1 -> aux ("w" :: acc) 2
      |_ -> acc
    in List.rev (aux [] longueur)
  in split_fen := !split_fen @ (complete fen_length);
  if List.nth !split_fen 1 = "w" then begin
    position.white_to_move <- 0
  end
  else begin
    position.white_to_move <- 1
  end;
  let ep_square_string = (List.nth !split_fen 3) in
  if ep_square_string <> "-" then begin
    state.ep_square <- Hashtbl.find hash_coord ep_square_string
  end;
  let castlings = (List.nth !split_fen 2) in
  if castlings <> "-" then begin
    let number_of_white_castlings, number_of_black_castlings =
      String.fold_left
      (fun (white_castlings, black_castlings) c -> 
        if Char.uppercase_ascii c = c then white_castlings + 1, black_castlings else white_castlings, black_castlings + 1
      ) (0, 0) castlings
    in let white_rooks_squares = index_list (Int64.logand rows.(0) position.pieces.(pieces_rep.(0).(rook))) in
    let black_rooks_squares = index_list (Int64.logand rows.(7) position.pieces.(pieces_rep.(1).(rook))) in
    let castling_aux white_to_move number_of_castling player_castling_info from_king rooks_squares index =
      if number_of_castling <> 0 then begin
        if !chess_960 then begin
          player_castling_info.from_king <- from_king
        end;
        let aux castling_column edge_column edge_rook =
          if castling_column = edge_column then begin
            edge_rook
          end
          else begin
            castling_column + white_to_move * 56
          end
        in if number_of_castling = 2 then begin
          state.castling_rights <- state.castling_rights lxor player_castling_info.long_castling lxor player_castling_info.short_castling;
          if !chess_960 then begin
            let long_castling_column = Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[index + 1]) in
            let short_castling_column = Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[index]) in
            if !chess_960 then begin
              player_castling_info.from_long_rook <- aux long_castling_column 0 (List.hd rooks_squares);
              player_castling_info.from_short_rook <- aux short_castling_column 7 (List.hd (List.rev rooks_squares))
            end
          end
        end
        else if number_of_castling = 1 then begin
          let castling_column = Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[index]) in
          if castling_column < from_king mod 8 then begin
            state.castling_rights <- state.castling_rights lxor player_castling_info.long_castling;
            if !chess_960 then begin
              player_castling_info.from_long_rook <- aux castling_column 0 (List.hd rooks_squares)
            end
          end
          else begin
            state.castling_rights <- state.castling_rights lxor player_castling_info.short_castling;
            if !chess_960 then begin
              player_castling_info.from_short_rook <- aux castling_column 7 (List.hd (List.rev rooks_squares))
            end
          end
        end
      end
    in
    castling_aux 0 number_of_white_castlings white_castling_info from_white_king white_rooks_squares 0;
    castling_aux 1 number_of_black_castlings black_castling_info from_black_king black_rooks_squares number_of_white_castlings;
    if !chess_960 then begin
      init_castling_info ()
    end
  end;
  state.half_moves <- (try int_of_string (List.nth !split_fen 4) with _ -> 0);
  state.zobrist <- zobrist position;
  if position.white_to_move = 0 then begin
    move_counter := (try (2 * (int_of_string (List.nth !split_fen 5) - 1)) with _ -> 0)
  end
  else begin
    move_counter := (try (2 * (int_of_string (List.nth !split_fen 5) - 1) + 1) with _ -> 0)
  end;
  state.in_check <- is_attacked (lsb_index pieces_bitboards.(pieces_rep.(position.white_to_move).(king))) (position.white_to_move) (position.occupancy.(0) ||| position.occupancy.(1)) pieces_bitboards pieces_rep.(position.white_to_move lxor 1);
  board_record.(0) <- state.zobrist