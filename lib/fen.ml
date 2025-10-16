(*Modules implémentant les traductions FEN*)

open Board
open Generator
open Traduction
open Zobrist

(*Tableau contenant la représentation algébrique des pièces*)
let tabfen_blanc = [|"P"; "N"; "B"; "R"; "Q"; "K"|]

let tabfen_noir = [|"p"; "n"; "b"; "r"; "q"; "k"|]

(*Tableau dont dont les élément sont le strind de l'index*)
let tabstring =
  [|
  "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7";
  "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15";
  "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23";
  "24"; "25"; "26"; "27"; "28"; "29"; "30"; "31";
  "32"; "33"; "34"; "35"; "36"; "37"; "38"; "39";
  "40"; "41"; "42"; "43"; "44"; "45"; "46"; "47";
  "48"; "49"; "50"; "51"; "52"; "53"; "54"; "55";
  "56"; "57"; "58"; "59"; "60"; "61"; "62"; "63"
  |]

(*Tableau utilisé pour expliciter la notation des castlings dans la notation FEN en cas d'ambiguïté*)
let tab_roques = [|"q"; "b"; "c"; "d"; "e"; "f"; "g"; "k"|]

(*Fonction utilisée pour expliciter la notation des castlings dans la notation FEN en cas d'ambiguïté*)
let xfen_castlings board =
  let castlings_representations = [|"K"; "Q"; "k"; "q"|] in
  let aux board from_short_rook from_long_rook white_to_move i =
    let decrement = if white_to_move then 56 else 0 in
    let square = ref decrement in
    let right_square = decrement + 8 in
    let rook = rook white_to_move in
    let short_index = from_short_rook - decrement in
    let long_index = from_long_rook - decrement in
    let maj = if white_to_move then String.uppercase_ascii else fun s -> s in
    while !square < right_square do
      let piece = board.(!square) in
      if piece = rook then begin
        if !square < from_long_rook then begin
          castlings_representations.(i + 1) <- maj (tab_roques.(long_index))
        end
        else if !square > from_short_rook then begin
          castlings_representations.(i) <- maj (tab_roques.(short_index))
        end
      end;
      incr square
    done
  in List.iter (fun (from_short_rook, from_long_rook, white_to_move, i) -> aux board from_short_rook from_long_rook white_to_move i)
  [(!from_short_white_rook, !from_long_white_rook, true, 0); (!from_short_black_rook, !from_long_black_rook, false, 2)];
  castlings_representations

(*Fonction représentant un board en sa notation FEN*)
let fen board white_to_move last_move (white_short, white_long, black_short, black_long) moves_record half_moves =
  let fen = ref "" in
  let empties = ref 0 in
  for i = 0 to 63 do
    let square = board.(i) in
    if square = 0 then begin
      empties := !empties + 1
    end
    else begin
      if !empties > 0 then begin
        fen := !fen ^ (string_of_int !empties) ^ (if square > 0 then tabfen_blanc.(square - 1) else tabfen_noir.(- square - 1));
        empties := 0
      end
      else begin
        fen := !fen ^ (if square > 0 then tabfen_blanc.(square - 1) else tabfen_noir.(- square - 1))
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
  if white_to_move then
    fen := !fen ^ " w "
  else begin
    fen := !fen ^ " b "
  end;
  if not (white_short || white_long || black_short || black_long) then begin
    fen := !fen ^ "-"
  end
  else begin
    let castlings_representations = xfen_castlings board in
    if white_short then fen := !fen ^ castlings_representations.(0);
    if white_long then fen := !fen ^ castlings_representations.(1);
    if black_short then fen := !fen ^ castlings_representations.(2);
    if black_long then fen := !fen ^ castlings_representations.(3)
  end;
  let moves = Array.make 2 Null in
  let number_of_moves = ref 0 in
  enpassant board white_to_move last_move moves number_of_moves;
  fen := !fen ^ " " ^ (if !number_of_moves > 0 then coord.(to_ (moves.(0))) ^ " " else "- ");
  fen := !fen ^ (string_of_int half_moves ^ " ");
  fen := !fen ^ string_of_int (1 + (List.length moves_record)/ 2);
  !fen

(*Dictionnaire associant la repsésentation des pièces dans les tableau-échiquier à une chaîne de caractères*)
let hash_fen =
  let ht = Hashtbl.create 13 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [ ('p', (-1)); ('n', (-2)); ('b', (-3)); ('r', (-4)); ('q', (-5)); ('k', (-6));
    ('P', 1); ('N', 2); ('B', 3); ('R', 4); ('Q', 5); ('K', 6)];
  ht

(*Fonction actualisant le board en fonction de la partie "pièces" du FEN*)
let board_of_fen board lines_list =
  for i = 0 to 63 do
    board.(i) <- 0
  done;
  for i = 0 to 7 do
    let ligne = List.nth lines_list i in
    let j = ref 0 in
    let k = ref 0 in
    while !j < 8 do
      let elt = ligne.[!k] in
      let piece = try Hashtbl.find hash_fen elt with _ ->
        let empties = (int_of_char elt) - 48 in
        j := !j + empties;
        0
      in if piece <> 0 then begin
        board.(8 * i + !j) <- piece;
        incr j
      end;
      incr k
    done
  done

(*Fonction permettant de déduire le dernier coup en fonction d'une capture en passant possible*)
let ep_deduction start_position ep_square white_to_move =
  let square = try Hashtbl.find hash_coord ep_square with _ -> (-1) in
  if square <> (-1) then begin
    if white_to_move && start_position.(square - 8) = 0 && start_position.(square) = 0 && start_position.(square + 8) = (-1) then begin
      Normal {piece = (-1); from = square - 8; to_ = square + 8; capture = 0}
    end
    else if start_position.(square + 8) = 0 && start_position.(square) = 0 && start_position.(square - 8) = 1 then begin
      Normal {piece = 1; from = square + 8; to_ = square - 8; capture = 0}
    end
    else begin
      Null
    end
  end
  else begin
    Null
  end

(*Tableau utilisé pour expliciter la notation des castlings dans la notation FEN en cas d'ambiguïté*)
let hash_castling_xfen =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [('q', 0); ('a', 0); ('b', 1); ('c', 2); ('d', 3); ('e', 4); ('f', 5); ('g', 6); ('k', 7); ('h', 7)];
  ht

(*Fonction vérifiant la validité des castlings d'une position XFEN*)
let valid_castlings start_position castlings castling_right =
  let white_short = ref false in
  let white_long = ref false in
  let black_short = ref false in
  let black_long = ref false in
  let white_castlings_number = ref 0 in
  let black_castlings_number = ref 0 in
  let n = if castlings = "-" then 0 else String.length castlings in
  for i = 0 to (n - 1) do 
    let roque = castlings.[i] in
    if roque = Char.uppercase_ascii roque then begin
      incr white_castlings_number
    end
    else begin
      incr black_castlings_number
    end
  done;
  let from_white_rooks = ref [] in
  let from_black_rooks = ref [] in
  let from_white_king = ref (-1) in
  let from_black_king = ref (-1) in
  let board_aux board white_to_move rook_positions king_position =
    let increment = if white_to_move then 56 else 0 in
    let square = ref increment in
    let right_square = increment + 8 in
    let rook = rook white_to_move in
    let king = king white_to_move in
    while !square < right_square do
      let piece = board.(!square) in
      if piece = rook then begin
        rook_positions := !square :: !rook_positions
      end
      else if piece = king then begin
        king_position := !square
      end;
      incr square
    done
  in List.iter
  (fun (white_to_move, rook_positions, king_position) -> board_aux start_position white_to_move rook_positions king_position)
  [(true, from_white_rooks, from_white_king); (false, from_black_rooks, from_black_king)];
  let index = ref 0 in
  let aux_string king_position castlings_number white_to_move index =
    let increment = if white_to_move then 56 else 0 in
    if castlings_number = 2 then begin
      let result = (try Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[!index]) + increment with _ -> (-2)), (try Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[!index + 1]) + increment with _ -> (-2))
      in index := !index + 2;
      result
    end
    else if castlings_number = 1 then begin
      let from_short_black_rook = (try Hashtbl.find hash_castling_xfen (Char.lowercase_ascii castlings.[!index]) + increment with _ -> (-2)) in
      incr index;
      if from_short_black_rook > king_position then begin
        from_short_black_rook, (-2)
      end
      else begin
        (-2), from_short_black_rook
      end
    end
    else begin
      (-2), (-2)
    end
  in let short_white, long_white = aux_string !from_white_king !white_castlings_number true index
  in let short_black, long_black = aux_string !from_black_king !black_castlings_number false index in
  if !white_castlings_number <> 0 && !from_white_king <> (-1) then begin
    if (long_white <> (-2) && List.exists (fun rook_position -> !from_white_king > rook_position) !from_white_rooks) then begin
      white_long := true
    end;
    if (short_white <> (-2) && List.exists (fun rook_position -> !from_white_king < rook_position) !from_white_rooks) then begin
      white_short := true
    end
  end;
  if !black_castlings_number <> 0 && !from_black_king <> (-1) then begin
    if (long_black <> (-2) && List.exists (fun rook_position -> !from_black_king > rook_position) !from_black_rooks) then begin
      black_long := true
    end;
    if (short_black <> (-2) && List.exists (fun rook_position -> !from_black_king < rook_position) !from_black_rooks) then begin
      black_short := true
    end
  end;
  castling_right := !white_short, !white_long, !black_short, !black_long;
  if !white_short || !black_short || !white_long || !black_long then begin
    let provisional_board = Array.make 64 0
    in let from_long_white = ref (if long_white = (-2) || not !white_long then (-2) else if long_white = 56 then (List.hd (List.rev !from_white_rooks)) else long_white) in
    let from_short_white = ref (if short_white = (-2) || not !white_short then (-1) else if short_white = 63 then (List.hd !from_white_rooks) else short_white) in
    let from_long_black = ref (if long_black = (-2) || not !black_long then (-2) else if long_black = 0 then (List.hd (List.rev !from_black_rooks)) else long_black) in
    let from_short_black = ref (if short_black = (-2) || not !black_short then (-1) else if short_black = 7 then (List.hd !from_black_rooks) else short_black) in
    let occupied_white_list = List.filter (fun square -> square > (-1)) [!from_long_white; !from_short_white; !from_white_king] in
    let occupied_black_list = List.filter (fun square -> square > (-1)) [!from_long_black; !from_short_black; !from_black_king] in
    let aux_depart from white_to_move occupied_list possible =
      if !from < 0 && possible then begin
        let candidate, direction = if !from = (-1) then 7, incr else 0, decr in
        let increment = if white_to_move then 56 else 0 in
        let b = ref true in
        let square = ref (increment + candidate) in
        while !b do
          if List.mem !square occupied_list then
            direction square
          else begin
            from := !square;
            b := false
          end
        done
      end
    in List.iter (fun (from, white_to_move, occupied_list, possible) -> aux_depart from white_to_move occupied_list possible)
    [(from_long_white, true, occupied_white_list, !white_long); (from_short_white, true, occupied_white_list, !white_short); (from_white_king, true, occupied_white_list, (!white_long || !white_short));
    (from_long_black, false, occupied_black_list, !black_long); (from_short_black, false, occupied_black_list, !black_short); (from_black_king, false, occupied_black_list, (!black_long || !black_short))];
    List.iter (fun (position, piece, possible) -> if possible then provisional_board.(position) <- piece)
    [(!from_white_king, 6, (!white_long || !white_short)); (!from_black_king, (-6), (!black_long || !black_short)); (!from_long_white, 4, !white_long); (!from_short_white, 4, !white_short); (!from_long_black, (-4), !black_long); (!from_short_black, (-4), !black_short)];
    castling_update provisional_board
  end

(*Fonction permettant de réinitialiser un board à l'état d'origine*)
let reset board white_to_move last_move castling_right king_position in_check moves_record zobrist_position board_record half_moves start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_half_moves = 
  for i = 0 to 63 do
    board.(i) <- start_position.(i)
  done;
  castling_update start_position;
  white_to_move := initial_white_to_move;
  last_move := initial_last_move;
  castling_right := initial_castling_right;
  king_position := initial_king_position;
  in_check := initial_in_check;
  moves_record := initial_moves_record;
  zobrist_position := initial_zobrist_position;
  board_record := [initial_zobrist_position];
  half_moves := initial_half_moves

(*Fonction traduisant une position FEN en l'int array correspondant. Par défaut si non rensigné, le trait est au blancs, il n'y a plus de castlings, pas de capture en passant, aucun coup joué*)
let position_of_fen chain start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves =
  let split_fen = ref (word_detection chain) in
  let fen_length = List.length !split_fen in
  let pieces_position = (List.nth !split_fen 0) in
  let split_ligne = Str.split (Str.regexp "/") pieces_position in
  board_of_fen start_position split_ligne;
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
    initial_king_position := index_array start_position (king !initial_white_to_move);
    initial_in_check := threatened start_position !initial_king_position !initial_white_to_move
  end
  else begin
    initial_white_to_move := false;
    initial_king_position := index_array start_position (king !initial_white_to_move);
    initial_in_check := threatened start_position !initial_king_position !initial_white_to_move
  end;
  let poussee_pep = ep_deduction start_position (List.nth !split_fen 3) !initial_white_to_move in
  if poussee_pep <> Null then begin
    initial_last_move := poussee_pep
  end;
  valid_castlings start_position (List.nth !split_fen 2) initial_castling_right;
  initial_half_moves := (try int_of_string (List.nth !split_fen 4) with _ -> 0);
  initial_zobrist_position := zobrist start_position !initial_white_to_move !initial_last_move !initial_castling_right;
  initial_board_record := [!initial_zobrist_position];
  let nombre_coup white_to_move coups_complets =
    if white_to_move then begin
      for _ = 1 to try (2 * (int_of_string (List.nth !split_fen 5) - 1)) with _ -> 0 do 
        initial_moves_record := Null :: !initial_moves_record
      done
    end
    else begin
      for _ = 1 to try (2 * (int_of_string coups_complets - 1) + 1) with _ -> 0 do 
        initial_moves_record := Null :: !initial_moves_record
      done
    end
  in nombre_coup !initial_white_to_move (List.nth !split_fen 5)