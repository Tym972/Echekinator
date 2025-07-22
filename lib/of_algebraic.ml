(*Module implémentant les fonctions qui permettent de traduire les coups de la notation algébrique vers la notation avec le type Mouvement*)

open Board
open Generator

(*Fonction supprimant les caractères dispensables de la notation algébrique*)
let remove chain =
  let nc = ref "" in
  for i = 0 to ((String.length chain) - 1) do
    let k = chain.[i] in
    if not (List.mem k ['x'; '('; ')'; '+'; '.'; '?'; '!'; '"'; '\n']) then begin
      nc := !nc ^ (String.make 1 k)
    end
  done;
  !nc

(*Dictionnaire associant une pièce en notation algébrique anglaise à la valeur des pièces pour le moteur*)
let hash_pieces =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', 4); ('N', 2); ('B', 3); ('Q', 5); ('K', 6)];
  ht

(*Fonction traduisant une capture en passant de la notation algébrique vers la notation avec le type Mouvement*)
let ep_origin move white_to_move =
  let to_ = Hashtbl.find hash_coord ((String.sub move 1 1) ^ (String.sub move 2 1)) in
  let player_sign = if white_to_move then 1 else (-1) in
  let from = Hashtbl.find hash_coord ((String.sub move 0 1) ^ string_of_int (int_of_string (String.sub move 2 1) - player_sign))
  in Enpassant {from = from ; to_ = to_}

(*Fonction traduisant une notation algébrique exhaustive vers la notation avec le type Mouvement*)
let normal_origin move white_to_move board =
  let player_sign = if white_to_move then 1 else (-1) in
  let piece = player_sign * Hashtbl.find hash_pieces move.[0] in
  let from = Hashtbl.find hash_coord ((String.sub move 1 1) ^ (String.sub move 2 1)) in
  let to_ = Hashtbl.find hash_coord ((String.sub move 3 1) ^ (String.sub move 4 1)) in
  Normal {piece = piece; from = from; to_ = to_; capture = board.(to_)}

(*Fonction traduisant le move d'un pawn de la notation algébrique vers la notation avec le type Mouvement*)
let pawn_origin board move white_to_move =
  let from = ref (-1) in
  let l = String.length move in
  let square = Hashtbl.find hash_coord ((String.sub move (l - 2) 1) ^ (String.sub move (l - 1) 1)) in
  let player_sign = if white_to_move then 1 else (-1) in
  if String.length move = 2 then begin
    if board.(square + 8 * player_sign) = player_sign then begin
      from := square + 8 * player_sign
    end
    else begin
      from := square + 16 * player_sign
    end
  end
  else begin
    from := Hashtbl.find hash_coord ((String.sub move 0 1) ^ string_of_int ((int_of_string (String.sub move 2 1)) - player_sign))
  end;
  Normal {piece = (pawn white_to_move); from = !from; to_ = square; capture = board.(square)}

(*Fonction traduisant le move d'une tour de la notation algébrique vers la notation avec le type Mouvement*)
let distance_origin board move player_legal_moves piece vect_piece =
  let from = ref (-1) in
  let l = String.length move in
  let square = Hashtbl.find hash_coord ((String.sub move (l - 2) 1) ^ (String.sub move (l - 1) 1)) in
  if String.length move = 3 then begin
    let t = tab64.(square) in
    let i = ref 0 in
    while (!from = (-1) && !i < Array.length vect_piece) do
      let dir = vect_piece.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidate = tab120.(t + (!k * dir)) in
        let dest = board.(candidate) in
        if dest = piece && (List.mem (Normal {piece = piece; from = candidate; to_ = square; capture = board.(square)}) player_legal_moves) then begin
          from := candidate;
          s := false
        end
        else if dest = 0 then begin
          k := !k + 1
        end
        else begin
          s := false
        end
      done;
      i := !i + 1
    done
  end
  else begin
    let x = (int_of_char move.[1]) in
    if (x > 48 && x < 57) then begin
      let square_0 = Hashtbl.find hash_coord ("a" ^ (String.sub move 1 1)) in
      let i = ref 0 in
      while (!from = (-1) && !i < 8) do
        let candidate = square_0 + !i in
        if board.(candidate) = piece && (List.mem (Normal {piece = piece; from = candidate; to_ = square; capture = board.(square)}) player_legal_moves) then begin
          from := candidate
        end;
        i := !i + 1
      done
    end
    else begin
      let square_0 = Hashtbl.find hash_coord ((String.sub move 1 1) ^ "8") in
      let i = ref 0 in
      while (!from = (-1) && !i < 8) do
        let candidate = square_0 + (8 * !i) in
        if board.(candidate) = piece && (List.mem (Normal {piece = piece; from = candidate; to_ = square; capture = board.(square)}) player_legal_moves) then begin
          from := candidate
        end;
        i := !i + 1
      done
    end
  end;
  Normal {piece = piece; from = !from; to_ = square; capture = board.(square)}

(*Fonction traduisant une promotion en notation algébrique vers la notation avec le type Mouvement*)
let origine_promotion move white_to_move board =
  let from = ref (-1) in
  let l = String.length move in
  let square = Hashtbl.find hash_coord ((String.sub move (l - 4) 1) ^ (String.sub move (l - 3) 1)) in
  let promo = ref 0 in
  let player_sign = if white_to_move then 1 else (-1) in
  promo := player_sign * Hashtbl.find hash_pieces (Char.uppercase_ascii move.[l - 1]);
  if String.length move = 4 then begin
    from := square + player_sign * 8
  end
  else begin
    from := Hashtbl.find hash_coord ((String.sub move 0 1) ^ string_of_int ((int_of_string (String.sub move 2 1)) - player_sign))
  end;
  Promotion {from = !from; to_ = square; promotion = !promo; capture = board.(square)}

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

(*Dictionnaire associant une pièce en notation algébrique anglaise à la fonction à appliquer pour trouver sa square de départ*)
let hash_origin =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', (4, rook_vect)); ('N', (2, knight_vect)); ('B', (3, bishop_vect));
      ('Q', (5, king_vect)); ('K', (6, king_vect))];
  ht

(*Traduit un move noté en notation algébrique en sa notation avec le type mouvement*)
let move_of_algebric board move white_to_move player_legal_moves =
  let translated_move = ref Null in
  if List.mem move ["0-0"; "O-O"] then begin
    translated_move := if white_to_move then Castling {sort = 1} else Castling {sort = 3}
  end
  else if List.mem move ["0-0-0"; "O-O-O"] then begin
    translated_move := if white_to_move then Castling {sort = 2} else Castling {sort = 4}
  end
  else if String.contains move 'p' then begin
    translated_move := ep_origin move white_to_move
  end
  else if String.contains move '=' then begin
    translated_move := origine_promotion move white_to_move board
  end
  else begin match String.length move with
    |2 |3 when move.[0] = Char.lowercase_ascii move.[0] && move.[0] <> move.[1] -> translated_move := pawn_origin board move white_to_move
    |5 -> translated_move := normal_origin move white_to_move board
    |_->
      let piece, vect_piece = (Hashtbl.find hash_origin move.[0]) in
      translated_move := distance_origin board move player_legal_moves (if white_to_move then piece else (- piece)) vect_piece
  end;
  if not (List.mem !translated_move player_legal_moves) then begin
    failwith "Invalid Move"
  end
  else begin
    !translated_move
  end

(*Dictionnaire associant la notation algébrique française des pièces à leur notation algébrique anglaise*)
let dicofrench =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('T', "R"); ('C', "N"); ('F', "B");
      ('D', "Q"); ('R', "K")];
  ht

(*Fonction interprétant la notation UCI*)
let mouvement_of_uci uci board player_legal_moves =
  let move = ref Null in
    let from = Hashtbl.find hash_coord (String.sub uci 0 2) in
    let to_ = Hashtbl.find hash_coord (String.sub uci 2 2) in
    let promotion = try Hashtbl.find hash_pieces (Char.uppercase_ascii uci.[4]) with _ -> 10 in
    let white_to_move = board.(from) > 0 in
    let player_sign = if white_to_move then 1 else (-1) in
    if promotion <> 10 then begin
      move := Promotion {from; to_; capture = board.(to_); promotion = player_sign * promotion}
    end
    else if board.(from) = 6 * player_sign && (to_ / 8 = from / 8) && (abs (to_ - from) <> 1 || board.(to_) * player_sign > 0) then begin
      let player_castling = if white_to_move then 1 else 3 in
      let arrivee_pr, depart_tour_pr, to_long, from_long_rook = if white_to_move then 62, !from_short_white_rook, 58, !from_long_white_rook else 6, !from_short_black_rook, 2, !from_long_black_rook in
      if (not !chess_960 && to_ = arrivee_pr) || (!chess_960 && to_ = depart_tour_pr) then begin
        move := Castling {sort = player_castling}
      end
      else if (not !chess_960 && to_ = to_long) || (!chess_960 && to_ = from_long_rook) then begin
        move := Castling {sort = player_castling + 1}
      end
    end
    else if board.(from) = player_sign && (from - to_) mod 8 <> 0 && board.(to_) = 0 then begin
      move := Enpassant {from; to_}
    end
    else begin
      move := Normal {piece = board.(from); from; to_; capture = board.(to_)}
    end;
    if not (List.mem !move player_legal_moves) then begin
      failwith "Invalid Move"
    end
    else begin
      !move
    end
  
(*Fonction permettant une tolérance à l'approximation de l'utilisateur dans sa saisie*)
let tolerance board move white_to_move player_legal_moves =
  try mouvement_of_uci move board player_legal_moves with _ ->
  try move_of_algebric board move white_to_move player_legal_moves with _ ->
  try move_of_algebric board (move ^ "ep") white_to_move player_legal_moves with _ ->
  try move_of_algebric board (String.capitalize_ascii move) white_to_move player_legal_moves with _ ->
  try move_of_algebric board (Hashtbl.find dicofrench move.[0] ^ String.sub move 1 (String.length move - 1)) white_to_move player_legal_moves with _ ->
  try move_of_algebric board (Hashtbl.find dicofrench (Char.uppercase_ascii move.[0]) ^ String.sub move 1 (String.length move - 1)) white_to_move player_legal_moves with _ -> Null

(*Fonction convertissant un relevé de coups notés algébriquement en un relevé de coups notés avec le type Mouvement*)
let move_list_of_algebric_list algebraic_list initial_white_to_move initial_last_move initial_castling_right start_position =
  let board = Array.copy start_position in
  let white_to_move = ref initial_white_to_move in
  let last_move = ref initial_last_move in
  let right_to_castle = ref initial_castling_right in
  let algebraic_list = ref algebraic_list in
  let move_list = ref [] in
  let verification = ref true in
  while !verification && !algebraic_list <> [] do
    let move = List.hd !algebraic_list in
    let king_position = (index_array board (king !white_to_move)) in
    let player_legal_moves = legal_moves board !white_to_move !last_move !right_to_castle king_position (threatened board king_position !white_to_move) in
    let translated_move = tolerance board move !white_to_move player_legal_moves in
    if translated_move <> Null then begin
      move_list := translated_move :: !move_list;
      make_move_1 board translated_move white_to_move last_move right_to_castle;
      algebraic_list := List.tl !algebraic_list
    end
    else begin
      verification := false
    end
  done;
  List.rev !move_list

(*Fonction convertissant la notation d'un string de coups notés algébriquement en une list de coups notés avec le type Mouvement*)
let move_list_of_san san initial_white_to_move initial_last_move initial_castling_right start_position =
  move_list_of_algebric_list (algebric_list_of_san san) initial_white_to_move initial_last_move initial_castling_right start_position