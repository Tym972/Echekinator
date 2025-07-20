(*Module implémentant les fonctions qui permettent de traduire un move de la notation avec le type Mouvement vers la notation algébrique*)

open Board
open Generator

(*Tableau assoicant la valeur des pièces pour le moteur (indice) à leur notation algébrique anglaise*)
let english_pieces = [|""; "P"; "N"; "B"; "R"; "Q"; "K"|]

(*Fonction renvoyant un string (string vide, column de départ, line de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique*)
(*let precise from antecedent precision =
  if (List.length !antecedent) > 0 then begin
    let origin = coord.(from) in
    let column = origin.[0] in
    let line = origin.[1] in
    let rec func list a n = match list with
      |[] -> true
      |h::t -> h.[n] <> a && func t a n
    in if func !antecedent column 0 then begin
      precision := String.make 1 column
    end
    else if func !antecedent line 1 then begin
      precision := String.make 1 line
    end
    else begin
      precision := origin
    end
  end

(*Fonction renvoyant un string (string vide, column de départ, line de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique pour le move d'une tour*)
let precision_piece board from to_ player_legal_moves piece vect_piece =
  let precision = ref "" in
  let antecedent = ref [] in
  let tab64_square = tab64.(to_) in
  for i = 0 to (Array.length vect_piece - 1) do
    let dir = vect_piece.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (tab120.(tab64_square + (!k * dir)) <> (-1) && !iterate) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if (dest = piece && candidate <> from && (List.mem (Normal {piece = piece; from = candidate; to_; capture = board.(to_)}) player_legal_moves)) then begin
        antecedent := coord.(candidate) :: !antecedent;
        iterate := false
      end
      else if dest = 0 then begin
        k := !k + 1
      end
      else begin
        iterate := false
      end
    done
  done;
  precise from antecedent precision;
  !precision

(*Fonction renvoyant un string (string vide, column de départ, line de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique pour le move d'un knight*)
let knight_precision board from to_ white_to_move player_legal_moves =
  let precision = ref "" in
  let antecedent = ref [] in
  let tab64_square = tab64.(to_) in
  let knight = knight white_to_move in
  for i = 0 to 7 do
    let dir = knight_vect.(i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      if (board.(candidate) = knight && candidate <> from && (List.mem (Normal {piece = knight; from = candidate; to_; capture = board.(to_)}) player_legal_moves)) then begin
        antecedent := coord.(candidate) :: !antecedent;
      end
    end
  done;
  precise from antecedent precision;
  !precision

(*Tableau associant la valeur des pièces pour le moteur à la fonction à applique pour lever une éventuelle ambiguïté de la notation algébrique*)
let vect = [|bishop_vect; rook_vect; king_vect; king_vect|]

(*Fonction participant à traduire un move classique noté en type Mouvement en sa notation algébrique*)
let algebraic_of_normal piece from to_ capture board player_legal_moves =
  let white_to_move =  piece > 0 in
  let algebraic = ref "" in
  let capture = if capture <> 0 then "x" else "" in
  let precision = ref "" in
  if (piece = pawn white_to_move && (from - to_) mod 2 <> 0) then begin
    precision := String.sub coord.(from) 0 1
  end
  else if abs piece = 2 then begin
    precision := knight_precision board from to_ white_to_move player_legal_moves
  end
  else if not (List.mem (abs piece) [1; 6]) then begin
    precision := precision_piece board from to_ player_legal_moves piece vect.((abs piece) - 3)
  end;
  if piece > 0 then begin
    algebraic := (if piece <> 1 then english_pieces.(piece) else "") ^ !precision ^ capture ^ coord.(to_)
  end
  else begin
    algebraic := (if piece <> (-1) then english_pieces.( - piece) else "") ^ !precision ^ capture ^ coord.(to_)
  end;
  !algebraic

(*Fonction participant à traduire une promotion notée en type Mouvement en sa notation algébrique*)
let algebraic_of_promotion from to_ promotion =
  let algebraic = ref "" in
  let player_sign = if to_ < 8 then 1 else (-1) in
  if abs (from - to_) = 8 then begin
    algebraic := coord.(to_) ^ "=" ^ english_pieces.(player_sign * promotion)
  end
  else begin
    algebraic := (String.sub coord.(from) 0 1) ^ "x" ^ (coord.(to_)) ^ "=" ^ english_pieces.(player_sign * promotion)
  end;
  !algebraic

(*Fonction traduisant un move noté avec le type Mouvement en sa notation algébrique*)
let algebraic_of_move move board player_legal_moves = match move with
  |Enpassant {from = from; to_ = to_} -> String.sub coord.(from) 0 1 ^ "x" ^ coord.(to_) ^ "ep"
  |Castling {sort} -> if sort mod 2 = 1 then "0-0" else "0-0-0"
  |Normal {piece = piece; from = from; to_ = to_; capture} -> algebraic_of_normal piece from to_ capture board player_legal_moves
  |Promotion {from = from; to_ = to_; promotion = promotion; capture = _} -> algebraic_of_promotion from to_ promotion
  |_ -> ""

(*Fonction traduisant un relevé de coups en type Mouvement en sa notation algébrique*)
let san_of_move_list list start_position initial_last_move initial_castling_right =
  let board = Array.copy start_position in
  let white_to_move = ref true in
  let last_move = ref initial_last_move in
  let castling_right = ref initial_castling_right in
  let moves_list = ref (List.rev list) in
  let word = ref "" in
  let line = ref "" in
  let algebraic = ref "" in
  let counter = ref 1 in
  while !moves_list <> [] do
    let king_position = index_array board (king !white_to_move) in
    let in_check = threatened board king_position !white_to_move in
    let player_legal_moves = legal_moves board !white_to_move !last_move !castling_right king_position in_check in
    let move = List.hd !moves_list in
    moves_list := List.tl !moves_list;
    if move <> Null then begin
      if !white_to_move then begin
        word := (string_of_int !counter) ^ ". " ^ algebraic_of_move move board player_legal_moves;
        incr counter
      end
      else begin
        if !word = "" then word := (string_of_int (!counter - 1)) ^ "...";
        word := !word ^ " " ^ algebraic_of_move move board player_legal_moves
      end;
      make_move_1 board move white_to_move last_move castling_right;
      if threatened board (index_array board (king !white_to_move)) !white_to_move then begin
        word := !word ^ "+"
      end;
      if !moves_list <> [] then begin
        if !white_to_move then begin
          if (String.length (!line ^ !word) > 80) then begin
            algebraic := !algebraic ^ "\n" ^ !line;
            line := !word ^ " "
          end
          else begin
            line := !line ^ !word ^ " "
          end;
        end
      end
      else begin
        if (win board !white_to_move move king_position in_check) = lose !white_to_move then begin
          let result = if !white_to_move then "0-1" else "1-0" in
          word := String.sub !word 0 (String.length !word - 1) ^ "# " ^ result
        end
        else begin
          word := !word ^ " 1/2-1/2"
        end;
        if (String.length (!line ^ !word) > 80) then begin
          algebraic := !algebraic ^ "\n" ^ !line ^ "\n" ^ !word
        end
        else begin
          algebraic := !algebraic ^ "\n" ^ !line ^ !word
        end
      end
    end
    else begin
      if !white_to_move then begin
        incr counter
      end;
      white_to_move := not !white_to_move
    end
  done;
  !algebraic*)

(*Fonction traduisant un move en sa notation UCI*)
let uci_of_mouvement move = match move with
  |Castling {sort} ->
    let from_king, to_short, to_long, from_short_rook, from_long_rook = if sort < 3 then !from_white_king, 62, 58, !from_short_white_rook, !from_long_white_rook else !from_black_king, 6, 2, !from_short_black_rook, !from_long_black_rook in
    let arrivee_roque, depart_tour = if sort mod 2 = 1 then to_short, from_short_rook else to_long, from_long_rook in
    coord.(from_king) ^ coord.(if not !chess_960 then arrivee_roque else depart_tour)
  |Promotion {from = _; to_ = _; capture = _; promotion} -> coord.(from move) ^ coord.(to_ move) ^ (String.lowercase_ascii english_pieces.(abs promotion))
  |Null -> "0000"
  |_ -> coord.(from move) ^ coord.(to_ move)