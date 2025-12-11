(*Module implémentant les fonctions qui permettent de générer les moves possibles à partir d'une position*)

open Board
open Zobrist

(*Fonction indiquant si une square est attaquée par une pièce ennemie*)
let threatened position square =
  let threat = ref false in
  let tab64_square = tab64.(square) in
  let player_sign = if position.board.(square) > 0 then 1 else (-1) in
  let i = ref 0 in
  while (not !threat && !i < 4) do
    let direction = bishop_vect.(!i) in
    let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
    if !iterate then begin
      let attacker = position.board.(tab120.(tab64_square + direction)) * player_sign in
      if attacker <> 0 then begin
        iterate :=  false;
        if ((attacker <= (-3) && attacker <> (-4)) || (attacker = (-1) && direction * player_sign < 0)) then begin
          threat := true
        end
      end;
      let distance = ref 2 in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker = position.board.(tab120.(tab64_square + (!distance * direction))) * player_sign in
        if attacker = 0 then begin
          incr distance
        end
        else if attacker > 0 then begin
          iterate :=  false
        end
        else begin
          if attacker = (-3) || attacker = (-5) then begin
            threat := true
          end;
          iterate :=  false
        end
      done;
    end;
    incr i
  done;
  if not !threat then begin
    let i = ref 0 in
    while (not !threat && !i < 8) do
      let direction = knight_vect.(!i) in
      if tab120.(tab64_square + direction) <> (-1) then begin
        if position.board.(tab120.(tab64_square + direction)) = (-2) * player_sign then begin
          threat := true
        end
      end;
      incr i
    done
  end;
  if not !threat then begin
    let i = ref 0 in
    while (not !threat && !i < 4) do
      let direction = rook_vect.(!i) in
      let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
      if !iterate then begin
        let attacker = position.board.(tab120.(tab64_square + direction)) * player_sign in
        if attacker <> 0 then begin
          iterate :=  false;
          if attacker <= (-4) then begin
            threat := true
          end
        end;
        let distance = ref 2 in
        while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
          let attacker = position.board.(tab120.(tab64_square + (!distance * direction))) * player_sign in
          if attacker = 0 then begin
            incr distance
          end
          else if attacker > 0 then begin
            iterate :=  false
          end
          else begin
            if attacker = (-4) || attacker = (-5) then begin
              threat := true
            end;
            iterate :=  false
          end
        done
      end;
      incr i
    done
  end;
  !threat

let add_move move moves number_of_moves =
  moves.(!number_of_moves) <- move;
  incr number_of_moves

let remove_move index moves number_of_moves =
  moves.(index) <- moves.(!number_of_moves - 1);
  decr number_of_moves

(*Fonction construisant une list des déplacements possible d'une tour*)
let rook_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let direction = rook_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un fou*)
let bishop_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let direction = bishop_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un cavalier*)
let knight_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = knight_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker <= 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'une dame*)
let queen_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = king_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un roi*)
let king_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = king_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker <= 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'un pion*)
let pawn_moves board square moves number_of_moves =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  if piece > 0 then begin
    let attacker_square_1 = tab120.(tab64_square - 10) in
    if board.(attacker_square_1) = 0 then begin
      if square > 15 then begin
        add_move (Normal {piece = 1; from = square; to_ = attacker_square_1}) moves number_of_moves;
        if (square > 47 && square < 56) then begin
          let attacker_square_2 = tab120.(tab64_square - 20) in
          if board.(attacker_square_2) = 0 then begin
            add_move (Normal {piece = 1; from = square; to_ = attacker_square_2}) moves number_of_moves
          end
        end
      end
      else begin
        List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_1; promotion = i}) moves number_of_moves) [5; 4; 3; 2]
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(tab64_square - 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 < 0 then begin
        if square > 15 then begin
          add_move (Normal {piece = 1; from = square; to_ = attacker_square_3}) moves number_of_moves
        end
        else begin
          List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_3; promotion = i}) moves number_of_moves) [5; 4; 3; 2]
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(tab64_square - 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 < 0 then begin
        if square > 15 then begin
          add_move (Normal {piece = 1; from = square; to_ = attacker_square_4}) moves number_of_moves
        end
        else begin
          List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_4; promotion = i}) moves number_of_moves) [5; 4; 3; 2]
        end
      end
    end
  end
  else begin
    let attacker_square_1 = tab120.(tab64_square + 10) in
    if board.(attacker_square_1) = 0 then begin
      if square < 48 then begin
        add_move (Normal {piece = (-1); from = square; to_ = attacker_square_1}) moves number_of_moves;
        if (square > 7 && square < 16) then begin
          let attacker_square_2 = tab120.(tab64_square + 20) in
          if (board.(attacker_square_2) = 0) then begin
            add_move (Normal {piece = (-1); from = square; to_ = attacker_square_2}) moves number_of_moves
          end
        end
      end
      else begin
        List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_1; promotion = i}) moves number_of_moves) [(-5); (-4); (-3); (-2)]
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(tab64_square + 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 > 0 then begin
        if square < 48 then begin
          add_move (Normal {piece = (-1); from = square; to_ = attacker_square_3}) moves number_of_moves
        end
        else begin
          List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_3; promotion = i}) moves number_of_moves) [(-5); (-4); (-3); (-2)]
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(tab64_square + 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 > 0 then begin
        if square < 48 then begin
          add_move (Normal {piece = (-1); from = square; to_ = attacker_square_4}) moves number_of_moves
        end
        else begin
          List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_4; promotion = i}) moves number_of_moves) [(-5); (-4); (-3); (-2)]
        end
      end
    end
  end

(*Tableau des fonctions à appliquer pour construire la list de moves*)
let tabfun = [|pawn_moves; knight_moves; bishop_moves; rook_moves; queen_moves|]

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pseudo_legal_moves position king_position king_moves_index moves number_of_moves =
  king_moves position.board king_position moves number_of_moves;
  king_moves_index := (0, !number_of_moves - 1);
  if position.white_to_move then begin
    let aux from arrive =
      for i = from downto arrive do
        let piece = position.board.(i) in
        if piece > 0 then begin
          (tabfun.(piece - 1) position.board i moves number_of_moves)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        let piece = position.board.(i) in
        if piece < 0 then begin
          (tabfun.(- piece - 1) position.board i moves number_of_moves)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end

let pin_generator piece square board moves number_of_moves pin_table =
  let tab64_square = tab64.(square) in
  let direction = pin_table.(square) in
  let func direction =
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        add_move (Normal {piece = piece; from = square; to_ = attacker_square}) moves number_of_moves;
        iterate :=  false
      end
    done
  in match abs piece with
  |1 ->
    if piece > 0 then begin
      if abs direction = 10 then begin
        let attacker_square_1 = tab120.(tab64_square - 10) in
        if board.(attacker_square_1) = 0 then begin
          add_move (Normal {piece = 1; from = square; to_ = attacker_square_1}) moves number_of_moves;
          if (square > 47 && square < 56) then begin
            let attacker_square_2 = tab120.(tab64_square - 20) in
            if board.(attacker_square_2) = 0 then begin
              add_move (Normal {piece = 1; from = square; to_ = attacker_square_2}) moves number_of_moves;
            end
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square - 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 < 0 then begin
          if square > 15 then begin
            add_move (Normal {piece = 1; from = square; to_ = attacker_square_3}) moves number_of_moves;
          end
          else begin
            List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_3; promotion = i}) moves number_of_moves) [5; 4; 3; 2]
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square - 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 < 0 then begin
          if square > 15 then begin
            add_move (Normal {piece = 1; from = square; to_ = attacker_square_4}) moves number_of_moves;
          end
          else begin
            List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_4; promotion = i}) moves number_of_moves) [5; 4; 3; 2]
          end
        end
      end
    end
    else begin
      if abs direction = 10 then begin
        let attacker_square_1 = tab120.(tab64_square + 10) in
        if board.(attacker_square_1) = 0 then begin
          add_move (Normal {piece = (-1); from = square; to_ = attacker_square_1}) moves number_of_moves;
          if (square > 7 && square < 16) then begin
            let attacker_square_2 = tab120.(tab64_square + 20) in
            if (board.(attacker_square_2) = 0) then begin
              add_move (Normal {piece = (-1); from = square; to_ = attacker_square_2}) moves number_of_moves;
            end
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square + 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 > 0 then begin
          if square < 48 then begin
            add_move (Normal {piece = (-1); from = square; to_ = attacker_square_3}) moves number_of_moves;
          end
          else begin
            List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_3; promotion = i}) moves number_of_moves) [(-5); (-4); (-3); (-2)]
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square + 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 > 0 then begin
          if square < 48 then begin
            add_move (Normal {piece = (-1); from = square; to_ = attacker_square_4}) moves number_of_moves;
          end
          else begin
            List.iter (fun i -> add_move (Promotion {from = square; to_ = attacker_square_4; promotion = i}) moves number_of_moves) [(-5); (-4); (-3); (-2)]
          end
        end
      end
    end
  |3 when List.mem (abs direction) [9; 11] -> List.iter (fun direction -> func direction) [direction; - direction]
  |4 when List.mem (abs direction) [1; 10] -> List.iter (fun direction -> func direction) [direction; - direction]
  |5 -> List.iter (fun direction -> func direction) [direction; - direction]
  |_ -> ()


(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pin_moves position king_position king_moves_index pinned_pieces pinned_moves_index moves number_of_moves pin_table =
  king_moves position.board king_position moves number_of_moves;
  king_moves_index := (0, !number_of_moves - 1);
  if position.white_to_move then begin
    let aux from to_ = 
    for square = from downto to_ do
      let piece = position.board.(square) in
      if not (List.mem square pinned_pieces) then begin
        if piece > 0 then begin
          tabfun.(piece - 1) position.board square moves number_of_moves
        end
      end
      else begin
        let first_pinned_move_index = !number_of_moves in
        pin_generator piece square position.board moves number_of_moves pin_table;
        pinned_moves_index := (first_pinned_move_index, !number_of_moves - 1) :: !pinned_moves_index
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for square = from to to_ do
        let piece = position.board.(square) in
        if not (List.mem square pinned_pieces) then begin
          if piece < 0 then begin
            tabfun.(- piece - 1) position.board square moves number_of_moves
          end
        end
        else begin
          let first_pinned_move_index = !number_of_moves in
          pin_generator piece square position.board moves number_of_moves pin_table;
          pinned_moves_index := (first_pinned_move_index, !number_of_moves - 1) :: !pinned_moves_index
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end

(*Fonction actualisant les variables relatives au castlings en fonction de la position de départ*)
let castling_update start_position =
  List.iter (fun tab -> Array.fill tab 0 (Array.length tab) 0) [white_short_path; white_long_path; black_short_path; black_long_path];
  List.iter (fun mut -> mut := 0) [white_short_path_length; white_long_path_length; black_short_path_length; black_long_path_length];
  List.iter (fun mut -> mut := []) [white_short_empties; white_long_empties; black_short_empties; black_long_empties];
  List.iter (fun (v, b) -> v := b) [(white_king_pinnable, true); (black_king_pinnable, true); (white_long_rook_in_a, true); (black_long_rook_in_a, true); (white_long_rook_in_b, false); (black_long_rook_in_b, false); (white_short_rook_in_h, true); (white_short_rook_in_h, true)];
  List.iter (fun long_directions -> long_directions := (-1)) [white_long_directions; black_long_directions];
  List.iter (fun (vect_fou_roque_joueur_gr, vect_fou_roque_adversaire_pr) ->
    for i = 0 to 1 do
      vect_fou_roque_joueur_gr.(i) <- (- vect_fou_roque_adversaire_pr.(i))
    done)
  [(white_long_bishop_vect, black_short_bishop_vect); (black_long_bishop_vect, white_short_bishop_vect)];
  let aux player_sign from_king depart_tour_pr from_long_rook clouage_roi_1 clouage_roi_2 roi_clouable long_rook_in_a long_rook_in_b tour_pr_en_h long_directions vect_fou_roque_pr vect_fou_roque_gr =
    let long_rook = ref true in
    let increment = if player_sign > 0 then 56 else 0 in
    for square = 0 + increment to 7 + increment do
      let piece = player_sign * start_position.(square) in
      if piece = 4 then begin
        if !long_rook then begin
          from_long_rook := square;
          long_rook_in_a := square = 0 + increment;
          long_rook_in_b := square = 1 + increment;
          long_rook := false
        end
        else begin
          depart_tour_pr := square;
          tour_pr_en_h := square = 7 + increment
        end
      end
      else if piece = 6 then begin
        from_king := square;
        clouage_roi_1 := square - (player_sign * 8);
        clouage_roi_2 := square - (player_sign * 16);
        if List.mem square [2 + increment; 6 + increment] then begin
          roi_clouable := false
        end
        else if square = 1 + increment then begin
          List.iter (fun i -> vect_fou_roque_gr.(i) <- vect_fou_roque_pr.(i)) [0; 1];
          long_directions := 1
        end
      end
    done
  in List.iter
  (fun (player_sign, from_king, depart_tour_pr, from_long_rook, clouage_roi_1, clouage_roi_2, roi_clouable, long_rook_in_a, long_rook_in_b, tour_pr_en_h, long_directions, vect_fou_roque_pr, vect_fou_roque_gr) ->
  aux player_sign from_king depart_tour_pr from_long_rook clouage_roi_1 clouage_roi_2 roi_clouable  long_rook_in_a long_rook_in_b tour_pr_en_h long_directions vect_fou_roque_pr vect_fou_roque_gr)
  [((-1), from_black_king, from_short_black_rook, from_long_black_rook, black_king_pin_1, black_king_pin_2, black_king_pinnable, black_long_rook_in_a, black_long_rook_in_b, black_short_rook_in_h, black_long_directions, black_short_bishop_vect, black_long_bishop_vect);
  (1, from_white_king, from_short_white_rook, from_long_white_rook, white_king_pin_1, white_king_pin_2, white_king_pinnable, white_long_rook_in_a, white_long_rook_in_b, white_short_rook_in_h, white_long_directions, white_short_bishop_vect, white_long_bishop_vect)];
  let func reference square piece =
    if piece > 0 then begin
      reference := 12 * square + (piece - 1) 
    end
    else begin
      reference := 12 * square + (5 - piece)
    end
  in List.iter
  (fun (reference, square, piece) -> func reference square piece)
  [(zobrist_from_white_king, !from_white_king, 6); (zobrist_from_black_king, !from_black_king, (-6));
  (zobrist_from_short_white_rook, !from_short_white_rook, 4); (zobrist_from_long_white_rook, !from_long_white_rook, 4);
  (zobrist_from_short_black_rook, !from_short_black_rook, (-4)); (zobrist_from_long_black_rook, !from_long_black_rook, (-4))];
  let j = ref 0 in
  let aux path path_lenght empties j i =
    path.(!j) <- i;
    incr path_lenght;
    empties := i :: !empties;
    incr j
  in
  for i = !from_white_king + 1 to 62 do
    aux white_short_path white_short_path_length white_short_empties j i
  done;
  j := 0;
  if !from_white_king = 57 then begin
    aux white_long_path white_long_path_length white_long_empties j 58
  end
  else begin
    for i = !from_white_king - 1 downto 58 do
      aux white_long_path white_long_path_length white_long_empties j i
    done
  end;
  j := 0;
  for i = !from_black_king + 1 to 6 do
    aux black_short_path black_short_path_length black_short_empties j i
  done;
  j := 0;
  if !from_black_king = 1 then begin
    aux black_long_path black_long_path_length black_long_empties j 2
  end
  else begin 
    for i = !from_black_king - 1 downto 2 do
      aux black_long_path black_long_path_length black_long_empties j i
    done
  end;
  j := 0;
  for i = !from_long_white_rook + 1 to 59 do
    white_long_empties := i :: !white_long_empties
  done;
  for i = !from_short_white_rook - 1 downto 61 do
    white_short_empties := i :: !white_short_empties
  done;
  j := 0;
  for i = !from_long_black_rook + 1 to 3 do
    black_long_empties := i :: !black_long_empties
  done;
  for i = !from_short_black_rook - 1 downto 5 do
    black_short_empties := i :: !black_short_empties
  done;
  let rec remove_sorted_doubles list = match list with
    |[] -> []
    |[h] -> [h]
    |h :: g :: t -> if h = g then h :: remove_sorted_doubles t else h :: (remove_sorted_doubles (g :: t))
  in let rec aux_liste list depart_tour from_king = match list with
    |[] -> []
    |h::t when not (List.mem h [depart_tour; from_king]) -> h :: aux_liste t depart_tour from_king
    |_::t -> aux_liste t depart_tour from_king
  in white_short_empties := List.rev (remove_sorted_doubles (merge_sort (aux_liste !white_short_empties !from_short_white_rook !from_white_king)));
  black_short_empties := List.rev (remove_sorted_doubles (merge_sort (aux_liste !black_short_empties !from_short_black_rook !from_black_king)));
  white_long_empties := remove_sorted_doubles (merge_sort (aux_liste !white_long_empties !from_long_white_rook !from_white_king));
  black_long_empties := remove_sorted_doubles (merge_sort (aux_liste !black_long_empties !from_long_black_rook !from_black_king))

(*Fonction permettant de vérifier la validité des roques*)
let castling_threats board player_sign clouage_roi pseudo_e2 path path_lenght vect_bishop vect_knight signe_roque =
  let i = ref 0 in
  let b = ref false in
  let diagonale = ref pseudo_e2 in
  let bishop_dir = ref 2 in
    while !i < path_lenght do
      let m = tab64.(path.(!i)) in
      if !diagonale > 0 then begin
        bishop_dir := 1
      end;
      let j = ref 0 in
      while (not !b && !j < !bishop_dir) do
        let direction = vect_bishop.(!j) in
        let distance = ref 1 in
        let iterate = ref true in
        while (tab120.(m + (!distance * direction)) <> (-1) && !iterate) do
          let attacker_square = tab120.(m + (!distance * direction)) in
          let attacker = player_sign * board.(attacker_square) in
          if attacker = 0 then begin
            incr distance
          end
          else if attacker > 0 then begin
            iterate :=  false
          end
          else begin
            if attacker = (-3) || attacker = (-5) || ((attacker = (-6) || attacker = (-1)) && !distance = 1)  then begin
              b := true
            end;
            iterate :=  false
          end
        done;
        incr j
      done;
      if not !b then begin
        let i = ref 0 in
        while (not !b && !i < 4) do
          let direction = vect_knight.(!i) in
          if tab120.(m + direction) <> (-1) then begin
            let attacker_square = tab120.(m + direction) in
            if player_sign * board.(attacker_square) = (-2) then begin
              b := true
            end
          end;
          incr i
        done
      end;
      if not !b then begin
        if !i < path_lenght then begin
          diagonale := player_sign * board.(clouage_roi + signe_roque * (1 + !i))
        end;
        if List.mem !diagonale [(-4); (-5); (-6)] then begin
          b := true
        end
        else if !diagonale = 0 then begin
          let direction = player_sign * (-10) in
          let distance = ref 2 in
          let iterate = ref true in
          while (tab120.(m + (!distance * direction)) <> (-1) && !iterate) do
            let attacker_square = tab120.(m + (!distance * direction)) in
            let attacker = player_sign * board.(attacker_square) in
            if attacker = 0 then begin
              incr distance
            end
            else if attacker > 0 then begin
              iterate :=  false
            end
            else begin
              if attacker = (-4) || attacker = (-5) then begin
                b := true
              end;
              iterate :=  false
            end
          done
        end
      end;
      incr i;
      bishop_dir := 2
    done;
  !b

(*Fonction indiquant l'absence de menace en a ou b empêchant un grand castlings*)
let possible_long board white_to_move =
  if white_to_move then begin
    let b1 = board.(57) in
    not ((b1 < (-3)) || ((b1 = 0 || !white_long_rook_in_b) && List.mem board.(56) [(-4); (-5)]))
  end
  else begin
    let b8 = board.(1) in
    not (b8 > 3 || ((b8 = 0 || !black_long_rook_in_b) && List.mem board.(0) [4; 5]))
  end

(*Fonction construisant une list des roques possible d'un joueur*)
let castlings (position : position) moves number_of_moves =
  if position.white_to_move then begin
    let pseudo_e2 = position.board.(!white_king_pin_1) in
    if not (!white_king_pinnable && (pseudo_e2 = (-1) || position.board.(!white_king_pin_2) = (-2))) then begin
      if position.castling_rights.white_short && (!white_short_rook_in_h || position.board.(63) > (-4)) && List.for_all (fun square -> position.board.(square) = 0) !white_short_empties && not (castling_threats position.board 1 !white_king_pin_1 pseudo_e2 white_short_path !white_short_path_length white_short_bishop_vect white_castling_knights_vect 1) then
        add_move (Castling {sort = 1}) moves number_of_moves
      end;
      if position.castling_rights.white_long && (!white_long_rook_in_a || possible_long position.board true) && List.for_all (fun square -> position.board.(square) = 0) !white_long_empties && not (castling_threats position.board 1 !white_king_pin_1 pseudo_e2 white_long_path !white_long_path_length white_long_bishop_vect white_castling_knights_vect !white_long_directions) then
         add_move (Castling {sort = 2}) moves number_of_moves
  end
  else begin
    let pseudo_e7 = - position.board.(!black_king_pin_1) in
    if not (!black_king_pinnable && (pseudo_e7 = (-1) || position.board.(!black_king_pin_2) = 2)) then begin
      if  position.castling_rights.black_short && (!black_short_rook_in_h || position.board.(7) < 4) && List.for_all (fun square -> position.board.(square) = 0) !black_short_empties && not (castling_threats position.board (-1) !black_king_pin_1 pseudo_e7 black_short_path !black_short_path_length black_short_bishop_vect black_castlings_knights_vect 1) then
         add_move (Castling {sort = 3}) moves number_of_moves
      end;
      if  position.castling_rights.black_long && (!black_long_rook_in_a || possible_long position.board false) && List.for_all (fun square -> position.board.(square) = 0) !black_long_empties && not (castling_threats position.board (-1) !black_king_pin_1 pseudo_e7 black_long_path !black_long_path_length black_long_bishop_vect black_castlings_knights_vect !black_long_directions)then
         add_move (Castling {sort = 4}) moves number_of_moves
    end

(*Fonction construisant une list des prises en passant possible d'un joueur*)
let enpassant (position : position) moves number_of_moves =
  if position.white_to_move then begin
    if position.ep_square <> (-1) then begin
      let right = position.ep_square + 9
      in if (position.ep_square <> 23 && position.board.(right) = 1) then begin
        add_move (Enpassant {from = right; to_ = position.ep_square}) moves number_of_moves
      end;
      let left = position.ep_square + 7
      in if (position.ep_square <> 16 && position.board.(left) = 1) then begin
        add_move (Enpassant {from = left; to_ = position.ep_square}) moves number_of_moves
      end
    end
  end
  else begin
    if position.ep_square <> (-1) then begin
      let right = position.ep_square - 7
      in if (position.ep_square <> 47 && position.board.(right) = (-1)) then begin
        add_move (Enpassant {from = right; to_ = position.ep_square}) moves number_of_moves
      end;
      let left = position.ep_square - 9
      in if (position.ep_square <> 40 && position.board.(left) = (-1)) then begin
        add_move (Enpassant {from = left; to_ = position.ep_square}) moves number_of_moves
      end
    end
  end

let make_light position move = match move with
  |Normal {piece; from; to_} -> begin
    position.board.(from) <- 0;
    position.last_capture <- position.board.(to_);
    position.board.(to_) <- piece
  end
  |Castling {sort} -> begin
    position.last_capture <- 0;
    match sort with
    |1 -> position.board.(!from_white_king) <- 0; position.board.(!from_short_white_rook) <- 0; position.board.(62) <- 6; position.board.(61) <- 4
    |2 -> position.board.(!from_white_king) <- 0; position.board.(!from_long_white_rook) <- 0; position.board.(58) <- 6; position.board.(59) <- 4
    |3 -> position.board.(!from_black_king) <- 0; position.board.(!from_short_black_rook) <- 0; position.board.(6) <- (-6); position.board.(5) <- (-4)
    |_ -> position.board.(!from_black_king) <- 0; position.board.(!from_long_black_rook) <- 0; position.board.(2) <- (-6); position.board.(3) <- (-4)
  end
  |Enpassant {from; to_} -> begin
    let player_pawn = if from < 32 then 1 else (-1) in
      position.board.(from) <- 0;
      position.last_capture <- - player_pawn;
      position.board.(to_) <- player_pawn;
      position.board.(to_ + player_pawn * 8) <- 0
  end
  |Promotion {from; to_; promotion} -> begin
    position.board.(from) <- 0;
    position.last_capture <- position.board.(to_);
    position.board.(to_) <- promotion
  end
  |Null -> ()

(*Fonction permettant de jouer un move sur l'échiquier*)
let make position move =
  position.white_to_move <- not position.white_to_move;
  position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(768);
  if position.ep_square <> (-1) then begin
    position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(773 + (position.ep_square mod 8));
    position.ep_square <- (-1)
  end;
  begin match move with
    |Normal {piece; from; to_} -> begin
      position.board.(from) <- 0;
      position.last_capture <- position.board.(to_);
      position.board.(to_) <- piece;
      if piece = 6 then begin
        if to_ = !from_short_black_rook && position.castling_rights.black_short then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = false;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771)
        end
        else if to_ = !from_long_black_rook && position.castling_rights.black_long then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = false;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(772)
        end
        else begin
          if position.castling_rights.white_short then begin
            position.castling_rights <- {
            white_short = false;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769)
          end;
          if position.castling_rights.white_long then begin
            position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = false;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(770)
          end
        end;
      end
      else if piece = (-6) then begin
        if to_ = !from_short_white_rook && position.castling_rights.white_short then begin
          position.castling_rights <- {
            white_short = false;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769)
        end
        else if to_ = !from_long_white_rook && position.castling_rights.white_long then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  =false;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(770)
        end
        else begin
          if position.castling_rights.black_short then begin
            position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = false;
            black_long  = position.castling_rights.black_long;
          };
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771)
          end;
          if position.castling_rights.black_long then begin
            position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = false;
          };
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(772)
          end
        end
      end
      else begin
        if position.castling_rights.white_short && (from = !from_short_white_rook || to_ = !from_short_white_rook) then begin
          position.castling_rights <- {
            white_short = false;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769)
        end
        else if position.castling_rights.white_long && (from = !from_long_white_rook || to_ = !from_long_white_rook) then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = false;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(770)
        end;
        if position.castling_rights.black_short && (from = !from_short_black_rook || to_ = !from_short_black_rook) then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = false;
            black_long  = position.castling_rights.black_long;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771)
        end
        else if position.castling_rights.black_long && (from = !from_long_black_rook || to_ = !from_long_black_rook) then begin
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = position.castling_rights.black_short;
            black_long  = false;
          };
          position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(772)
        end
      end;
      if (abs piece = 1) && (abs (from - to_) = 16) && ((from mod 8 <> 0 && position.board.(to_ - 1) = - piece) || (from mod 8 <> 7 && position.board.(to_ + 1) = - piece)) then begin
        position.ep_square <- from + (if piece > 0 then - 8 else 8);
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(773 + (from mod 8))
      end;
      let piece_index, capture_index = if piece > 0 then (piece - 1), (5 - position.last_capture) else (5 - piece), (position.last_capture - 1) in
      if position.last_capture = 0 then begin
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(12 * from + piece_index) lxor tab_zobrist.(12 * to_ + piece_index)
      end
      else begin
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(12 * from + piece_index) lxor tab_zobrist.(12 * to_ + piece_index) lxor tab_zobrist.(12 * to_ + capture_index)
      end;
      if (abs piece = 1 || position.last_capture <> 0) then begin
        position.board_record <- [position.zobrist_position];
        position.half_moves <- 0
      end
      else begin
        position.board_record <- position.zobrist_position :: position.board_record;
        position.half_moves <- position.half_moves + 1 
      end
    end
    |Castling {sort} -> begin
      position.last_capture <- 0;
      begin match sort with
        |1 ->
          position.board.(!from_white_king) <- 0;
          position.board.(!from_short_white_rook) <- 0;
          position.board.(62) <- 6;
          position.board.(61) <- 4;
          if position.castling_rights.white_long then begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769) lxor tab_zobrist.(770) lxor tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(749) lxor tab_zobrist.(!zobrist_from_short_white_rook) lxor tab_zobrist.(735)
          end
          else begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769) lxor tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(749) lxor tab_zobrist.(!zobrist_from_short_white_rook) lxor tab_zobrist.(735)
          end;
          position.castling_rights <- {
            white_short = false;
            white_long  = false;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          }
        |2 ->
          position.board.(!from_white_king) <- 0;
          position.board.(!from_long_white_rook) <- 0;
          position.board.(58) <- 6;
          position.board.(59) <- 4;
          if position.castling_rights.white_short then begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769) lxor tab_zobrist.(770) lxor tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(701) lxor tab_zobrist.(!zobrist_from_long_white_rook) lxor tab_zobrist.(711)
          end
          else begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(770) lxor tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(701) lxor tab_zobrist.(!zobrist_from_long_white_rook) lxor tab_zobrist.(711)
          end;
          position.castling_rights <- {
            white_short = false;
            white_long  = false;
            black_short = position.castling_rights.black_short;
            black_long  = position.castling_rights.black_long;
          }
        |3 ->
          position.board.(!from_black_king) <- 0;
          position.board.(!from_short_black_rook) <- 0;
          position.board.(6) <- (-6);
          position.board.(5) <- (-4);
          if position.castling_rights.black_long then begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771) lxor tab_zobrist.(772) lxor tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(83) lxor tab_zobrist.(!zobrist_from_short_black_rook) lxor tab_zobrist.(69)
          end
          else begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771) lxor tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(83) lxor tab_zobrist.(!zobrist_from_short_black_rook) lxor tab_zobrist.(69)
          end;
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = false;
            black_long  = false;
          }
        |_ ->
          position.board.(!from_black_king) <- 0;
          position.board.(!from_long_black_rook) <- 0;
          position.board.(2) <- (-6);
          position.board.(3) <- (-4);
          if position.castling_rights.black_short then begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771) lxor tab_zobrist.(772) lxor tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(35) lxor tab_zobrist.(!zobrist_from_long_black_rook) lxor tab_zobrist.(45)
          end
          else begin
            position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(772) lxor tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(35) lxor tab_zobrist.(!zobrist_from_long_black_rook) lxor tab_zobrist.(45)
          end;
          position.castling_rights <- {
            white_short = position.castling_rights.white_short;
            white_long  = position.castling_rights.white_long;
            black_short = false;
            black_long  = false;
          }
      end;
      position.board_record <- position.zobrist_position :: position.board_record;
      position.half_moves <- position.half_moves + 1 
    end
    |Enpassant {from; to_} -> begin
      let player_pawn, from_index, to_index, capture_index =
        if from < 32 then
          1, 12 * from, 12* to_, (12 * (to_ + 8) + 6)
        else
          (-1), 12 * from + 6, 12 * to_ + 6, (12 * (to_ - 8))
      in position.board.(from) <- 0;
      position.last_capture <- - player_pawn;
      position.board.(to_) <- player_pawn;
      position.board.(to_ + player_pawn * 8) <- 0;
      position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(from_index) lxor tab_zobrist.(to_index) lxor tab_zobrist.(capture_index);
      position.board_record <- [position.zobrist_position];
      position.half_moves <- 0
    end
    |Promotion {from; to_; promotion} -> begin
      position.board.(from) <- 0;
      position.last_capture <- position.board.(to_);
      position.board.(to_) <- promotion;
      if position.castling_rights.white_short && to_ = !from_short_white_rook then begin
        position.castling_rights <- {
          white_short = false;
          white_long  = position.castling_rights.white_long;
          black_short = position.castling_rights.black_short;
          black_long  = position.castling_rights.black_long;
        };
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(769)
      end;
      if position.castling_rights.white_long && to_ = !from_long_white_rook then begin
        position.castling_rights <- {
          white_short = position.castling_rights.white_short;
          white_long  = false;
          black_short = position.castling_rights.black_short;
          black_long  = position.castling_rights.black_long;
        };
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(770)
      end;
      if position.castling_rights.black_short && to_ = !from_short_black_rook then begin
        position.castling_rights <- {
          white_short = position.castling_rights.white_short;
          white_long  = position.castling_rights.white_long;
          black_short = false;
          black_long  = position.castling_rights.black_long;
        };
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(771)
      end;
      if position.castling_rights.black_long && to_ = !from_long_black_rook then begin
        position.castling_rights <- {
          white_short = position.castling_rights.white_short;
          white_long  = position.castling_rights.white_long;
          black_short = position.castling_rights.black_short;
          black_long  = false;
        };
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(772)
      end;
      let pawn_index, promotion_index, capture_index = if promotion > 0 then 0, (promotion - 1), (5 - position.last_capture) else 6, (5 - promotion), (position.last_capture - 1) in
      if position.last_capture = 0 then begin
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(12 * from + pawn_index) lxor tab_zobrist.(12 * to_ + promotion_index)
      end
      else begin
        position.zobrist_position <- position.zobrist_position lxor tab_zobrist.(12 * from + pawn_index) lxor tab_zobrist.(12 * to_ + promotion_index) lxor tab_zobrist.(12 * to_ + capture_index)
      end;
      position.board_record <- [position.zobrist_position];
      position.half_moves <- 0
    end
    |Null -> ()
  end

let unmake_light position move last_capture =
  begin match move with
    |Normal {piece; from; to_} -> begin
      position.board.(from) <- piece;
      position.board.(to_) <- position.last_capture
    end
    |Castling {sort} -> begin
      match sort with
      |1 -> position.board.(62) <- 0; position.board.(61) <- 0; position.board.(!from_short_white_rook) <- 4; position.board.(!from_white_king) <- 6
      |2 -> position.board.(58) <- 0; position.board.(59) <- 0; position.board.(!from_long_white_rook) <- 4; position.board.(!from_white_king) <- 6
      |3 -> position.board.(6) <- 0; position.board.(5) <- 0; position.board.(!from_short_black_rook) <- (-4); position.board.(!from_black_king) <- (-6)
      |_ -> position.board.(2) <- 0; position.board.(3) <- 0; position.board.(!from_long_black_rook) <- (-4); position.board.(!from_black_king) <- (-6)
    end
    |Enpassant {from; to_} -> begin
      if from < 32 then begin
        position.board.(from) <- 1;
        position.board.(to_) <- 0;
        position.board.(to_ + 8) <- (-1)
      end
      else begin
        position.board.(from) <- (-1);
        position.board.(to_) <- 0;
        position.board.(to_ - 8) <- 1
      end
    end
    |Promotion {from; to_; promotion} -> begin  
      position.board.(from) <- if promotion > 0 then 1 else (-1);
      position.board.(to_) <- position.last_capture
    end
    |Null -> ()
  end;
  position.last_capture <- last_capture

(*Fonction permettant l'annulation d'un move*)
let unmake position undo_info move =
  begin match move with
    |Normal {piece; from; to_} -> begin
      position.board.(from) <- piece;
      position.board.(to_) <- position.last_capture
    end
    |Castling {sort} -> begin
      match sort with
      |1 -> position.board.(62) <- 0;  position.board.(61) <- 0; position.board.(!from_short_white_rook) <- 4;  position.board.(!from_white_king) <- 6
      |2 -> position.board.(58) <- 0; position.board.(59) <- 0; position.board.(!from_long_white_rook) <- 4;  position.board.(!from_white_king) <- 6
      |3 -> position.board.(6) <- 0; position.board.(5) <- 0; position.board.(!from_short_black_rook) <- (-4);  position.board.(!from_black_king) <- (-6)
      |_ -> position.board.(2) <- 0; position.board.(3) <- 0; position.board.(!from_long_black_rook) <- (-4);  position.board.(!from_black_king) <- (-6)
    end
    |Enpassant {from; to_} -> begin
      if from < 32 then begin
        position.board.(from) <- 1;
        position.board.(to_) <- 0;
        position.board.(to_ + 8) <- (-1)
      end
      else begin
        position.board.(from) <- (-1);
        position.board.(to_) <- 0;
        position.board.(to_ - 8) <- 1
      end
    end
    |Promotion {from; to_; promotion} -> begin  
      position.board.(from) <- if promotion > 0 then 1 else (-1);
      position.board.(to_) <- position.last_capture
    end
    |Null -> ()
  end;
  position.white_to_move <- not position.white_to_move;
  position.ep_square <- undo_info.ep_square;
  position.castling_rights <- undo_info.castling_rights;
  position.board_record <- undo_info.board_record;
  position.half_moves <- undo_info.half_moves;
  position.zobrist_position <- undo_info.zobrist_position;
  position.last_capture <- undo_info.last_capture


(*Fonction permettant de jouer un move sur l'échiquier*)
(*let make_acc board move = match move with
  |Normal {piece; from; to_; capture} -> begin
    board.(from) <- 0;
    board.(to_) <- piece;
    if piece > 0 then begin
      let idx_to = 12 * to_ + (piece - 1) in
      let idx_from = 12 * from + (piece - 1) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - piece) in
      let idx_from = 12 * from + (5 - piece) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (capture - 1)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done 
      end
    end
  end
  |Castling {sort} -> begin
    match sort with
    |1 ->
      board.(!from_white_king) <- 0;
      board.(62) <- 6;
      board.(!from_short_white_rook) <- 0;
      board.(61) <- 4;
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 749) +. hidden_weights.(i * 768 + 735) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_short_white_rook)
      done
    |2 ->
      board.(!from_white_king) <- 0;
      board.(58) <- 6;
      board.(!from_long_white_rook) <- 0;
      board.(59) <- 4;
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 701) +. hidden_weights.(i * 768 + 711) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_long_white_rook)
      done
    |3 ->
      board.(!from_black_king) <- 0;
      board.(6) <- (-6);
      board.(!from_short_black_rook) <- 0;
      board.(5) <- (-4);
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 83) +. hidden_weights.(i * 768 + 69) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_short_black_rook)
      done
    |_ ->
      board.(!from_black_king) <- 0;
      board.(2) <- (-6);
      board.(!from_long_black_rook) <- 0;
      board.(3) <- (-4);
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 35) +. hidden_weights.(i * 768 + 45) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_long_black_rook)
      done
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      board.(from) <- 0;
      board.(to_) <- 1;
      board.(to_ + 8) <- 0;
      let idx_to = 12 * to_ in
      let idx_from = 12 * from in
      let idx_capture = 12 * (to_ + 8) + 6 in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
    else begin
      board.(from) <- 0;
      board.(to_) <- (-1);
      board.(to_ - 8) <- 0;
      let idx_to = 12 * to_ + 6 in
      let idx_from = 12 * from + 6 in
      let idx_capture = 12 * (to_ - 8) in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
  end
  |Promotion {from; to_; promotion; capture} -> begin
    board.(from) <- 0;
    board.(to_) <- promotion;
    if to_ < 8 then begin
      let idx_to = 12 * to_ + (promotion - 1) in
      let idx_from = 12 * from in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - promotion) in
      let idx_from = 12 * from + 6 in
      if capture = 0 then begin 
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = 12 * to_ + (capture - 1) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
  end
  |Null -> ()

(*Fonction permettant l'annulation d'un move*)
let unmake_acc board move = match move with
  |Normal {piece; from; to_; capture} -> begin
    board.(from) <- piece;
    board.(to_) <- capture;
    if piece > 0 then begin
      let idx_to = 12 * to_ + (piece - 1) in
      let idx_from = 12 * from + (piece - 1) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - piece) in
      let idx_from = 12 * from + (5 - piece) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (capture - 1)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
        done 
      end
    end
  end
  |Castling {sort} -> begin
    match sort with
    |1 ->
      board.(62) <- 0;
      board.(!from_white_king) <- 6;
      board.(61) <- 0;
      board.(!from_short_white_rook) <- 4;
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + 749) -. hidden_weights.(i * 768 + 735) +. hidden_weights.(i * 768 + !zobrist_from_white_king) +. hidden_weights.(i * 768 + !zobrist_from_short_white_rook)
      done
    |2 ->
      board.(58) <- 0;
      board.(!from_white_king) <- 6;
      board.(59) <- 0;
      board.(!from_long_white_rook) <- 4;
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + 701) -. hidden_weights.(i * 768 + 711) +. hidden_weights.(i * 768 + !zobrist_from_white_king) +. hidden_weights.(i * 768 + !zobrist_from_long_white_rook)
      done
    |3 ->
      board.(6) <- 0;
      board.(!from_black_king) <- (-6);
      board.(5) <- 0;
      board.(!from_short_black_rook) <- (-4);
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + 83) -. hidden_weights.(i * 768 + 69) +. hidden_weights.(i * 768 + !zobrist_from_black_king) +. hidden_weights.(i * 768 + !zobrist_from_short_black_rook)
      done
    |_ ->
      board.(2) <- 0;
      board.(!from_black_king) <- (-6);
      board.(3) <- 0;
      board.(!from_long_black_rook) <- (-4);
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + 35) -. hidden_weights.(i * 768 + 45) +. hidden_weights.(i * 768 + !zobrist_from_black_king) +. hidden_weights.(i * 768 + !zobrist_from_long_black_rook)
      done
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      board.(from) <- 1;
      board.(to_) <- 0;
      board.(to_ + 8) <- (-1);
      let idx_to = 12 * to_ in
      let idx_from = 12 * from in
      let idx_capture = 12 * (to_ + 8) + 6 in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
      done
    end
    else begin
      board.(from) <- (-1);
      board.(to_) <- 0;
      board.(to_ - 8) <- 1;
      let idx_to = 12 * to_ + 6 in
      let idx_from = 12 * from + 6 in
      let idx_capture = 12 * (to_ - 8) in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
      done
    end
  end
  |Promotion {from; to_; promotion; capture} -> begin
    board.(from) <- if promotion > 0 then 1 else (-1);
    board.(to_) <- capture;
    if to_ < 8 then begin
      let idx_to = 12 * to_ + (promotion - 1) in
      let idx_from = 12 * from in
      if capture = 0 then begin 
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - promotion) in
      let idx_from = 12 * from + 6 in
      if capture = 0 then begin 
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = 12 * to_ + (capture - 1) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) -. hidden_weights.(i * 768 + idx_to) +. hidden_weights.(i * 768 + idx_from) +. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
  end
  |Null -> ()*)

(*Fonction donnant la capture d'un move
let capture move = match move with
  |Normal {piece = _; from = _; to_ = _; capture} | Promotion {from = _; to_ = _; capture; promotion = _} -> capture
  |Enpassant {from = _; to_} -> if to_ < 24 then (-1) else 1
  |_ -> 0*)

(*Fonction donnant la pièce d'un move classique*)
let piece move = match move with
  |Normal {piece; from = _; to_ = _} -> piece
  |Promotion {from = _; to_ = _; promotion} -> if promotion > 0 then 1 else (-1)
  |Enpassant {from = _; to_} -> if to_ < 24 then 1 else (-1)
  |_ -> 0

(*Fonction indiquant si une pièce pin une pièce clouable*)
let pin board chessman white_to_move tab64_square direction distance =
  let pinned = ref false in
  let distance = ref (distance + 1) in
  let iterate = ref true in
  let opponent_queen = queen (not white_to_move) in
  while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
    let attacker_square = tab120.(tab64_square + (!distance * direction)) in
    let attacker = board.(attacker_square) in
    if opponent_queen * attacker < 0 then begin
      iterate :=  false
    end
    else if opponent_queen * attacker > 0 then begin 
      if attacker = chessman || attacker = opponent_queen then begin
        pinned:= true;
      end;
      iterate :=  false
    end;
    incr distance
  done;
  !pinned

(*Fonction donnant les cases des pièces clouées (indépendanmment de leur possibilité de mouvement) en considérant que le roi est dans la square en argument*)
let pinned_squares position king_square pin_table =
  let list = ref [] in
  let tab64_square = tab64.(king_square) in
  let player_sign = if position.white_to_move then 1 else (-1) in
  let aux vect piece =
    for i = 0 to 3 do
      let direction = vect.(i) in
      let distance = ref 1 in
      let iterate = ref true in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker_square = tab120.(tab64_square + (!distance * direction)) in
        let attacker = player_sign * position.board.(attacker_square) in
        if attacker > 0 then begin
          if pin position.board (- piece * player_sign) position.white_to_move tab64_square direction !distance then begin
            list := attacker_square :: !list;
            pin_table.(attacker_square) <- direction
          end;
          iterate :=  false
        end
        else if attacker < 0 then begin 
          iterate :=  false
        end;
        incr distance
      done
    done;
  in List.iter (fun (vect, piece) -> aux vect piece ) [(rook_vect, 4); (bishop_vect, 3)];
  !list

(*Fonction construisant une list des moves légaux du joueur*)
let legal_moves (position : position) king_position in_check =
  
  let moves = Array.make 256 Null in
  let number_of_moves = ref 0 in
  let king_moves_index = ref (0,0) in
  let to_remove = ref [] in
  let pin_table = Array.make 64 0 in
  let aux move move_index king_position =
    let last_capture = position.last_capture in
    make_light position move;
    if threatened position king_position then begin
      to_remove := move_index :: !to_remove
    end;
    unmake_light position move last_capture;
  in if in_check then begin
    pseudo_legal_moves position king_position king_moves_index moves number_of_moves;
    for i = fst !king_moves_index to snd !king_moves_index do
      let king_move = moves.(i) in
      aux king_move i (to_ king_move)
    done;
    for i = snd !king_moves_index + 1 to !number_of_moves - 1 do
      let other_move = moves.(i) in
      aux other_move i king_position
    done
  end
  else begin
    let pinned_pieces = pinned_squares position king_position pin_table in
    if pinned_pieces = [] then begin
      pseudo_legal_moves position king_position king_moves_index moves number_of_moves;
      for i = fst !king_moves_index to snd !king_moves_index do
        let king_move = moves.(i) in
        aux king_move i (to_ king_move)
      done;
      castlings position moves number_of_moves
    end
    else begin
      let pinned_moves_index = ref [] in
      pin_moves position king_position king_moves_index pinned_pieces pinned_moves_index moves number_of_moves pin_table;
      for i = fst !king_moves_index to snd !king_moves_index do
        let king_move = moves.(i) in
        aux king_move i (to_ king_move)
      done;
      castlings position moves number_of_moves
    end
  end;
  let index_first_ep = !number_of_moves in
  enpassant position moves number_of_moves;
  for i = index_first_ep to !number_of_moves - 1 do
    let prise_en_passant = moves.(i) in
    aux prise_en_passant i king_position
  done;
  List.iter (fun index -> remove_move index moves number_of_moves) !to_remove;
  moves, number_of_moves

(*Fonction construisant une list des déplacements possible d'une tour*)
let rook_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let direction = rook_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        list := Normal {piece = piece; from = square; to_ = attacker_square} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un fou*)
let bishop_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let direction = bishop_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = attacker_square} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un cavalier*)
let knight_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = knight_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker < 0 then begin
        list := Normal {piece = piece; from = square; to_ = attacker_square} :: !list
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'une dame*)
let queen_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = king_vect.(i) in
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = attacker_square} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un roi*)
let king_captures board square =
  let list = ref [] in
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = king_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker < 0 then begin
        list := Normal {piece = piece; from = square; to_ = attacker_square} :: !list
      end
    end
  done;
  !list

(*Fonction construisant une list des captures/ promotions possible d'un pion*)
let pawn_captures board square list =
  let piece = board.(square) in
  let p = tab64.(square) in
  if piece > 0 then begin
    let attacker_square_1 = tab120.(p - 10) in
    if board.(attacker_square_1) = 0 && square < 16 then begin
      list :=
      Promotion {from = square; to_ = attacker_square_1; promotion = 5} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 4} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 3} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 2} ::
      !list
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(p - 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = 5} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 4} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 2} ::
          !list
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(p - 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = 5} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 3} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 2} ::
          !list
        end
      end
    end
  end
  else begin
    let attacker_square_1 = tab120.(p + 10) in
    if board.(attacker_square_1) = 0 && square > 47 then begin
      list :=
      Promotion {from = square; to_ = attacker_square_1; promotion = (-5)} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-4)} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-3)} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-2)} :: !list
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(p + 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = (-5)} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-4)} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-3)} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-2)} ::
          !list
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(p + 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = (-5)} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-4)} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-3)} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-2)} :: !list
        end
      end
    end
  end

let captures_vect = [|pawn_captures; knight_captures; bishop_captures; rook_captures; queen_captures|]

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pseudo_legal_captures position king_position =
  let move_list = ref [] in
  let king_move_list = king_captures position.board king_position in
  if position.white_to_move then begin
    let aux from arrive =
      for i = from downto arrive do
        let piece = position.board.(i) in
        if piece > 0 then begin
          (captures_vect.(piece - 1) position.board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        let piece = position.board.(i) in
        if piece < 0 then begin
          (captures_vect.(- piece - 1) position.board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list

let pin_captures_generator piece square board pinned_list pin_table =
  let tab64_square = tab64.(square) in
  let direction = pin_table.(square) in
  let func direction =
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        pinned_list := Normal {piece = piece; from = square; to_ = attacker_square} :: !pinned_list;
        iterate :=  false
      end
    done
  in match abs piece with
  |1 ->
    if piece > 0 then begin
      if (square + 1) mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square - 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 < 0 then begin
          if square > 15 then begin
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = 5} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 4} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 2} ::
            !pinned_list
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square - 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 < 0 then begin
          if square > 15 then begin
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = 5} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 3} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 2} ::
            !pinned_list
          end
        end
      end
    end
    else begin
      if square mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square + 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 > 0 then begin
          if square < 48 then begin
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = (-5)} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-4)} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-3)} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-2)} ::
            !pinned_list
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square + 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 > 0 then begin
          if square < 48 then begin
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = (-5)} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-4)} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-3)} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-2)} ::
            !pinned_list
          end
        end
      end
    end
  |3 when List.mem (abs direction) [9; 11] -> List.iter (fun direction -> func direction) [direction; - direction]
  |4 when List.mem (abs direction) [1; 10] -> List.iter (fun direction -> func direction) [direction; - direction]
  |5 -> List.iter (fun direction -> func direction) [direction; - direction]
  |_ -> ()

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pin_captures position king_position pinned_pieces pin_table =
  let move_list = ref [] in
  let king_move_list = king_captures position.board king_position in
  let pinned_list = ref [] in
  if position.white_to_move then begin
    let aux from to_ = 
    for square = from downto to_ do
      let piece = position.board.(square) in
      if not (List.mem square pinned_pieces) then begin
        if piece > 0 then begin
          captures_vect.(piece - 1) position.board square move_list
        end
      end
      else begin
        pin_captures_generator piece square position.board pinned_list pin_table
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for square = from to to_ do
        let piece = position.board.(square) in
        if not (List.mem square pinned_pieces) then begin
          if piece < 0 then begin
            captures_vect.(- piece - 1) position.board square move_list
          end
        end
        else begin
          pin_captures_generator piece square position.board pinned_list pin_table
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list, !pinned_list

let captures position =
  let move_list = ref [] in
  let player_king = king position.white_to_move in
  let king_position = index_array position.board player_king in
  let pin_table = Array.make 64 0 in
  let aux move king_position =
    let last_capture = position.last_capture in
    make_light position move;
    if not (threatened position king_position) then begin
      move_list := move :: !move_list
    end;
    unmake_light position move last_capture
  in let ep_array = Array.make 2 Null in
  let number_of_ep = ref 0 in
  enpassant position ep_array number_of_ep;
  for i = 0 to !number_of_ep - 1 do
    aux ep_array.(i) king_position
  done;
  if threatened position king_position then begin
    let moves, king_moves = pseudo_legal_captures position king_position in
    List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
    List.iter (fun other_move -> aux other_move king_position) moves;
    !move_list
  end
  else begin
    let pinned_pieces = pinned_squares position king_position pin_table in
    if pinned_pieces = [] then begin
      let moves, king_moves = pseudo_legal_captures position king_position in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      !move_list @ moves
    end
    else begin
      let moves, king_moves, pinned_moves = pin_captures position king_position pinned_pieces pin_table in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
     !move_list @ pinned_moves @ moves
    end
  end

let move_array_mem move legal_moves number_of_legal_moves =
  let mem = ref false in
  let i = ref 0 in
  while !i < number_of_legal_moves && not !mem do
    if legal_moves.(!i) = move then mem := true;
    incr i
  done;
  !mem