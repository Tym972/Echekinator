(*Module implémentant les fonctions qui permettent de générer les moves possibles à partir d'une position*)

open Board
open Zobrist

(*Fonction indiquant si une square est attaquée par une pièce ennemie*)
let threatened board square white_to_move =
  let threat = ref false in
  let tab64_square = tab64.(square) in
  let player_sign = if white_to_move then 1 else (-1) in
  let pawn_vect = [|(-9) * player_sign; (-11) * player_sign|] in
  let i = ref 0 in
  while (not !threat && !i < 2) do
    let dir = pawn_vect.(!i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      if board.(candidate) = (- player_sign) then begin
        threat := true
      end
    end;
    incr i
  done;
  if not !threat then begin
    let i = ref 0 in
    while (not !threat && !i < 8) do
      let dir = knight_vect.(!i) in
      if tab120.(tab64_square + dir) <> (-1) then begin
        let candidate = tab120.(tab64_square + dir) in
        if board.(candidate) = (-2) * player_sign then begin
          threat := true
        end
      end;
      incr i
    done
  end;
  if not !threat then begin
    let i = ref 0 in
    while (not !threat && !i < 4) do
      let dir = bishop_vect.(!i) in
      let k = ref 1 in
      let iterate = ref true in
      while (tab120.(tab64_square + (!k * dir)) <> (-1) && !iterate) do
        let candidate = tab120.(tab64_square + (!k * dir)) in
        let dest = board.(candidate) * player_sign in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          iterate :=  false
        end
        else begin
          if dest = (-3) || dest = (-5) || (dest = (-6) && !k = 1) then begin
            threat := true
          end;
          iterate :=  false
        end
      done;
      incr i
    done
  end;
  if not !threat then begin
    let i = ref 0 in
    while (not !threat && !i < 4) do
      let dir = rook_vect.(!i) in
      let k = ref 1 in
      let iterate = ref true in
      while (tab120.(tab64_square + (!k * dir)) <> (-1) && !iterate) do
        let candidate = tab120.(tab64_square + (!k * dir)) in
        let dest = board.(candidate) * player_sign in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          iterate :=  false
        end
        else begin
          if dest = (-4) || dest = (-5) || (dest = (-6) && !k = 1) then begin
            threat := true
          end;
          iterate :=  false
        end
      done;
      incr i
    done
  end;
  !threat

(*Fonction construisant une list des déplacements possible d'une tour*)
let rook_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let dir = rook_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = 0} :: !list;
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin 
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un fou*)
let bishop_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let dir = bishop_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = 0} :: !list;
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un cavalier*)
let knight_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let dir = knight_vect.(i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      let dest = board.(candidate) in
      if piece * dest <= 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'une dame*)
let queen_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let dir = king_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = 0} :: !list;
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un roi*)
let king_moves board square =
  let list = ref [] in
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let dir = king_vect.(i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      let dest = board.(candidate) in
      if piece * dest <= 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list
      end
    end
  done;
  !list

(*Fonction construisant une list des déplacements possible d'un pion*)
let pawn_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  if piece > 0 then begin
    let candidate_1 = tab120.(tab64_square - 10) in
    if board.(candidate_1) = 0 then begin
      if square > 15 then begin
        list := Normal {piece = 1; from = square; to_ = candidate_1; capture = 0} :: !list;
        if (square > 47 && square < 56) then begin
          let candidate_2 = tab120.(tab64_square - 20) in
          if board.(candidate_2) = 0 then begin
            list := Normal {piece = 1; from = square; to_ = candidate_2; capture = 0} :: !list
          end
        end
      end
      else begin
        list :=
        Promotion {from = square; to_ = candidate_1; promotion = 5; capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = 4; capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = 3; capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = 2; capture = 0} ::
        !list
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidat3 = tab120.(tab64_square - 9) in
      let dest3 = board.(candidat3) in
      if dest3 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = candidat3; capture = dest3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat3; promotion = 5; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 4; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 3; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 2; capture = dest3} ::
          !list
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidat4 = tab120.(tab64_square - 11) in
      let dest4 = board.(candidat4) in
      if dest4 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = candidat4; capture = dest4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat4; promotion = 5; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 4; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 3; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 2; capture = dest4} ::
          !list
        end
      end
    end
  end
  else begin
    let candidate_1 = tab120.(tab64_square + 10) in
    if board.(candidate_1) = 0 then begin
      if square < 48 then begin
        list := Normal {piece = (-1); from = square; to_ = candidate_1; capture = 0} :: !list;
        if (square > 7 && square < 16) then begin
          let candidate_2 = tab120.(tab64_square + 20) in
          if (board.(candidate_2) = 0) then begin
            list := Normal {piece = (-1); from = square; to_ = candidate_2; capture = 0} :: !list
          end
        end
      end
      else begin
        list :=
        Promotion {from = square; to_ = candidate_1; promotion = (-5); capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = (-4); capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = (-3); capture = 0} ::
        Promotion {from = square; to_ = candidate_1; promotion = (-2); capture = 0} ::
        !list
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidat3 = tab120.(tab64_square + 9) in
      let dest3 = board.(candidat3) in
      if dest3 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = candidat3; capture = dest3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat3; promotion = (-5); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-4); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-3); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-2); capture = dest3} ::
          !list
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidat4 = tab120.(tab64_square + 11) in
      let dest4 = board.(candidat4) in
      if dest4 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = candidat4; capture = dest4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat4; promotion = (-5); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-4); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-3); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-2); capture = dest4} ::
          !list
        end
      end
    end
  end

(*Tableau des fonctions à appliquer pour construire la list de moves*)
let tabfun = [|pawn_moves; knight_moves; bishop_moves; rook_moves; queen_moves|]

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pseudo_legal_moves board white_to_move king_position =
  let move_list = ref [] in
  let king_move_list = king_moves board king_position in
  if white_to_move then begin
    let aux from arrive =
      for i = from downto arrive do
        let piece = board.(i) in
        if piece > 0 then begin
          (tabfun.(piece - 1) board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        let piece = board.(i) in
        if piece < 0 then begin
          (tabfun.(- piece - 1) board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pin_moves board white_to_move king_position pinned_pieces =
  let move_list = ref [] in
  let king_move_list = king_moves board king_position in
  let pinned_list = ref [] in
  if white_to_move then begin
    let aux from to_ = 
    for i = from downto to_ do
      if not (List.mem i pinned_pieces) then begin
        let piece = board.(i) in
        if piece > 0 then begin
          (tabfun.(piece - 1) board i move_list)
        end
      end
      else begin
        let piece = board.(i) in (tabfun.(piece - 1) board i pinned_list)
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        if not (List.mem i pinned_pieces) then begin
          let piece = board.(i) in
          if piece < 0 then begin
            (tabfun.(- piece - 1) board i move_list)
          end
        end
        else begin
          let piece = board.(i) in (tabfun.(- piece - 1) board i pinned_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list, !pinned_list

let chess_960 = ref false

(*Variables indiquant la positions initiales des pièces impliquées dans un castlings*)
let from_white_king = ref 60
let from_black_king = ref 4
let from_short_white_rook = ref 63
let from_long_white_rook = ref 56
let from_short_black_rook = ref 7
let from_long_black_rook = ref 0

(*Cases de passage du roi pour le castlings*)
let white_short_path = [|61; 62; 0; 0; 0; 0|]
let white_long_path = [|59; 58; 0; 0; 0|]
let black_short_path = [|5; 6; 0; 0; 0; 0|]
let black_long_path = [|3; 2; 0; 0; 0|]

(*Nombre de square traversée par le roi*)
let white_short_path_length = ref 2
let white_long_path_length = ref 2
let black_short_path_length = ref 2
let black_long_path_length = ref 2

(*Cases devant êtres vides pour le castlings*)
let white_short_empties = ref [61; 62]
let white_long_empties = ref [59; 58; 57]
let black_short_empties = ref [5; 6]
let black_long_empties = ref [3; 2; 1]

(*Variable indiquant si le roi doit se déplacer pour atteindre sa square de castlings (échecs 960)*)
let white_king_pinnable = ref true
let black_king_pinnable = ref true

(*Variables indicating whether the starting column of the queenside rook is a*)
let white_long_rook_in_a = ref true
let black_long_rook_in_a = ref true

(*Variables indicating whether the starting column of the kingside rook is h*)
let white_short_rook_in_h = ref true
let black_short_rook_in_h = ref true

(*Critical case for the castling rights*)
let white_king_pin_1 = ref 52
let white_king_pin_2 = ref 44
let black_king_pin_1 = ref 12
let black_king_pin_2 = ref 20

(*Variable indiquant la direction du grand castlings, valant 1 si le déplacement du roi se fait vers la droite, (-1) sinon*)
let white_long_directions = ref (-1)
let black_long_directions = ref (-1)

(*Variable indiquant si la square de départ de la tour du grand castlings est b1 (respectivement b8)*)
let white_long_rook_in_b = ref false
let black_long_rook_in_b = ref false

(*Vecteurs de déplacement des potentielles menaces au castlings*)
let white_short_bishop_vect = [|(-9); (-11)|]
let white_long_bishop_vect = [|(-11); (-9)|]
let black_short_bishop_vect = [|11; 9|]
let black_long_bishop_vect = [|9; 11|]
let white_castling_knights_vect = [|(-8); (-12); (-19); (-21)|]
let black_castlings_knights_vect = [|8; 12; 19; 21|]

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
  let j = ref 0 in
  let aux chemin path_lenght vides j i =
    chemin.(!j) <- i;
    incr path_lenght;
    vides := i :: !vides;
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
        let dir = vect_bishop.(!j) in
        let k = ref 1 in
        let iterate = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !iterate) do
          let candidate = tab120.(m + (!k * dir)) in
          let dest = player_sign * board.(candidate) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            iterate :=  false
          end
          else begin
            if dest = (-3) || dest = (-5) || ((dest = (-6) || dest = (-1)) && !k = 1)  then begin
              b := true
            end;
            iterate :=  false
          end
        done;
        incr j
      done;
      if not !b then begin
        let k = ref 0 in
        while (not !b && !k < 4) do
          let dir = vect_knight.(!k) in
          if tab120.(m + dir) <> (-1) then begin
            let candidate = tab120.(m + dir) in
            if player_sign * board.(candidate) = (-2) then begin
              b := true
            end
          end;
          incr k
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
          let dir = player_sign * (-10) in
          let k = ref 2 in
          let iterate = ref true in
          while (tab120.(m + (!k * dir)) <> (-1) && !iterate) do
            let candidate = tab120.(m + (!k * dir)) in
            let dest = player_sign * board.(candidate) in
            if dest = 0 then begin
              incr k
            end
            else if dest > 0 then begin
              iterate :=  false
            end
            else begin
              if dest = (-4) || dest = (-5) then begin
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
let castlings board white_to_move (white_short, white_long, black_short, black_long) =
  let l = ref [] in 
  if white_to_move then begin
    let pseudo_e2 = board.(!white_king_pin_1) in
    if not (!white_king_pinnable && (pseudo_e2 = (-1) || board.(!white_king_pin_2) = (-2))) then begin
      if white_short && (!white_short_rook_in_h || board.(63) > (-4)) && List.for_all (fun square -> board.(square) = 0) !white_short_empties && not (castling_threats board 1 !white_king_pin_1 pseudo_e2 white_short_path !white_short_path_length white_short_bishop_vect white_castling_knights_vect 1)
          then l := Castling {sort = 1} :: !l
      end;
      if white_long && (!white_long_rook_in_a || possible_long board true) && List.for_all (fun square -> board.(square) = 0) !white_long_empties && not (castling_threats board 1 !white_king_pin_1 pseudo_e2 white_long_path !white_long_path_length white_long_bishop_vect white_castling_knights_vect !white_long_directions)
        then l := Castling {sort = 2} :: !l
  end
  else begin
    let pseudo_e7 = - board.(!black_king_pin_1) in
    if not (!black_king_pinnable && (pseudo_e7 = (-1) || board.(!black_king_pin_2) = 2)) then begin
      if black_short && (!black_short_rook_in_h || board.(7) < 4) && List.for_all (fun square -> board.(square) = 0) !black_short_empties && not (castling_threats board (-1) !black_king_pin_1 pseudo_e7 black_short_path !black_short_path_length black_short_bishop_vect black_castlings_knights_vect 1)
          then l := Castling {sort = 3} :: !l
      end;
      if black_long && (!black_long_rook_in_a || possible_long board false) && List.for_all (fun square -> board.(square) = 0) !black_long_empties && not (castling_threats board (-1) !black_king_pin_1 pseudo_e7 black_long_path !black_long_path_length black_long_bishop_vect black_castlings_knights_vect !black_long_directions)
        then l := Castling {sort = 4} :: !l
    end;
  !l

(*Fonction adaptant les droits aux roques en fonction du move*)
let modification_roque move (white_short, white_long, black_short, black_long) =
  if (white_short || white_long || black_short || black_long) then  begin match move with
    |Normal {from; piece; to_; capture = _} ->
      if piece = 6 then begin
        false, false, black_short && to_ <> !from_short_black_rook, black_long && to_ <> !from_long_black_rook
      end
      else if piece = (-6) then begin
        white_short && to_ <> !from_short_white_rook, white_long && to_ <> !from_long_white_rook, false, false
      end
      else begin
        white_short && from <> !from_short_white_rook && to_ <> !from_short_white_rook, white_long && from <> !from_long_white_rook && to_ <> !from_long_white_rook, black_short && from <> !from_short_black_rook && to_ <> !from_short_black_rook, black_long && from <> !from_long_black_rook && to_ <> !from_long_black_rook
      end
    |Castling {sort} ->
      if sort = 1 || sort = 2 then begin
        false, false, black_short, black_long
      end
      else begin
        white_short, white_long, false, false
      end
    |Promotion {from = _; to_; promotion = _; capture = _} ->
      white_short && to_ <> !from_short_white_rook, white_long && to_ <> !from_long_white_rook, black_short && to_ <> !from_short_black_rook, black_long && to_ <> !from_long_black_rook
    |_ -> white_short, white_long, black_short, black_long
  end
  else begin
   (false, false, false, false)
  end

(*Fonction analysant l'historique pour détecter les roques impossibles, en renvoyant un quadruplet de booléens : petit castlings blanc, grands castlings blanc, petit castlings noir, grands castlings noir. Non utilisée*)
let rec roques_possibles listes_coups = match listes_coups with
  |[] -> true, true, true, true
  |h :: t ->
    let prb1, grb1, prn1, grn1 = roques_possibles t in
    let prb2, grb2, prn2, grn2 = modification_roque h (prb1, grb1, prn1, grn1) in
    prb1 && prb2, grb1 && grb2, prn1 && prn2, grn1 && grn2

(*Fonction construisant une list des prises en passant possible d'un joueur*)
let enpassant board white_to_move last_move =
  let l = ref [] in
  if white_to_move then begin
    let aux move = match move with
      |Normal {piece; from; to_; capture = _} when (piece = (-1) && (from < 16) && (to_ > 23)) -> to_
      |_-> (-1)
    in let to_ = aux last_move
    in if to_ <> (-1) then begin
      let droite = to_ + 1
      in if (droite <> 32 && board.(droite) = 1) then begin
        l := Enpassant {from = droite; to_ = droite - 9} :: !l
      end;
      let gauche = to_ - 1
      in if (gauche <> 23 && board.(gauche) = 1) then begin
        l := Enpassant {from = gauche; to_ = gauche - 7} :: !l
      end
    end
  end
  else begin
    let aux move = match move with
      |Normal {piece; from; to_; capture = _} when (piece = 1 && (from > 47) && (to_ < 40)) -> to_
      |_-> (-1)
    in let to_ = aux last_move
    in if to_ <> (-1) then begin
      let droite = to_ + 1
      in if (droite <> 40 && board.(droite) = (-1)) then begin
        l := Enpassant {from = droite; to_ = droite + 7} :: !l
      end;
      let gauche = to_ - 1
      in if (gauche <> 31 && board.(gauche) = (-1)) then begin
        l := Enpassant {from = gauche; to_ = gauche + 9} :: !l
      end
    end
  end;
  !l

(*Fonction permettant de jouer un move sur l'échiquier*)
let make board move = match move with
  |Normal {piece; from; to_; capture = _} -> begin
    board.(from) <- 0;
    board.(to_) <- piece
  end
  |Castling {sort} -> begin
    match sort with
    |1 -> board.(!from_white_king) <- 0; board.(!from_short_white_rook) <- 0; board.(62) <- 6; board.(61) <- 4
    |2 -> board.(!from_white_king) <- 0; board.(!from_long_white_rook) <- 0; board.(58) <- 6; board.(59) <- 4
    |3 -> board.(!from_black_king) <- 0; board.(!from_short_black_rook) <- 0; board.(6) <- (-6); board.(5) <- (-4)
    |_ -> board.(!from_black_king) <- 0; board.(!from_long_black_rook) <- 0; board.(2) <- (-6); board.(3) <- (-4)
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      board.(from) <- 0;
      board.(to_) <- 1;
      board.(to_ + 8) <- 0
    end
    else begin
      board.(from) <- 0;
      board.(to_) <- (-1);
      board.(to_ - 8) <- 0
    end
  end
  |Promotion {from; to_; promotion; capture = _} -> begin
    board.(from) <- 0;
    board.(to_) <- promotion
  end
  |Null -> ()

(*Fonction permettant l'annulation d'un move*)
let unmake board move = match move with
  |Normal {piece; from; to_; capture} -> begin
    board.(from) <- piece;
    board.(to_) <- capture
  end
  |Castling {sort} -> begin
    match sort with
    |1 -> board.(62) <- 0;  board.(61) <- 0; board.(!from_short_white_rook) <- 4;  board.(!from_white_king) <- 6
    |2 -> board.(58) <- 0; board.(59) <- 0; board.(!from_long_white_rook) <- 4;  board.(!from_white_king) <- 6
    |3 -> board.(6) <- 0; board.(5) <- 0; board.(!from_short_black_rook) <- (-4);  board.(!from_black_king) <- (-6)
    |_ -> board.(2) <- 0; board.(3) <- 0; board.(!from_long_black_rook) <- (-4);  board.(!from_black_king) <- (-6)
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      board.(from) <- 1;
      board.(to_) <- 0;
      board.(to_ + 8) <- (-1)
    end
    else begin
      board.(from) <- (-1);
      board.(to_) <- 0;
      board.(to_ - 8) <- 1
    end
  end
  |Promotion {from; to_; promotion; capture} -> begin
    board.(from) <- if promotion > 0 then 1 else (-1);
    board.(to_) <- capture
  end
  |Null -> ()

(*Fonction donnant la square de départ d'un move classique et d'une promotion*)
let from move = match move with
  |Normal {piece = _; from; to_ = _; capture = _} | Enpassant {from; to_ = _} | Promotion {from; to_ = _; capture = _; promotion = _} -> from
  |Castling {sort} -> if sort < 3 then !from_white_king else !from_black_king
  |_ -> (-1)

(*Fonction donnant la square d'arrivée d'un move*)
let to_ move = match move with
  |Normal {piece = _; from = _; to_; capture = _} | Promotion {from = _; to_; capture = _; promotion = _} | Enpassant {from = _; to_} -> to_
  |Castling {sort} -> begin
    match sort with
      |1 -> 62
      |2 -> 58
      |3 -> 6
      |_ -> 2
  end
  |_ -> (-1)

(*Fonction donnant la capture d'un move*)
let capture move = match move with
  |Normal {piece = _; from = _; to_ = _; capture} | Promotion {from = _; to_ = _; capture; promotion = _} -> capture
  |Enpassant {from = _; to_} -> if to_ < 24 then (-1) else 1
  |_ -> 0

(*Fonction donnant la pièce d'un move classique*)
let piece move = match move with
  |Normal {piece; from = _; to_ = _; capture = _} -> piece
  |_ -> 0

(*Fonction indiquant si une pièce cloue une pièce clouable*)
let cloue board chessman white_to_move cord64 vect distance =
  let b = ref false in
  let k = ref distance in
  let iterate = ref true in
  let dame_adverse = queen (not white_to_move) in
  while (!iterate && tab120.(cord64 + (!k * vect)) <> (-1)) do
    let candidate = tab120.(cord64 + (!k * vect)) in
    let dest = board.(candidate) in
    if dame_adverse * dest < 0 then begin
      iterate :=  false
    end
    else if dame_adverse * dest > 0 then begin 
      if dest = chessman || dest = dame_adverse then begin
        b := true;
      end;
      iterate :=  false
    end;
    incr k
  done;
  !b

(*Fonction donnant les cases des pièces clouées (indépendanmment de leur possibilité de mouvement) en considérant que le roi est dans la square en argument*)
let clouees board case_roi white_to_move =
  let ensemble = ref [] in
  let t = tab64.(case_roi) in
  let player_sign = if white_to_move then 1 else (-1) in
  let aux vect piece =
    for i = 0 to 3 do
      let dir = vect.(i) in
      let k = ref 1 in
      let iterate = ref true in
      while (!iterate && tab120.(t + (!k * dir)) <> (-1)) do
        let candidate = tab120.(t + (!k * dir)) in
        let dest = player_sign * board.(candidate) in
        if dest > 0 then begin
          if cloue board (-piece * player_sign) white_to_move t dir (!k + 1) then begin
            ensemble := candidate :: !ensemble
          end;
          iterate :=  false
        end
        else if dest < 0 then begin 
          iterate :=  false
        end;
        incr k
      done
    done;
  in List.iter (fun (vect, piece) -> aux vect piece ) [(rook_vect, 4); (bishop_vect, 3)];
  !ensemble

(*Fonction construisant une list des moves légaux du joueur*)
let legal_moves board white_to_move last_move (white_short, white_long, black_short, black_long) =
  let move_list = ref [] in
  let player_king = king white_to_move in
  let king_position = index_array board player_king in
  let aux move king_position =
    make board move;
    if not (threatened board king_position white_to_move) then begin
      move_list := move :: !move_list
    end;
    unmake board move
  in List.iter (fun prise_en_passant -> aux prise_en_passant king_position) (enpassant board white_to_move last_move);
  if threatened board king_position white_to_move then begin
    let moves, king_moves = pseudo_legal_moves board white_to_move king_position in
    List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
    List.iter (fun other_move -> aux other_move king_position) moves;
    !move_list
  end
  else begin
    let castling_rights = if white_to_move then white_short || white_long else black_short || black_long in
    let pinned_pieces = clouees board king_position white_to_move in
    if pinned_pieces = [] then begin
      let moves, king_moves = pseudo_legal_moves board white_to_move king_position in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      if castling_rights then (castlings board white_to_move (white_short, white_long, black_short, black_long)) @ !move_list @ moves else !move_list @ moves
    end
    else begin
      let moves, king_moves, coups_clouees = pin_moves board white_to_move king_position pinned_pieces in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      List.iter (fun pinned_move -> aux pinned_move king_position) coups_clouees;
      if castling_rights then (castlings board white_to_move (white_short, white_long, black_short, black_long)) @ !move_list @ moves else !move_list @ moves
    end
  end

(*Fonction construisant une list des déplacements possible d'une tour*)
let rook_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let dir = rook_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin 
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un fou*)
let bishop_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 3 do
    let dir = bishop_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un cavalier*)
let knight_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let dir = knight_vect.(i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      let dest = board.(candidate) in
      if piece * dest < 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'une dame*)
let queen_captures board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let dir = king_vect.(i) in
    let k = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!k * dir)) <> (-1)) do
      let candidate = tab120.(tab64_square + (!k * dir)) in
      let dest = board.(candidate) in
      if dest = 0 then begin
        incr k
      end
      else if piece * dest > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list;
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
    let dir = king_vect.(i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidate = tab120.(tab64_square + dir) in
      let dest = board.(candidate) in
      if piece * dest < 0 then begin
        list := Normal {piece = piece; from = square; to_ = candidate; capture = dest} :: !list
      end
    end
  done;
  !list

(*Fonction construisant une list des captures/ promotions possible d'un pion*)
let pawn_captures board square list =
  let piece = board.(square) in
  let p = tab64.(square) in
  if piece > 0 then begin
    let candidate_1 = tab120.(p - 10) in
    if board.(candidate_1) = 0 && square < 16 then begin
      list :=
      Promotion {from = square; to_ = candidate_1; promotion = 5; capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = 4; capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = 3; capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = 2; capture = 0} ::
      !list
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidat3 = tab120.(p - 9) in
      let dest3 = board.(candidat3) in
      if dest3 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = candidat3; capture = dest3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat3; promotion = 5; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 4; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 3; capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = 2; capture = dest3} ::
          !list
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidat4 = tab120.(p - 11) in
      let dest4 = board.(candidat4) in
      if dest4 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = candidat4; capture = dest4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat4; promotion = 5; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 4; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 3; capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = 2; capture = dest4} ::
          !list
        end
      end
    end
  end
  else begin
    let candidate_1 = tab120.(p + 10) in
    if board.(candidate_1) = 0 && square > 47 then begin
      list :=
      Promotion {from = square; to_ = candidate_1; promotion = (-5); capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = (-4); capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = (-3); capture = 0} ::
      Promotion {from = square; to_ = candidate_1; promotion = (-2); capture = 0} :: !list
    end;
    if (square mod 8 <> 0) then begin
      let candidat3 = tab120.(p + 9) in
      let dest3 = board.(candidat3) in
      if dest3 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = candidat3; capture = dest3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat3; promotion = (-5); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-4); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-3); capture = dest3} ::
          Promotion {from = square; to_ = candidat3; promotion = (-2); capture = dest3} ::
          !list
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidat4 = tab120.(p + 11) in
      let dest4 = board.(candidat4) in
      if dest4 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = candidat4; capture = dest4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = candidat4; promotion = (-5); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-4); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-3); capture = dest4} ::
          Promotion {from = square; to_ = candidat4; promotion = (-2); capture = dest4} :: !list
        end
      end
    end
  end

let captures_vect = [|pawn_captures; knight_captures; bishop_captures; rook_captures; queen_captures|]

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pseudo_legal_captures board white_to_move king_position =
  let move_list = ref [] in
  let king_move_list = king_captures board king_position in
  if white_to_move then begin
    let aux from arrive =
      for i = from downto arrive do
        let piece = board.(i) in
        if piece > 0 then begin
          (captures_vect.(piece - 1) board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        let piece = board.(i) in
        if piece < 0 then begin
          (captures_vect.(- piece - 1) board i move_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list

(*Fonction construisant une list des déplacements classique possibles d'un joueur*)
let pin_captures board white_to_move king_position pinned_pieces =
  let move_list = ref [] in
  let king_move_list = king_captures board king_position in
  let pinned_list = ref [] in
  if white_to_move then begin
    let aux from to_ = 
    for i = from downto to_ do
      if not (List.mem i pinned_pieces) then begin
        let piece = board.(i) in
        if piece > 0 then begin
          (captures_vect.(piece - 1) board i move_list)
        end
      end
      else begin
        let piece = board.(i) in (captures_vect.(piece - 1) board i pinned_list)
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for i = from to to_ do
        if not (List.mem i pinned_pieces) then begin
          let piece = board.(i) in
          if piece < 0 then begin
            (captures_vect.(- piece - 1) board i move_list)
          end
        end
        else begin
          let piece = board.(i) in (captures_vect.(- piece - 1) board i pinned_list)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, king_position - 1; king_position + 1, 63]
  end;
  !move_list, king_move_list, !pinned_list

let captures board white_to_move last_move =
  let move_list = ref [] in
  let player_king = king white_to_move in
  let king_position = index_array board player_king in
  let aux move king_position =
    make board move;
    if not (threatened board king_position white_to_move) then begin
      move_list := move :: !move_list
    end;
    unmake board move
  in List.iter (fun prise_en_passant -> aux prise_en_passant king_position) (enpassant board white_to_move last_move);
  if threatened board king_position white_to_move then begin
    let moves, king_moves = pseudo_legal_captures board white_to_move king_position in
    List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
    List.iter (fun other_move -> aux other_move king_position) moves;
    !move_list
  end
  else begin
    let pinned_pieces = clouees board king_position white_to_move in
    if pinned_pieces = [] then begin
      let moves, king_moves = pseudo_legal_captures board white_to_move king_position in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      !move_list @ moves
    end
    else begin
      let moves, king_moves, coups_clouees = pin_captures board white_to_move king_position pinned_pieces in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      List.iter (fun pinned_move -> aux pinned_move king_position) coups_clouees;
     !move_list @ moves
    end
  end

(*Fonction permettant de jouer un move en actualisant les variables d'états de la partie*)
let make_move_1 board move white_to_move last_move castling_rights = 
  make board move;
  castling_rights := modification_roque move !castling_rights;
  last_move := move;
  white_to_move := not !white_to_move

(*Fonction renvoyant le relevé des positions actualisé*)
let new_board_record last_move board_record board white_to_move castling_rights =
  if is_irreversible last_move then begin
    [zobrist board white_to_move last_move castling_rights]
  end
  else begin
    zobrist board white_to_move last_move castling_rights :: !board_record
  end

(*Fonction permettant de jouer un move en actualisant les variables d'états de la partie*)
let make_move_2 board move white_to_move last_move castling_rights moves_record board_record = 
  make_move_1 board move white_to_move last_move castling_rights;
  moves_record := move :: !moves_record;
  board_record := new_board_record move board_record board !white_to_move !castling_rights

(*Fonction permettant de jouer une list de moves*)
let make_list move_list board last_move moves_record board_record castling_rights white_to_move =
  let rec func move_list board last_move moves_record board_record castling_rights white_to_move controle = match move_list with
    |[] -> ()
    |move :: t when !controle ->
      if List.mem move (legal_moves board !white_to_move !last_move !castling_rights) then begin
        make_move_2 board move white_to_move last_move castling_rights moves_record board_record;
        func t board last_move moves_record board_record castling_rights white_to_move controle
      end
      else if move = Null then begin
        func t board last_move moves_record board_record castling_rights white_to_move controle
      end
      else begin
        controle := false
      end
    |_ -> ()
  in func move_list board last_move moves_record board_record castling_rights white_to_move (ref true)

(*Fonction indiquant si un move est valide*)
let is_legal board move white_to_move =
  let b = ref true in
  make board move;
  if piece move = (king white_to_move) then begin
    if threatened board (to_ move) white_to_move then begin
      b := false
    end
  end
  else if threatened board (index_array board (king white_to_move)) white_to_move then begin
    b := false
  end;
  unmake board move;
  !b

(*Fonction indiquant efficacement si un move est valide, utilisée uniquement pour des moves classiques, hors roi*)
let is_legal_effective board move white_to_move king_position in_check pinned_pieces =
  let b = ref true in
  if in_check then begin
    make board move;
    if threatened board king_position white_to_move then begin
      b := false
    end;
    unmake board move
  end
  else if List.mem (from move) pinned_pieces then begin
    make board move;
    if threatened board king_position white_to_move then begin
      b := false
    end;
    unmake board move
  end;
  !b

(*Fonction renvoyant le statut de la partie (2 si elle est en cours, 0 si il y a pat, 1 si les blancs l'emportent, -1 si les noirs l'emportent)*)
let win board white_to_move last_move =
  let winner = ref 2 in
  if white_to_move then begin
    if (legal_moves board white_to_move last_move (false, false, false, false)) = [] then begin
      if threatened board (index_array board 6) true then
        winner := -1
      else
        winner := 0
    end
  end
  else begin
    if (legal_moves board white_to_move last_move (false, false, false, false)) = [] then begin
      if threatened board (index_array board (-6)) false then
        winner := 1
      else
        winner := 0
    end
  end;
  !winner

(*Tableau dont les élément indiquent si la présence de la pièce d'indice correspondant est imcompatible avec la nulle par manque de matériel*)
let insufficient_mating_materiel_vect = [|false; true; false; false; true; true; false|]