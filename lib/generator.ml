(*Module implémentant les fonctions qui permettent de générer les moves possibles à partir d'une position*)

open Board

(*Fonction indiquant si une square est attaquée par une pièce ennemie*)
let threatened board square white_to_move =
  let threat = ref false in
  let tab64_square = tab64.(square) in
  let player_sign = if white_to_move then 1 else (-1) in
  let i = ref 0 in
  while (not !threat && !i < 4) do
    let direction = bishop_vect.(!i) in
    let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
    if !iterate then begin
      let attacker = board.(tab120.(tab64_square + direction)) * player_sign in
      if attacker <> 0 then begin
        iterate :=  false;
        if ((attacker <= (-3) && attacker <> (-4)) || (attacker = (-1) && direction * player_sign < 0)) then begin
          threat := true
        end
      end;
      let distance = ref 2 in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker = board.(tab120.(tab64_square + (!distance * direction))) * player_sign in
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
        if board.(tab120.(tab64_square + direction)) = (-2) * player_sign then begin
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
        let attacker = board.(tab120.(tab64_square + direction)) * player_sign in
        if attacker <> 0 then begin
          iterate :=  false;
          if attacker <= (-4) then begin
            threat := true
          end
        end;
        let distance = ref 2 in
        while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
          let attacker = board.(tab120.(tab64_square + (!distance * direction))) * player_sign in
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

(*Fonction construisant une list des déplacements possible d'une tour*)
let rook_moves board square list =
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = 0} :: !list;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un fou*)
let bishop_moves board square list =
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = 0} :: !list;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
        iterate :=  false
      end
    done
  done

(*Fonction construisant une list des déplacements possible d'un cavalier*)
let knight_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  for i = 0 to 7 do
    let direction = knight_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker <= 0 then begin
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list
      end
    end
  done

(*Fonction construisant une list des déplacements possible d'une dame*)
let queen_moves board square list =
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = 0} :: !list;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
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
    let direction = king_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if piece * attacker <= 0 then begin
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list
      end
    end
  done;
  !list

(*Fonction construisant une list des déplacements possible d'un pion*)
let pawn_moves board square list =
  let piece = board.(square) in
  let tab64_square = tab64.(square) in
  if piece > 0 then begin
    let attacker_square_1 = tab120.(tab64_square - 10) in
    if board.(attacker_square_1) = 0 then begin
      if square > 15 then begin
        list := Normal {piece = 1; from = square; to_ = attacker_square_1; capture = 0} :: !list;
        if (square > 47 && square < 56) then begin
          let attacker_square_2 = tab120.(tab64_square - 20) in
          if board.(attacker_square_2) = 0 then begin
            list := Normal {piece = 1; from = square; to_ = attacker_square_2; capture = 0} :: !list
          end
        end
      end
      else begin
        list :=
        Promotion {from = square; to_ = attacker_square_1; promotion = 5; capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = 4; capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = 3; capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = 2; capture = 0} ::
        !list
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(tab64_square - 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_3; capture = attacker_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = 5; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 4; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 3; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 2; capture = attacker_3} ::
          !list
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(tab64_square - 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_4; capture = attacker_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = 5; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 4; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 3; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 2; capture = attacker_4} ::
          !list
        end
      end
    end
  end
  else begin
    let attacker_square_1 = tab120.(tab64_square + 10) in
    if board.(attacker_square_1) = 0 then begin
      if square < 48 then begin
        list := Normal {piece = (-1); from = square; to_ = attacker_square_1; capture = 0} :: !list;
        if (square > 7 && square < 16) then begin
          let attacker_square_2 = tab120.(tab64_square + 20) in
          if (board.(attacker_square_2) = 0) then begin
            list := Normal {piece = (-1); from = square; to_ = attacker_square_2; capture = 0} :: !list
          end
        end
      end
      else begin
        list :=
        Promotion {from = square; to_ = attacker_square_1; promotion = (-5); capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = (-4); capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = (-3); capture = 0} ::
        Promotion {from = square; to_ = attacker_square_1; promotion = (-2); capture = 0} ::
        !list
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(tab64_square + 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_3; capture = attacker_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = (-5); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-4); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-3); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-2); capture = attacker_3} ::
          !list
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(tab64_square + 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_4; capture = attacker_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = (-5); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-4); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-3); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-2); capture = attacker_4} ::
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

let pin_table = Array.make 64 0

let pin_generator piece square board pinned_list =
  let tab64_square = tab64.(square) in
  let direction = pin_table.(square) in
  let func direction =
    let distance = ref 1 in
    let iterate = ref true in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) in
      if attacker = 0 then begin
        pinned_list := Normal {piece = piece; from = square; to_ = attacker_square; capture = 0} :: !pinned_list;
        incr distance
      end
      else if piece * attacker > 0 then begin
        iterate :=  false
      end
      else begin 
        pinned_list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !pinned_list;
        iterate :=  false
      end
    done
  in match abs piece with
  |1 ->
    if piece > 0 then begin
      if abs direction = 10 then begin
        let attacker_square_1 = tab120.(tab64_square - 10) in
        if board.(attacker_square_1) = 0 then begin
          pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_1; capture = 0} :: !pinned_list;
          if (square > 47 && square < 56) then begin
            let attacker_square_2 = tab120.(tab64_square - 20) in
            if board.(attacker_square_2) = 0 then begin
              pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_2; capture = 0} :: !pinned_list
            end
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square - 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 < 0 then begin
          if square > 15 then begin
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_3; capture = attacker_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = 5; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 4; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 3; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 2; capture = attacker_3} ::
            !pinned_list
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square - 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 < 0 then begin
          if square > 15 then begin
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_4; capture = attacker_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = 5; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 4; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 3; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 2; capture = attacker_4} ::
            !pinned_list
          end
        end
      end
    end
    else begin
      if abs direction = 10 then begin
        let attacker_square_1 = tab120.(tab64_square + 10) in
        if board.(attacker_square_1) = 0 then begin
          pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_1; capture = 0} :: !pinned_list;
          if (square > 7 && square < 16) then begin
            let attacker_square_2 = tab120.(tab64_square + 20) in
            if (board.(attacker_square_2) = 0) then begin
              pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_2; capture = 0} :: !pinned_list
            end
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 9 then begin
        let attacker_square_3 = tab120.(tab64_square + 9) in
        let attacker_3 = board.(attacker_square_3) in
        if attacker_3 > 0 then begin
          if square < 48 then begin
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_3; capture = attacker_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = (-5); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-4); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-3); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-2); capture = attacker_3} ::
            !pinned_list
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square + 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 > 0 then begin
          if square < 48 then begin
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_4; capture = attacker_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = (-5); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-4); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-3); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-2); capture = attacker_4} ::
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
let pin_moves board white_to_move king_position pinned_pieces =
  let move_list = ref [] in
  let king_move_list = king_moves board king_position in
  let pinned_list = ref [] in
  if white_to_move then begin
    let aux from to_ = 
    for square = from downto to_ do
      let piece = board.(square) in
      if not (List.mem square pinned_pieces) then begin
        if piece > 0 then begin
          tabfun.(piece - 1) board square move_list
        end
      end
      else begin
        pin_generator piece square board pinned_list
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for square = from to to_ do
        let piece = board.(square) in
        if not (List.mem square pinned_pieces) then begin
          if piece < 0 then begin
            tabfun.(- piece - 1) board square move_list
          end
        end
        else begin
          pin_generator piece square board pinned_list
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

(*Constantes pour le calcul du hash zobrist et du vecteur NNUE*)
let zobrist_from_white_king = ref 725
let zobrist_from_black_king = ref 59
let zobrist_from_short_white_rook = ref 759
let zobrist_from_long_white_rook = ref 675
let zobrist_from_short_black_rook = ref 93
let zobrist_from_long_black_rook = ref 9

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

(*Cases devant êtres empties pour le castlings*)
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

(*Variable indiquant la direction du grand castlings, valant 1 si le déplacement du roi se fait vers la right, (-1) sinon*)
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
  let list = ref [] in
  if white_to_move then begin
    let aux move = match move with
      |Normal {piece; from; to_; capture = _} when (piece = (-1) && to_ - from = 16) -> to_
      |_-> (-1)
    in let to_ = aux last_move
    in if to_ <> (-1) then begin
      let right = to_ + 1
      in if (right <> 32 && board.(right) = 1) then begin
        list := Enpassant {from = right; to_ = right - 9} :: !list
      end;
      let left = to_ - 1
      in if (left <> 23 && board.(left) = 1) then begin
        list := Enpassant {from = left; to_ = left - 7} :: !list
      end
    end
  end
  else begin
    let aux move = match move with
      |Normal {piece; from; to_; capture = _} when (piece = 1 && from - to_ = 16) -> to_
      |_-> (-1)
    in let to_ = aux last_move
    in if to_ <> (-1) then begin
      let right = to_ + 1
      in if (right <> 40 && board.(right) = (-1)) then begin
        list := Enpassant {from = right; to_ = right + 7} :: !list
      end;
      let left = to_ - 1
      in if (left <> 31 && board.(left) = (-1)) then begin
        list := Enpassant {from = left; to_ = left + 9} :: !list
      end
    end
  end;
  !list

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
  |Promotion {from = _; to_ = _; capture = _; promotion} -> if promotion > 0 then 1 else (-1)
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
let pinned_squares board king_square white_to_move =
  let list = ref [] in
  let tab64_square = tab64.(king_square) in
  let player_sign = if white_to_move then 1 else (-1) in
  let aux vect piece =
    for i = 0 to 3 do
      let direction = vect.(i) in
      let distance = ref 1 in
      let iterate = ref true in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker_square = tab120.(tab64_square + (!distance * direction)) in
        let attacker = player_sign * board.(attacker_square) in
        if attacker > 0 then begin
          if pin board (- piece * player_sign) white_to_move tab64_square direction !distance then begin
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
let legal_moves board white_to_move last_move (white_short, white_long, black_short, black_long) king_position in_check =
  let move_list = ref [] in
  let aux move king_position =
    make board move;
    if not (threatened board king_position white_to_move) then begin
      move_list := move :: !move_list
    end;
    unmake board move
  in List.iter (fun prise_en_passant -> aux prise_en_passant king_position) (enpassant board white_to_move last_move);
  if in_check then begin
    let moves, king_moves = pseudo_legal_moves board white_to_move king_position in
    List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
    List.iter (fun other_move -> aux other_move king_position) moves;
    !move_list
  end
  else begin
    let castling_rights = if white_to_move then white_short || white_long else black_short || black_long in
    let pinned_pieces = pinned_squares board king_position white_to_move in
    if pinned_pieces = [] then begin
      let moves, king_moves = pseudo_legal_moves board white_to_move king_position in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      if castling_rights then (castlings board white_to_move (white_short, white_long, black_short, black_long)) @ !move_list @ moves else !move_list @ moves
    end
    else begin
      let moves, king_moves, pinned_moves = pin_moves board white_to_move king_position pinned_pieces in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      if castling_rights then (castlings board white_to_move (white_short, white_long, black_short, black_long)) @ !move_list @ pinned_moves @ moves else !move_list @ pinned_moves @ moves
    end
  end

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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list;
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
        list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !list
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
      Promotion {from = square; to_ = attacker_square_1; promotion = 5; capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 4; capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 3; capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = 2; capture = 0} ::
      !list
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(p - 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_3; capture = attacker_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = 5; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 4; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 3; capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = 2; capture = attacker_3} ::
          !list
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(p - 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 < 0 then begin
        if square > 15 then begin
          list := Normal {piece = 1; from = square; to_ = attacker_square_4; capture = attacker_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = 5; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 4; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 3; capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = 2; capture = attacker_4} ::
          !list
        end
      end
    end
  end
  else begin
    let attacker_square_1 = tab120.(p + 10) in
    if board.(attacker_square_1) = 0 && square > 47 then begin
      list :=
      Promotion {from = square; to_ = attacker_square_1; promotion = (-5); capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-4); capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-3); capture = 0} ::
      Promotion {from = square; to_ = attacker_square_1; promotion = (-2); capture = 0} :: !list
    end;
    if (square mod 8 <> 0) then begin
      let attacker_square_3 = tab120.(p + 9) in
      let attacker_3 = board.(attacker_square_3) in
      if attacker_3 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_3; capture = attacker_3} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_3; promotion = (-5); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-4); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-3); capture = attacker_3} ::
          Promotion {from = square; to_ = attacker_square_3; promotion = (-2); capture = attacker_3} ::
          !list
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let attacker_square_4 = tab120.(p + 11) in
      let attacker_4 = board.(attacker_square_4) in
      if attacker_4 > 0 then begin
        if square < 48 then begin
          list := Normal {piece = (-1); from = square; to_ = attacker_square_4; capture = attacker_4} :: !list
        end
        else begin
          list :=
          Promotion {from = square; to_ = attacker_square_4; promotion = (-5); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-4); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-3); capture = attacker_4} ::
          Promotion {from = square; to_ = attacker_square_4; promotion = (-2); capture = attacker_4} :: !list
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

let pin_captures_generator piece square board pinned_list =
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
        pinned_list := Normal {piece = piece; from = square; to_ = attacker_square; capture = attacker} :: !pinned_list;
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
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_3; capture = attacker_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = 5; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 4; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 3; capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = 2; capture = attacker_3} ::
            !pinned_list
          end
        end
      end;
      if square mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square - 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 < 0 then begin
          if square > 15 then begin
            pinned_list := Normal {piece = 1; from = square; to_ = attacker_square_4; capture = attacker_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = 5; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 4; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 3; capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = 2; capture = attacker_4} ::
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
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_3; capture = attacker_3} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_3; promotion = (-5); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-4); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-3); capture = attacker_3} ::
            Promotion {from = square; to_ = attacker_square_3; promotion = (-2); capture = attacker_3} ::
            !pinned_list
          end
        end
      end;
      if (square + 1) mod 8 <> 0 && abs direction = 11 then begin
        let attacker_square_4 = tab120.(tab64_square + 11) in
        let attacker_4 = board.(attacker_square_4) in
        if attacker_4 > 0 then begin
          if square < 48 then begin
            pinned_list := Normal {piece = (-1); from = square; to_ = attacker_square_4; capture = attacker_4} :: !pinned_list
          end
          else begin
            pinned_list :=
            Promotion {from = square; to_ = attacker_square_4; promotion = (-5); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-4); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-3); capture = attacker_4} ::
            Promotion {from = square; to_ = attacker_square_4; promotion = (-2); capture = attacker_4} ::
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
let pin_captures board white_to_move king_position pinned_pieces =
  let move_list = ref [] in
  let king_move_list = king_captures board king_position in
  let pinned_list = ref [] in
  if white_to_move then begin
    let aux from to_ = 
    for square = from downto to_ do
      let piece = board.(square) in
      if not (List.mem square pinned_pieces) then begin
        if piece > 0 then begin
          captures_vect.(piece - 1) board square move_list
        end
      end
      else begin
        pin_captures_generator piece square board pinned_list
      end
    done in List.iter (fun (a, b) -> aux a b) [63, king_position + 1; king_position - 1, 0]
  end
  else begin
    let aux from to_ =
      for square = from to to_ do
        let piece = board.(square) in
        if not (List.mem square pinned_pieces) then begin
          if piece < 0 then begin
            captures_vect.(- piece - 1) board square move_list
          end
        end
        else begin
          pin_captures_generator piece square board pinned_list
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
    let pinned_pieces = pinned_squares board king_position white_to_move in
    if pinned_pieces = [] then begin
      let moves, king_moves = pseudo_legal_captures board white_to_move king_position in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
      !move_list @ moves
    end
    else begin
      let moves, king_moves, pinned_moves = pin_captures board white_to_move king_position pinned_pieces in
      List.iter (fun king_move -> aux king_move (to_ king_move)) king_moves;
     !move_list @ pinned_moves @ moves
    end
  end

(*Fonction permettant de jouer un move en actualisant les variables d'états de la partie*)
let make_move_1 board move white_to_move last_move castling_rights = 
  make board move;
  castling_rights := modification_roque move !castling_rights;
  last_move := move;
  white_to_move := not !white_to_move

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
let win board white_to_move last_move king_position in_check =
  let winner = ref 2 in
  if white_to_move then begin
    if (legal_moves board white_to_move last_move (false, false, false, false) king_position in_check) = [] then begin
      if threatened board (index_array board 6) true then
        winner := -1
      else
        winner := 0
    end
  end
  else begin
    if (legal_moves board white_to_move last_move (false, false, false, false) king_position in_check) = [] then begin
      if threatened board (index_array board (-6)) false then
        winner := 1
      else
        winner := 0
    end
  end;
  !winner

(*Tableau dont les élément indiquent si la présence de la pièce d'indice correspondant est imcompatible avec la nulle par manque de matériel*)
let insufficient_mating_materiel_vect = [|false; true; false; false; true; true; false|]