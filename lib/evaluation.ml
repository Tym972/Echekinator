(*Module implémentant des fonctions d'évaluation*)

open Board
open Piece_square_tables
open Generator

(*Valeur des pièces pour l'évaluation*)
let tabvalue = [|0; 10; 32; 33; 51; 88; 950|]

let eval_materiel2 board (tb, tn) =
  let material = ref 0 in
  let position = ref 0 in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i)
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i)
    end
  done;
  !material, !position

let treatment white_to_move material position =
  if white_to_move then begin
    100 * material + position
  end
  else begin
    -(100 * material + position)
  end

(*Fonction indique la différence du nombre de structure de pions doublées entre les deux joueurs*)
let doubled board =
  let difference_doubles_pions = ref 0 in
  for i = 8 to 55 do
    if board.(i) = (-1) then begin
      if board.(i + 8) = (-1) then begin
        difference_doubles_pions := !difference_doubles_pions + 1
      end
    end
    else if board.(i) = 1 then begin
      if board.(i - 8) = 1 then begin
        difference_doubles_pions := !difference_doubles_pions - 1
      end
    end
  done;
  !difference_doubles_pions

let eval_materiel3 board (tb, tn) white_to_move =
  let material = ref 0 in
  let position = ref 0 in
  let white_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  let black_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i);
      white_pieces.(square) <- white_pieces.(square) + 1
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i);
      black_pieces.(- square) <- black_pieces.(- square) + 1
    end
  done;
  (*let only_white_king () = white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
  in let only_black_king () = black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0
  in let score_draw =
    let func () =
      let white_minor = white_pieces.(2) + white_pieces.(3) in
      let black_minor = black_pieces.(2) + black_pieces.(3) in
      white_minor < 3 && black_minor < 3 && begin
        (white_minor < 2 && black_minor < 2) || (*K vs K, K + Minor vs K + Minor*)
        ((white_pieces.(3) = 1 && (white_minor = 1 || black_minor > 0)) || (black_pieces.(3) = 1 && (black_minor = 1 || white_minor > 0))) || (*K + B + B vs K + B*)
        ((white_pieces.(2) = 2 && black_minor < 2) || black_pieces.(2) = 2 && white_minor < 2) (*K + N + N vs K + Minor, K + N + N vs K*)
      end
    in white_pieces.(1) = 0 && black_pieces.(1) = 0
      &&
      (only_white_king && only_black_king ||
      (white_pieces.(4) = 0 && black_pieces.(4) = 0 && white_pieces.(5) = 0 && black_pieces.(5) = 0 && func ()))*)
  zugzwang :=
    if white_to_move then
      white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
    else
      black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0;
  (*in let draw =
    if interior then

    else
      false*)
  (*if score_draw then
    0, 0
  else*)
    !material, !position
  
let eval1_q board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta, king_position, king_in_check in
  let material, position = eval_materiel3 board tab_ouverture white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let eval2_q board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta, king_position, king_in_check in
  let material, position = eval_materiel3 board tab_mdg white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let eval3_q board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta, king_position, king_in_check in
  let material, position = eval_materiel3 board tab_finale white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let traitement2 white_to_move material position =
  if white_to_move then begin
    100. *. material +. position
  end
  else begin
    -. (100. *. material +. position)
  end

let evolved board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta, king_position, king_in_check in
  let tab_pieces = [|ref 0; ref 0; ref 0; ref 0; ref 0; ref 0|] in
  let note_ouverture(*, note_mdj*), note_finale = ref 0(*, ref 0*), ref 0 in
  let tab_phase = [|1; 1; 2; 4|] in
  let material = ref 0 in
  for i = 0 to 63 do
    let piece = board.(i) in
    if piece > 0 then begin
      incr tab_pieces.(piece - 1);
      material := !material + tabvalue.(piece);
      note_ouverture := !note_ouverture + tab_pieces_blanches_ouverture.(piece - 1).(i);
      (*note_mdj := !note_mdj + tab_pieces_blanches_mdg.(piece - 1).(i);*)
      note_finale := !note_finale + tab_pieces_blanches_finale.(piece - 1).(i)
    end
    else if piece < 0 then begin
      incr tab_pieces.(- piece - 1);
      material := !material - tabvalue.(- piece);
      note_ouverture := !note_ouverture - tab_pieces_noires_ouverture.(abs piece - 1).(i);
      (*note_mdj := !note_mdj - tab_pieces_noires_mdg.(abs piece - 1).(i);*)
      note_finale := !note_finale - tab_pieces_noires_finale.(abs piece - 1).(i)
    end
  done;
  let phase = ref 0 in
  for i = 1 to 4 do
    phase := !phase + !(tab_pieces.(i)) * tab_phase.(i - 1)
  done;
  if !phase <= 2 then begin
    0
  end
  else begin
    let phase_2 = ((float_of_int !phase) *. 256. +. ((float_of_int !phase) /. 2.)) /. (float_of_int !phase) in
    treatment white_to_move !material (int_of_float (((float_of_int !note_ouverture *. (256. -. phase_2)) +. ((float_of_int !note_finale *. phase_2) /. 256.)) /. 5.))
  end

(*Fonction indiquant si les deux tours d'un player son connectées*)
let tours_connectees board player = 
  let b = ref false in
  if Array.mem (rook player) board then begin
    let t = tab64.(index_array board (rook player)) in
    let s1 = ref true in
    let i = ref 0 in
    while (!i < 4 && !s1) do
      let dir = rook_vect.(!i) in
      let k = ref 1 in
      let s2 = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s2) do
        let dest = board.(tab120.(t + (!k * dir))) in
        if dest <> 0 then begin
          if dest = rook player then begin
            b := true;
            s1 := false
          end;
          s2 := false
        end
        else
          incr k
      done;
      incr i
    done;
  end;
  !b

(*Fonction indiquant si chaque player à moins de 3 pièces hors roi et pion sur l'échiquier, ou si leur nombre est inférieur à 6*)
let pieces_esseulee board =
  let pieces_blanches = ref 0 in
  let pieces_noires = ref 0 in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 1 then begin
      pieces_blanches := !pieces_blanches + 1
    end
    else if square < (-1) then begin
      pieces_noires := !pieces_noires + 1
    end
  done;
  (!pieces_blanches < 3 && !pieces_noires < 3) || ((!pieces_blanches + !pieces_noires) < 6)

(*Fonction indiquant si l'un des deux joueurs n'a plus que son roi*)
let roi_seul board =
  let blancs = ref true in
  let noirs = ref true in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 && square <> 6 then begin
      blancs := false
    end;
    if square < 0 && square <> (-6) then begin
      noirs := false
    end
  done;
  !blancs || !noirs

(*Fonction indiquant si une partie est dans sa phase finale*)
let finale board =
  pieces_esseulee board || roi_seul board

open Zobrist

let board_vector = Array.make 781 0

let vector board white_to_move last_move (prb, grb, prn, grn) =
  for i = 0 to 63 do
    let piece = board.(i) in
    if piece > 0 then begin
      board_vector.(12 * i + (piece - 1)) <- 1
    end
    else if piece < 0 then begin
      board_vector.(12 * i + (5 - piece)) <- 1
    end
  done;
  if white_to_move then begin
    board_vector.(768) <- 1
  end;
  if prb then begin
    board_vector.(769) <- 1
  end;
  if grb then begin
    board_vector.(770) <- 1
  end;
  if prn then begin
    board_vector.(771) <- 1
  end;
  if grn then begin
    board_vector.(772) <- 1
  end;
  let pep = column_ep last_move board in
  if pep <> (-1) then begin
    board_vector.(773 + pep) <- 1
  end

let new_vector move penultimate_move (aprb, agrb, aprn, agrn) (nprb, ngrb, nprn, ngrn) board =
  let pep_adversaire = column_ep penultimate_move board in
  if pep_adversaire <> (-1) then begin
    board_vector.(773 + pep_adversaire) <- 0
  end;
  if aprb <> nprb then begin
    board_vector.(769) <- 0
  end;
  if agrb <> ngrb then begin
    board_vector.(770) <- 0
  end;
  if aprn <> nprn then begin
    board_vector.(771) <- 0
  end;
  if agrn <> ngrn then begin
    board_vector.(772) <- 0
  end;
  match move with
    |Normal {piece; from; to_; capture} -> begin
      if (abs piece = 1 && abs (from - to_) = 16) && ((from mod 8 <> 0 && board.(to_ - 1) = - piece) || (from mod 8 <> 7 && board.(to_ + 1) = - piece)) then begin
        board_vector.(773 + (from mod 8)) <- 1
      end;
      if piece > 0 then begin
        if capture = 0 then begin
          board_vector.(12 * from + (piece - 1)) <- 0;
          board_vector.(12 * to_ + (piece - 1)) <- 1
        end
        else begin
          board_vector.(12 * from + (piece - 1)) <- 0;
          board_vector.(12 * to_ + (piece - 1)) <- 1;
          board_vector.(12 * to_ + (5 - capture)) <- 0
        end
      end
      else begin
        if capture = 0 then begin
          board_vector.(12 * from + (5 - piece)) <- 0;
          board_vector.(12 * to_ + (5 - piece)) <- 1
        end
        else begin
          board_vector.(12 * from + (5 - piece)) <- 0;
          board_vector.(12 * to_ + (5 - piece)) <- 1;
          board_vector.(12 * to_ + (capture - 1)) <- 0
        end
      end
    end
    |Castling {sort} -> begin (*ALERTE 960!!!!*)
      match sort with
      |1 ->
        board_vector.(!zobrist_from_white_king) <- 0;
        board_vector.(749) <- 1;
        board_vector.(!zobrist_from_short_white_rook) <- 0;
        board_vector.(735) <- 1
      |2 ->
        board_vector.(!zobrist_from_white_king) <- 0;
        board_vector.(701) <- 1;
        board_vector.(!zobrist_from_long_white_rook) <- 0;
        board_vector.(711) <- 1
      |3 ->
        board_vector.(!zobrist_from_black_king) <- 0;
        board_vector.(83) <- 1;
        board_vector.(!zobrist_from_short_black_rook) <- 0;
        board_vector.(69) <- 1
      |_ ->
        board_vector.(!zobrist_from_black_king) <- 0;
        board_vector.(35) <- 1;
        board_vector.(!zobrist_from_long_black_rook) <- 0;
        board_vector.(45) <- 1
    end
    |Enpassant {from; to_} -> begin
      if from < 32 then begin
        board_vector.(12 * from) <- 0;
        board_vector.(12 * to_) <- 1;
        board_vector.(12 * (to_ + 8) + 6) <- 0
      end
      else begin
        board_vector.(12 * from + 6) <- 0;
        board_vector.(12 * to_ + 6) <- 1;
        board_vector.(12 * (to_ - 8)) <- 0
      end
    end
    |Promotion {from; to_; promotion; capture} -> begin
      if to_ < 8 then begin
        if capture = 0 then begin
          board_vector.(12 * from) <- 0;
          board_vector.(12 * to_ + (promotion - 1)) <- 1;
        end
        else begin
          board_vector.(12 * from) <- 0;
          board_vector.(12 * to_ + (promotion - 1)) <- 1;
          board_vector.(12 * to_ + (5 - capture)) <- 0;
        end
      end
      else begin
        if capture = 0 then begin
          board_vector.(12 * from + 6) <- 0;
          board_vector.(12 * to_ + (5 - promotion)) <- 0;
        end
        else begin
          board_vector.(12 * from + 6) <- 0;
          board_vector.(12 * to_ + (5 - promotion)) <- 1;
          board_vector.(12 * to_ + (capture - 1)) <- 0;
        end
      end
    end
    |Null -> ()