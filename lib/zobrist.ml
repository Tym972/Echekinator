(*Module implémentant une fonction de hachage de*)
open Board
open Generator

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de capture en passant*)
let tab_zobrist = Array.make 781 0

let () =
  for i = 0 to 780 do
    tab_zobrist.(i) <- Int64.to_int (Random.int64 4611686018427387903L)
  done

(*Indique la colonne d'une capture en passant potentielle*)
let column_ep move board = match move with
  |Normal {piece; from; to_; capture = _} when (abs piece = 1 && abs (from - to_) = 16 && ((from mod 8 <> 0 && board.(to_ - 1) = - piece) || (from mod 8 <> 7 && board.(to_ + 1) = - piece))) -> from mod 8
  |_ -> -1

(*Fonction de hachage*)
let zobrist board white_to_move last_move (prb, grb, prn, grn) =
  let h = ref 0 in
  for i = 0 to 63 do
    let piece = board.(i) in
    if piece > 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (piece - 1))
    end
    else if piece < 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (5 - piece))
    end
  done;
  if white_to_move then begin
    h := !h lxor tab_zobrist.(768)
  end;
  if prb then begin
    h := !h lxor tab_zobrist.(769)
  end;
  if grb then begin
    h := !h lxor tab_zobrist.(770)
  end;
  if prn then begin
    h := !h lxor tab_zobrist.(771)
  end;
  if grn then begin
    h := !h lxor tab_zobrist.(772)
  end;
  let pep = column_ep last_move board in
  if pep <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep)
  end;
  !h

(*Fonction caulculant la valeur de la fonction de zobrist en fonction de la précédente et du move joué. Non utilisée.*)
let new_zobrist move penultimate_move old_zobrist (aprb, agrb, aprn, agrn) (nprb, ngrb, nprn, ngrn) board =
  let h = ref (old_zobrist lxor tab_zobrist.(768)) in
  let pep_opponent = column_ep penultimate_move board in
  if pep_opponent <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep_opponent)
  end;
  if aprb <> nprb then begin
    h:= !h lxor tab_zobrist.(769)
  end;
  if agrb <> ngrb then begin
    h:= !h lxor tab_zobrist.(770)
  end;
  if aprn <> nprn then begin
    h:= !h lxor tab_zobrist.(771)
  end;
  if agrn <> ngrn then begin
    h:= !h lxor tab_zobrist.(772)
  end;
  let aux move = match move with
    |Normal {piece; from; to_; capture} -> begin
      if (abs piece = 1 && abs (from - to_) = 16) && ((from mod 8 <> 0 && board.(to_ - 1) = - piece) || (from mod 8 <> 7 && board.(to_ + 1) = - piece)) then begin
        h := !h lxor tab_zobrist.(773 + (from mod 8))
      end;
      if piece > 0 then begin
        if capture = 0 then begin
          tab_zobrist.(12 * from + (piece - 1)) lxor tab_zobrist.(12 * to_ + (piece - 1))
        end
        else begin
          tab_zobrist.(12 * from + (piece - 1)) lxor tab_zobrist.(12 * to_ + (piece - 1)) lxor tab_zobrist.(12 * to_ + (5 - capture))
        end
      end
      else begin
        if capture = 0 then begin
          tab_zobrist.(12 * from + (5 - piece)) lxor tab_zobrist.(12 * to_ + (5 - piece))
        end
        else begin
          tab_zobrist.(12 * from + (5 - piece)) lxor tab_zobrist.(12 * to_ + (5 - piece)) lxor tab_zobrist.(12 * to_ + (capture - 1))
        end
      end
    end
    |Castling {sort} -> begin (*ALERTE 960!!!!*)
      match sort with
      |1 -> tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(749) lxor tab_zobrist.(!zobrist_from_short_white_rook) lxor tab_zobrist.(735)
      |2 -> tab_zobrist.(!zobrist_from_white_king) lxor tab_zobrist.(701) lxor tab_zobrist.(!zobrist_from_long_white_rook) lxor tab_zobrist.(711)
      |3 -> tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(83) lxor tab_zobrist.(!zobrist_from_short_black_rook) lxor tab_zobrist.(69)
      |_ -> tab_zobrist.(!zobrist_from_black_king) lxor tab_zobrist.(35) lxor tab_zobrist.(!zobrist_from_long_black_rook) lxor tab_zobrist.(45)
    end
    |Enpassant {from; to_} -> begin
      if from < 32 then begin
        tab_zobrist.(12 * from) lxor tab_zobrist.(12 * to_) lxor tab_zobrist.(12 * (to_ + 8) + 6)
      end
      else begin
        tab_zobrist.(12 * from + 6) lxor tab_zobrist.(12 * to_ + 6) lxor tab_zobrist.(12 * (to_ - 8))
      end
    end
    |Promotion {from; to_; promotion; capture} -> begin
      if to_ < 8 then begin
        if capture = 0 then begin
          tab_zobrist.(12 * from) lxor tab_zobrist.(12 * to_ + (promotion - 1))
        end
        else begin
          tab_zobrist.(12 * from) lxor tab_zobrist.(12 * to_ + (promotion - 1)) lxor tab_zobrist.(12 * to_ + (5 - capture))
        end
      end
      else begin
        if capture = 0 then begin
          tab_zobrist.(12 * from + 6) lxor tab_zobrist.(12 * to_ + (5 - promotion))
        end
        else begin
          tab_zobrist.(12 * from + 6) lxor tab_zobrist.(12 * to_ + (5 - promotion)) lxor tab_zobrist.(12 * to_ + (capture - 1))
        end
      end
    end
    |Null -> 0
  in
  !h lxor aux move

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition board_record n = match board_record with
  |[] -> false
  |h::q ->
    let rec aux liste k = match liste with
      |[] | [_] -> false
      |_::j::t -> (h = j && (k + 1 = n || aux t (k + 1))) || aux t k
    in aux q 1


let adapt_record zobrist_position move depth board_record half_moves =
  if is_irreversible move then begin
    if depth < 8 then begin
      [], 0
    end
    else begin
      [zobrist_position], 0
    end
  end
  else if half_moves + depth < 7 then begin
    [], half_moves + 1
  end
  else begin 
    zobrist_position :: board_record, half_moves + 1
  end