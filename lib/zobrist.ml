(*Module implémentant une fonction de hachage de*)
open Board

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
let zobrist board trait_aux_blancs dernier_move (prb, grb, prn, grn) =
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
  if trait_aux_blancs then begin
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
  let pep = column_ep dernier_move board in
  if pep <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep)
  end;
  !h

(*Fonction caulculant la valeur de la fonction de zobrist en fonction de la précédente et du move joué. Non utilisée.*)
let new_zobrist move avant_dernier_move ancien_zobrist (aprb, agrb, aprn, agrn) (nprb, ngrb, nprn, ngrn) board =
  let h = ref (ancien_zobrist lxor tab_zobrist.(768)) in
  let pep_adversaire = column_ep avant_dernier_move board in
  if pep_adversaire <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep_adversaire)
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
    |Castling {sort} -> begin
      match sort with
      |1 -> tab_zobrist.(725) lxor tab_zobrist.(749) lxor tab_zobrist.(759) lxor tab_zobrist.(735)
      |2 -> tab_zobrist.(725) lxor tab_zobrist.(701) lxor tab_zobrist.(675) lxor tab_zobrist.(711)
      |3 -> tab_zobrist.(59) lxor tab_zobrist.(83) lxor tab_zobrist.(93) lxor tab_zobrist.(69)
      |_ -> tab_zobrist.(59) lxor tab_zobrist.(35) lxor tab_zobrist.(9) lxor tab_zobrist.(45)
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
let repetition liste_releve_plateau n = match liste_releve_plateau with
  |[] -> false
  |h::q ->
    let rec aux liste k = match liste with
      |[] | [_] -> false
      |_::j::t -> (h = j && (k + 1 = n || aux t (k + 1))) || aux t k
    in aux q 1