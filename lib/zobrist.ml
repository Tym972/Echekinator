(*Module implémentant une fonction de hachage de*)
open Board

(*Constantes pour le calcul du hash zobrist et du vecteur NNUE*)
let zobrist_from_white_king = ref 725
let zobrist_from_black_king = ref 59
let zobrist_from_short_white_rook = ref 759
let zobrist_from_long_white_rook = ref 675
let zobrist_from_short_black_rook = ref 93
let zobrist_from_long_black_rook = ref 9

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de capture en passant*)
let tab_zobrist = Array.make 781 0

let () =
  for i = 0 to 780 do
    tab_zobrist.(i) <- Int64.to_int (Random.int64 4611686018427387903L)
  done

(*Fonction de hachage*)
let zobrist (position : position) =
  let h = ref 0 in
  for i = 0 to 63 do
    let piece = position.board.(i) in
    if piece > 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (piece - 1))
    end
    else if piece < 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (5 - piece))
    end
  done;
  if position.white_to_move then begin
    h := !h lxor tab_zobrist.(768)
  end;
  if position.castling_rights.white_short then begin
    h := !h lxor tab_zobrist.(769)
  end;
  if position.castling_rights.white_long then begin
    h := !h lxor tab_zobrist.(770)
  end;
  if position.castling_rights.black_short then begin
    h := !h lxor tab_zobrist.(771)
  end;
  if position.castling_rights.black_long then begin
    h := !h lxor tab_zobrist.(772)
  end;
  if position.ep_square <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + (position.ep_square mod 8))
  end;
  !h

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition board_record n = match board_record with
  |[] -> false
  |h::q ->
    let rec aux liste k = match liste with
      |[] | [_] -> false
      |_::j::t -> (h = j && (k + 1 = n || aux t (k + 1))) || aux t k
    in aux q 1

let zobrist_chessboard =
  zobrist {
    board = Array.copy chessboard;
    white_to_move = true;
    ep_square = (-1);
    castling_rights = {
      white_short = true;
      white_long = true;
      black_short = true;
      black_long = true
    };
    board_record = [];
    half_moves = 0;
    zobrist_position = 0;
    last_capture = 0
  }