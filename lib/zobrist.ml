(*Module implémentant une fonction de hachage de*)
open Board

let [@inline] zobrist_index square piece =
  square * 12 + piece - 1

(*Constantes pour le calcul du hash zobrist et du vecteur NNUE*)
let from_white_king_zobrist = ref (zobrist_index !from_white_king 6)
let from_black_king_zobrist = ref (zobrist_index !from_black_king 12)
let from_short_white_rook_zobrist = ref (zobrist_index !from_short_white_rook 4)
let from_long_white_rook_zobrist = ref (zobrist_index !from_long_white_rook 4)
let from_short_black_rook_zobrist = ref (zobrist_index !from_short_black_rook 10)
let from_long_black_rook_zobrist = ref (zobrist_index !from_long_black_rook 10)

(*On s'en bat les couilles*)
let to_short_white_king_zobrist = zobrist_index to_short_white_king 6
let to_long_white_king_zobrist = zobrist_index to_long_white_king 6
let to_short_black_king_zobrist = zobrist_index to_short_black_king 12
let to_long_black_king_zobrist = zobrist_index to_long_black_king 12
let to_short_white_rook_zobrist = zobrist_index to_short_white_rook 4
let to_long_white_rook_zobrist = zobrist_index to_long_white_rook 4
let to_short_black_rook_zobrist = zobrist_index to_short_black_rook 10
let to_long_black_rook_zobrist = zobrist_index to_long_black_rook 10

let white_to_move_zobrist = 768

let white_short_castling_zobrist = 769
let white_long_castling_zobrist = 770
let black_short_castling_zobrist = 771
let black_long_castling_zobrist = 772

let ep_zobrist = 773

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de capture en passant*)
let tab_zobrist = Array.make 781 0L

let () =
  for i = 0 to 780 do
    tab_zobrist.(i) <- Random.int64 Int64.max_int
  done

(*Fonction de hachage*)
let zobrist position =
  let state = position.state_infos.(position.ply) in
  let h = ref 0L in
  for i = 0 to 63 do
    let piece = position.board.(i) in
    if piece > 0 then begin
      h := Int64.logxor !h tab_zobrist.(12 * i + (piece - 1))
    end
    else if piece < 0 then begin
      h := Int64.logxor !h tab_zobrist.(12 * i + (5 - piece))
    end
  done;
  if position.white_to_move then begin
    h := Int64.logxor !h tab_zobrist.(768)
  end;
  if state.white_short_castling then begin
    h := Int64.logxor !h tab_zobrist.(769)
  end;
  if state.white_long_castling then begin
    h := Int64.logxor !h tab_zobrist.(770)
  end;
  if state.black_short_castling then begin
    h := Int64.logxor !h tab_zobrist.(771)
  end;
  if state.black_long_castling then begin
    h := Int64.logxor !h tab_zobrist.(772)
  end;
  if state.ep_square <> (-1) then begin
    h := Int64.logxor !h tab_zobrist.(773 + (state.ep_square mod 8))
  end;
  !h

let zobrist_chessboard =
  zobrist {
    board = chessboard;
    white_to_move = true;
    ply = 0;
    state_infos = state_info_array
  }