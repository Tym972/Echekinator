open Bitboards

let () =
  for i = 0 to 776 do
    tab_zobrist.(i) <- Random.int64 Int64.max_int
  done;
  for i = 0 to 15 do
    tab_zobrist_castling.(i) <- Random.int64 Int64.max_int
  done

(*Fonction de hachage*)
let zobrist position =
  let state = position.state.(position.ply) in
  let h = ref 0L in
  let aux pieces_bitboard piece =
    let bitboard = ref pieces_bitboard.(piece) in
    while !bitboard <> 0L do
      let from, other_pieces_bitboard = pop_lsb !bitboard in
      h := Int64.logxor !h tab_zobrist.(zobrist_index from piece);
      bitboard := other_pieces_bitboard
    done
  in for piece = 1 to 12 do
    aux position.pieces piece
  done;
  if position.white_to_move = 0 then begin
    h := Int64.logxor !h tab_zobrist.(768)
  end;
  h := Int64.logxor !h tab_zobrist_castling.(state.castling_rights);
  if state.ep_square <> (-1) then begin
    h := Int64.logxor !h tab_zobrist.(769 + (state.ep_square mod 8))
  end;
  !h