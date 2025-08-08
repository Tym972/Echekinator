open Libs.Board
open Libs.Generator

let pieces = [|1; 2; 3; 4; 5; 6; (-1); (-2); (-3); (-4); (-5); (-6)|]

let index_of_bitboard bitboard =
  let index = ref [] in
  if bitboard <> 0L then begin
    for i = 0 to 63 do
      if (Int64.logand) (Int64.shift_right bitboard i) 1L = 1L then index := (63 - i) :: !index
    done
  end;
  !index

let mailbox_of_bitboard bitboard =
  let mailbox = Array.make 64 0 in
  let rec aux_1 mailbox index piece = match index with
  |[] -> ()
  |h::t ->
    mailbox.(h) <- piece;
    aux_1 mailbox t piece
  in for i = 0 to 11 do
    aux_1 mailbox (index_of_bitboard bitboard.(i)) pieces.(i)
  done;
  mailbox

let bitboard_of_mailbox mailbox =
  let bitboard = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|]
  in for i = 63 downto 0 do
    let piece = mailbox.(i) in
    if piece > 0 then begin
      bitboard.(piece - 1) <- (Int64.logor) bitboard.(piece - 1) (Int64.shift_left 1L (63 - i))
    end
    else if piece < 0 then begin
      bitboard.(5 - piece) <- (Int64.logor) bitboard.(5 - piece) (Int64.shift_left 1L (63 - i))
    end
  done;
  bitboard

let update_bitboard move bitboard = match move with
  |Normal {piece; from; to_; capture} -> begin
    let white_to_move = piece > 0 in
    let idx = if white_to_move then piece - 1 else 5 - piece in
    bitboard.(idx) <- (Int64.logxor) bitboard.(idx) ((Int64.logor) (Int64.shift_left 1L (63 - to_)) (Int64.shift_left 1L (63 - from)));
    if capture <> 0 then begin
      let idxt = if white_to_move then 5 - capture else capture - 1 in
      bitboard.(idxt) <- (Int64.logxor) bitboard.(idxt) (Int64.shift_left 1L (63 - to_))
    end
  end
  |Promotion {from; to_; promotion; capture} -> begin
    let white_to_move = promotion > 0 in
    let idx = if white_to_move then 0 else 6 in
    bitboard.(idx) <- (Int64.logxor) bitboard.(idx) (Int64.shift_left 1L (63 - from));
    if capture <> 0 then begin
      let idxt = if white_to_move then 5 - capture else capture - 1 in
      bitboard.(idxt) <- (Int64.logxor) bitboard.(idxt) (Int64.shift_left 1L (63 - to_))
    end;
    let idxp = if white_to_move then promotion - 1 else 5 - promotion in
    bitboard.(idxp) <- (Int64.logxor) bitboard.(idxp) (Int64.shift_left 1L (63 - to_))
  end
  |Castling {sort} -> begin
    match sort with
    |1 ->
      bitboard.(5) <- (Int64.logxor) bitboard.(5) ((Int64.logor) (Int64.shift_left 1L 1) (Int64.shift_left 1L (63 - !from_white_king)));
      bitboard.(3) <- (Int64.logxor) bitboard.(3) ((Int64.logor) (Int64.shift_left 1L 2) (Int64.shift_left 1L (63 - !from_short_white_rook)));
    |2 ->
      bitboard.(5) <- (Int64.logxor) bitboard.(5) ((Int64.logor) (Int64.shift_left 1L 5) (Int64.shift_left 1L (63 - !from_white_king)));
      bitboard.(3) <- (Int64.logxor) bitboard.(3) ((Int64.logor) (Int64.shift_left 1L 4) (Int64.shift_left 1L (63 - !from_long_white_rook)));
    |3 ->
      bitboard.(11) <- (Int64.logxor) bitboard.(11) ((Int64.logor) (Int64.shift_left 1L 57) (Int64.shift_left 1L (63 - !from_black_king)));
      bitboard.(9) <- (Int64.logxor) bitboard.(9) ((Int64.logor) (Int64.shift_left 1L 58) (Int64.shift_left 1L (63 - !from_short_black_rook)));
    |_ ->
      bitboard.(11) <- (Int64.logxor) bitboard.(11) ((Int64.logor) (Int64.shift_left 1L 61) (Int64.shift_left 1L (63 - !from_black_king)));
      bitboard.(9) <- (Int64.logxor) bitboard.(9) ((Int64.logor) (Int64.shift_left 1L 60) (Int64.shift_left 1L (63 - !from_long_black_rook)));
  end
  |Enpassant {from; to_} -> begin
     if from < 32 then begin
      bitboard.(0) <- (Int64.logxor) bitboard.(0) ((Int64.logor) (Int64.shift_left 1L (63 - to_)) (Int64.shift_left 1L (63 - from)));
      bitboard.(6) <- (Int64.logxor) bitboard.(6) (Int64.shift_left 1L (55 - to_));
    end
    else begin
      bitboard.(6) <- (Int64.logxor) bitboard.(6) ((Int64.logor) (Int64.shift_left 1L (63 - to_)) (Int64.shift_left 1L (63 - from)));
      bitboard.(0) <- (Int64.logxor) bitboard.(0) (Int64.shift_left 1L (71 - to_));
    end
  end
  |_-> ()