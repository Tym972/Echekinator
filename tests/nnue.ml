open Libs.Board
open Libs.Bitboards
open Libs.Zobrist
open Libs.Uci
open Libs.Fen
open Libs.Traduction
open Libs.Evaluation
open Libs.Quiescence
open Libs.Transposition

let nodes_to_search = 50000

let accumulator,n,hidden_weights = [||],0,[||]
let update_acc move capture = match move with
  |Normal {piece; from; to_} -> begin
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
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 749) +. hidden_weights.(i * 768 + 735) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_short_white_rook)
      done
    |2 ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 701) +. hidden_weights.(i * 768 + 711) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_long_white_rook)
      done
    |3 ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 83) +. hidden_weights.(i * 768 + 69) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_short_black_rook)
      done
    |_ ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 35) +. hidden_weights.(i * 768 + 45) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_long_black_rook)
      done
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      let idx_to = 12 * to_ in
      let idx_from = 12 * from in
      let idx_capture = 12 * (to_ + 8) + 6 in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
    else begin
      let idx_to = 12 * to_ + 6 in
      let idx_from = 12 * from + 6 in
      let idx_capture = 12 * (to_ - 8) in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
  end
  |Promotion {from; to_; promotion} -> begin
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

(*(**)
let board_vector = Array.make 768 0.

(**)
let vector board =
  for i = 0 to 767 do
    board_vector.(i) <- 0.
  done;
  for i = 0 to 63 do
    let piece = board.(i) in
    if piece > 0 then begin
      board_vector.(12 * i + (piece - 1)) <- 1.
    end
    else if piece < 0 then begin
      board_vector.(12 * i + (5 - piece)) <- 1.
    end
  done

(**)
let n = 1556

(**)
let hidden_weights = Array.make (n * 768) 0.
let hidden_bias = Array.make n 0.
let output_weight = Array.make n 0.
let output_bias = 0.054845188
let accumulator = Array.make n 0.
let hidden_layer = Array.make n 0.*)

(*let board_of_vector vector =
  let tab_piece = [|1; 2; 3; 4; 5; 6; -1; -2; -3; -4; -5; -6|] in
  let board = Array.make 64 0 in
  for i = 0 to 767 do
    if vector.(i) = 1. then begin
      board.(i / 12) <- tab_piece.(i mod 12)
    end
  done;
  board

let print_matrix a m n =
  for i = 0 to (m - 1) do
    for j = 0 to (n - 1) do
      print_string (string_of_float a.(i * n + j) ^ " ")
    done;
    print_newline ()
  done

let matrix_multiplication (a, m, n1) (b, n2, p) =
  let c = Array.make (m * p) 0. in
    for i = 0 to (m - 1) do
      for j = 0 to (p - 1) do
        c.(i * p + j) <-
        let h = ref 0. in
        for k = 0 to (n1 - 1) do
          h := !h +. a.(i * n2 + k) *. b.(k * p + j)
        done;
        !h
      done
    done;
  c

let matrix_addition (a, m1, n1) (b, m2, n2) =
  let c = Array.make (m1 * n1) 0. in
  if m1 = m2 && n1 = n2 then begin
    for i = 0 to (m1 * n1 - 1) do
      c.(i) <- a.(i) +. b.(i)
    done
  end;
  c

let matrix_multiplication_ones a b m n =
  let tab = Array.make m 0. in
    for i = 0 to n - 1 do
      if b.(i) = 1. then begin
        for j = 0 to m - 1 do
          tab.(j) <- tab.(j) +. a.(j * n + i)
        done
      end
    done;
  tab

let vector_addition a b m =
  let c = Array.make m 0. in
  for i = 0 to (m - 1) do
    c.(i) <- a.(i) +. b.(i)
  done;
  c

let make_accumulator_merdique x =
 matrix_addition ((matrix_multiplication (hidden_weights, n, 768) (x, 768, 1)), n , 1) (hidden_bias, n, 1)

let make_accumulator x =
  vector_addition ((matrix_multiplication_ones hidden_weights x n 768)) (hidden_bias) n

(**)
let relu x = if x > 0. then x else 0.

let make_hidden_layer x =
  let tab = make_accumulator x in
  for i = 0 to n - 1 do
    tab.(i) <- relu tab.(i)
  done;
  tab

let make_output_layer x =
  (matrix_multiplication (output_weight, 1, n) (make_hidden_layer x, n, 1)).(0) +. output_bias

let evaluate () =
  let score = ref output_bias in
  for i = 0 to n - 1 do
    score := !score +. output_weight.(i) *. (if accumulator.(i) > 0. then accumulator.(i) else 0.)
  done;
  !score

let () =
  vector chessboard;
  for i = 0 to (n * 768) - 1 do
    hidden_weights.(i) <- (float_of_int i) *. 8. +. 3. *. (float_of_int (i * i))
  done;
  for i = 0 to n - 1 do
    hidden_bias.(i) <- 3. *. (float_of_int i) -. 7. ;
    output_weight.(i) <- (float_of_int i) *. -7. +. 2. *. (float_of_int (i * i))
  done;
  let acc = make_accumulator board_vector in
  let hid = make_hidden_layer board_vector in
  for i = 0 to n - 1 do
    accumulator.(i) <- acc.(i);
  done;
  for i = 0 to n - 1 do
    hidden_layer.(i) <- hid.(i)
  done*)