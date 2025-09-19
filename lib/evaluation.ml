(*Module implémentant des fonctions d'évaluation*)

open Board
open Piece_square_tables

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
  
let eval1_q board white_to_move =
  let material, position = eval_materiel3 board tab_ouverture white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let eval2_q board white_to_move =
  let material, position = eval_materiel3 board tab_mdg white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let eval3_q board white_to_move =
  let material, position = eval_materiel3 board tab_finale white_to_move in
  treatment white_to_move (material + 2 * (doubled board)) (position / 5)

let traitement2 white_to_move material position =
  if white_to_move then begin
    100. *. material +. position
  end
  else begin
    -. (100. *. material +. position)
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

let board_of_vector vector =
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
  done