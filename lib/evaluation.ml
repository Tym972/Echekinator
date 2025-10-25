(*Module implémentant des fonctions d'évaluation*)

open Board

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

let mg_value = [| 82; 337; 365; 477; 1025;  0|]
let eg_value = [| 94; 281; 297; 512;  936;  0|]

let mg_pawn_table = [|
      0;   0;   0;   0;   0;   0;  0;   0;
     98; 134;  61;  95;  68; 126; 34; -11;
     -6;   7;  26;  31;  65;  56; 25; -20;
    -14;  13;   6;  21;  23;  12; 17; -23;
    -27;  -2;  -5;  12;  17;   6; 10; -25;
    -26;  -4;  -4; -10;   3;   3; 33; -12;
    -35;  -1; -20; -23; -15;  24; 38; -22;
      0;   0;   0;   0;   0;   0;  0;   0;
  |]

let eg_pawn_table = [|
      0;   0;   0;   0;   0;   0;   0;   0;
    178; 173; 158; 134; 147; 132; 165; 187;
     94; 100;  85;  67;  56;  53;  82;  84;
     32;  24;  13;   5;  -2;   4;  17;  17;
     13;   9;  -3;  -7;  -7;  -8;   3;  -1;
      4;   7;  -6;   1;   0;  -5;  -1;  -8;
     13;   8;   8;  10;  13;   0;   2;  -7;
      0;   0;   0;   0;   0;   0;   0;   0;
|]

let mg_knight_table = [|
    -167; -89; -34; -49;  61; -97; -15; -107;
     -73; -41;  72;  36;  23;  62;   7;  -17;
     -47;  60;  37;  65;  84; 129;  73;   44;
      -9;  17;  19;  53;  37;  69;  18;   22;
     -13;   4;  16;  13;  28;  19;  21;   -8;
     -23;  -9;  12;  10;  19;  17;  25;  -16;
     -29; -53; -12;  -3;  -1;  18; -14;  -19;
    -105; -21; -58; -33; -17; -28; -19;  -23;
|]

let eg_knight_table = [|
    -58; -38; -13; -28; -31; -27; -63; -99;
    -25;  -8; -25;  -2;  -9; -25; -24; -52;
    -24; -20;  10;   9;  -1;  -9; -19; -41;
    -17;   3;  22;  22;  22;  11;   8; -18;
    -18;  -6;  16;  25;  16;  17;   4; -18;
    -23;  -3;  -1;  15;  10;  -3; -20; -22;
    -42; -20; -10;  -5;  -2; -20; -23; -44;
    -29; -51; -23; -15; -22; -18; -50; -64;
|]

let mg_bishop_table = [|
    -29;   4; -82; -37; -25; -42;   7;  -8;
    -26;  16; -18; -13;  30;  59;  18; -47;
    -16;  37;  43;  40;  35;  50;  37;  -2;
     -4;   5;  19;  50;  37;  37;   7;  -2;
     -6;  13;  13;  26;  34;  12;  10;   4;
      0;  15;  15;  15;  14;  27;  18;  10;
      4;  15;  16;   0;   7;  21;  33;   1;
    -33;  -3; -14; -21; -13; -12; -39; -21;
|]

let eg_bishop_table = [|
    -14; -21; -11;  -8; -7;  -9; -17; -24;
     -8;  -4;   7; -12; -3; -13;  -4; -14;
      2;  -8;   0;  -1; -2;   6;   0;   4;
     -3;   9;  12;   9; 14;  10;   3;   2;
     -6;   3;  13;  19;  7;  10;  -3;  -9;
    -12;  -3;   8;  10; 13;   3;  -7; -15;
    -14; -18;  -7;  -1;  4;  -9; -15; -27;
    -23;  -9; -23;  -5; -9; -16;  -5; -17;
|]

let mg_rook_table = [|
     32;  42;  32;  51; 63;  9;  31;  43;
     27;  32;  58;  62; 80; 67;  26;  44;
     -5;  19;  26;  36; 17; 45;  61;  16;
    -24; -11;   7;  26; 24; 35;  -8; -20;
    -36; -26; -12;  -1;  9; -7;   6; -23;
    -45; -25; -16; -17;  3;  0;  -5; -33;
    -44; -16; -20;  -9; -1; 11;  -6; -71;
    -19; -13;   1;  17; 16;  7; -37; -26;
|]

let eg_rook_table = [|
    13; 10; 18; 15; 12;  12;   8;   5;
    11; 13; 13; 11; -3;   3;   8;   3;
     7;  7;  7;  5;  4;  -3;  -5;  -3;
     4;  3; 13;  1;  2;   1;  -1;   2;
     3;  5;  8;  4; -5;  -6;  -8; -11;
    -4;  0; -5; -1; -7; -12;  -8; -16;
    -6; -6;  0;  2; -9;  -9; -11;  -3;
    -9;  2;  3; -1; -5; -13;   4; -20;
|]

let mg_queen_table = [|
    -28;   0;  29;  12;  59;  44;  43;  45;
    -24; -39;  -5;   1; -16;  57;  28;  54;
    -13; -17;   7;   8;  29;  56;  47;  57;
    -27; -27; -16; -16;  -1;  17;  -2;   1;
     -9; -26;  -9; -10;  -2;  -4;   3;  -3;
    -14;   2; -11;  -2;  -5;   2;  14;   5;
    -35;  -8;  11;   2;   8;  15;  -3;   1;
     -1; -18;  -9;  10; -15; -25; -31; -50;
|]

let eg_queen_table = [|
     -9;  22;  22;  27;  27;  19;  10;  20;
    -17;  20;  32;  41;  58;  25;  30;   0;
    -20;   6;   9;  49;  47;  35;  19;   9;
      3;  22;  24;  45;  57;  40;  57;  36;
    -18;  28;  19;  47;  31;  34;  39;  23;
    -16; -27;  15;   6;   9;  17;  10;   5;
    -22; -23; -30; -16; -16; -23; -36; -32;
    -33; -28; -22; -43;  -5; -32; -20; -41;
|]

let mg_king_table = [|
    -65;  23;  16; -15; -56; -34;   2;  13;
     29;  -1; -20;  -7;  -8;  -4; -38; -29;
     -9;  24;   2; -16; -20;   6;  22; -22;
    -17; -20; -12; -27; -30; -25; -14; -36;
    -49;  -1; -27; -39; -46; -44; -33; -51;
    -14; -14; -22; -46; -44; -30; -15; -27;
      1;   7;  -8; -64; -43; -16;   9;   8;
    -15;  36;  12; -54;   8; -28;  24;  14;
|]

let eg_king_table = [|
    -74; -35; -18; -18; -11;  15;   4; -17;
    -12;  17;  14;  17;  17;  38;  23;  11;
     10;  17;  23;  15;  20;  45;  44;  13;
     -8;  22;  24;  27;  26;  33;  26;   3;
    -18;  -4;  21;  24;  27;  23;   9; -11;
    -19;  -3;  11;  21;  23;  16;   7;  -9;
    -27; -11;   4;  13;  14;   4;  -5; -17;
    -53; -34; -21; -11; -28; -14; -24; -43
|]

let mg_tables = [|mg_pawn_table; mg_knight_table; mg_bishop_table; mg_rook_table; mg_queen_table; mg_king_table|]
let eg_tables = [|eg_pawn_table; eg_knight_table; eg_bishop_table; eg_rook_table; eg_queen_table; eg_king_table|]

let mg_table = Array.make 768 0
let eg_table = Array.make 768 0

let flip square =
  let rank = square / 8 in
  let file = square mod 8 in
  (7 - rank) * 8 + file

let () =
  for piece = 1 to 6 do
    for square = 0 to 63 do
      mg_table.(12 * square + (piece - 1)) <- mg_value.(piece - 1) + mg_tables.(piece - 1).(square);
      eg_table.(12 * square + (piece - 1)) <- eg_value.(piece - 1) + eg_tables.(piece - 1).(square);
    done;
  done;
  for piece = (-1) downto (-6) do
    for square = 0 to 63 do
      mg_table.(12 * square + (5 - piece)) <- mg_value.(- piece - 1) + mg_tables.(- piece - 1).(flip square);
      eg_table.(12 * square + (5 - piece)) <- eg_value.(- piece - 1) + eg_tables.(- piece - 1).(flip square);
    done;
  done

let hce board white_to_move =
  let mg = [|0; 0|] in
  let eg = [|0; 0|] in
  let gamephase = ref 0 in
  for square = 0 to 63 do
    let piece = board.(square) in
    if piece > 0 then begin
      mg.(0) <- mg.(0) + mg_table.(12 * square + (piece - 1));
      eg.(0) <- eg.(0) + eg_table.(12 * square + (piece - 1));
      incr gamephase
    end
    else if piece < 0 then begin
      mg.(1) <- mg.(1) + mg_table.(12 * square + (5 - piece));
      eg.(1) <- eg.(1) + eg_table.(12 * square + (5 - piece));
      incr gamephase
    end;
  done;
  let mg_score = mg.(0) - mg.(1) in
  let eg_score = eg.(0) - eg.(1) in
  let phase = min !gamephase 24 in
  let score = (mg_score * phase + eg_score * (24 - phase)) / 24 in
  if white_to_move then
    score
  else  
    - score