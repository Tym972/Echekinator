(*Module implémentant le type Mouvement, les constantes et les fonctions de bases du programme*)

(*Program version*)
let project_name = "Echekinator"

(*Type for chess moves*)
type move =
  |Castling of {sort : int}
  |Enpassant of  {from : int; to_ : int}
  |Normal of {piece : int; from : int; to_ : int; capture : int}
  |Promotion of {from : int; to_ : int; promotion : int; capture : int}
  |Null

(*Table of coordinates of a chessboard*)
let coord = [|
  "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8";
  "a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7";
  "a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6";
  "a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5";
  "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4";
  "a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3";
  "a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2";
  "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1"
|]

(* Hash table mapping chessboard coordinates to indices in the coord array *)
let hash_coord =
  let ht = Hashtbl.create 64 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ("a8", 0);  ("b8", 1);  ("c8", 2);  ("d8", 3);  ("e8", 4);  ("f8", 5);  ("g8", 6);  ("h8", 7);
      ("a7", 8);  ("b7", 9);  ("c7", 10); ("d7", 11); ("e7", 12); ("f7", 13); ("g7", 14); ("h7", 15);
      ("a6", 16); ("b6", 17); ("c6", 18); ("d6", 19); ("e6", 20); ("f6", 21); ("g6", 22); ("h6", 23);
      ("a5", 24); ("b5", 25); ("c5", 26); ("d5", 27); ("e5", 28); ("f5", 29); ("g5", 30); ("h5", 31);
      ("a4", 32); ("b4", 33); ("c4", 34); ("d4", 35); ("e4", 36); ("f4", 37); ("g4", 38); ("h4", 39);
      ("a3", 40); ("b3", 41); ("c3", 42); ("d3", 43); ("e3", 44); ("f3", 45); ("g3", 46); ("h3", 47);
      ("a2", 48); ("b2", 49); ("c2", 50); ("d2", 51); ("e2", 52); ("f2", 53); ("g2", 54); ("h2", 55);
      ("a1", 56); ("b1", 57); ("c1", 58); ("d1", 59); ("e1", 60); ("f1", 61); ("g1", 62); ("h1", 63)];
  ht

(* 120-element array where -1 represents an off-board square *)
let tab120 = [| 
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1;  0;  1;  2;  3;  4;  5;  6;  7; -1;
  -1;  8;  9; 10; 11; 12; 13; 14; 15; -1;
  -1; 16; 17; 18; 19; 20; 21; 22; 23; -1;
  -1; 24; 25; 26; 27; 28; 29; 30; 31; -1;
  -1; 32; 33; 34; 35; 36; 37; 38; 39; -1;
  -1; 40; 41; 42; 43; 44; 45; 46; 47; -1;
  -1; 48; 49; 50; 51; 52; 53; 54; 55; -1;
  -1; 56; 57; 58; 59; 60; 61; 62; 63; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1
|]

(* 64-element array mapping squares to their indices in tab120 *)
let tab64 = [| 
  21; 22; 23; 24; 25; 26; 27; 28;
  31; 32; 33; 34; 35; 36; 37; 38;
  41; 42; 43; 44; 45; 46; 47; 48;
  51; 52; 53; 54; 55; 56; 57; 58;
  61; 62; 63; 64; 65; 66; 67; 68;
  71; 72; 73; 74; 75; 76; 77; 78;
  81; 82; 83; 84; 85; 86; 87; 88;
  91; 92; 93; 94; 95; 96; 97; 98
|]

(*Possible directions of movement of a rook in the table tab64*)
let rook_vect = [|(-10); 10; (-1); 1|]

(*Possible directions of movement of a bishop in the table tab64*)
let bishop_vect = [|(-11); 11; (-9); 9|]

(*Possible directions of movement of a knight in the table tab64*)
let knight_vect = [|(-8); 8; (-12); 12; (-19); 19; (-21); 21|]

(*Possible directions of movement of a king in the table tab64*)
let king_vect = [|(-10); 10; (-1); 1; (-11); 11; (-9); 9|]

(*Initial chessboard setup. The pieces are represented by numbers, positive for white, negative for black. Empty square: 0; pawn: 1; knight: 2; bishop: 3; rook: 4; queen: 5; king: 6*)
let chessboard = [|
  (-4); (-2); (-3); (-5); (-6); (-3); (-2); (-4);
  (-1); (-1); (-1); (-1); (-1); (-1); (-1); (-1);
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  1; 1; 1; 1; 1; 1; 1; 1;
  4; 2; 3; 5; 6; 3; 2; 4
  |]

(**)
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
let hidden_layer = Array.make n 0.

(*Array used in print_board*)
let tab_print = [|"   |"; " P |"; " N |"; " B |"; " R |"; " Q |"; " K |"; " p |"; " n |"; " b |"; " r |"; " q |"; " k |"|]

(*Print the board in ASCII*)
let print_board board =
  let display = ref "   +---+---+---+---+---+---+---+---+\n" in
  for i = 0 to 7 do
    let k_list = ref [] in
    let k = string_of_int (8 - i) ^ "  |" in
    for j = 8 * i to 8 + 8 * i - 1 do
      let piece = board.(j) in
      k_list := tab_print.(if piece >= 0 then piece else (6 - piece)) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")

(*Function returning the win value associated with a player loss*)
let lose player_is_white = if player_is_white then (-1) else 1

(*Function returning the representation of the player's king in the chessboard*)
let king player_is_white = if player_is_white then 6 else (-6)

(*Function returning the representation of the player's queen in the chessboard*)
let queen player_is_white = if player_is_white then 5 else (-5)

(*Function returning the representation of the player's knight in the chessboard*)
let knight player_is_white = if player_is_white then 2 else (-2)

(*Function returning the representation of the player's bishop in the chessboard*)
let bishop player_is_white = if player_is_white then 3 else (-3)

(*Function returning the representation of the player's pawn in the chessboard*)
let pawn player_is_white = if player_is_white then 1 else (-1)

(*Function returning the representation of the player's rook in the chessboard*)
let rook player_is_white = if player_is_white then 4 else (-4)

(*Fonction indiquant si un move est irrémédiable (poussée de pion ou capture)*)
let is_irreversible move = match move with
  |Enpassant _ | Promotion _ -> true
  |Normal {piece; from = _; to_ = _; capture} when (abs piece = 1 || capture <> 0) -> true
  |_ -> false

let isquiet move = match move with
  |Normal {piece = _; from = _; to_ = _; capture} when capture <> 0 -> false
  |Enpassant _ | Promotion _ -> false
  |_ -> true

(*Fonction renvoyant l'indice de la première occurence d'un élément dans un tableau*)
let index_array array element =
  if element < 0 then begin
    let rec aux i =
      if array.(i) = element then i else aux (i + 1)
    in aux 0
  end
  else begin
    let rec aux i =
      if array.(i) = element then i else aux (i - 1)
    in aux 63
  end

(*Merge sort*)
let merge_sort l =
  let rec split l = match l with
    |[] -> [], []
    |[x] -> [x] , []
    |h::g::t -> let tg, td = split t in h::tg, g::td
  in let rec merge l1 l2 = match (l1, l2) with
    |[], l | l, [] -> l
    |h1 :: t1, h2 :: t2 -> if h1 >= h2 then h1 :: merge t1 l2 else h2 :: merge t2 l1
  in let rec tri_f l = match l with
    |[] | [_] -> l
    |_ -> let lg, ld = split l in merge (tri_f lg) (tri_f ld)
  in tri_f l

(*Max depth reached by the search*)
let max_depth = 255

(**)
let max_pv_length = max_depth

(**)
let pv_table = Array.make ((max_pv_length) * (max_pv_length + 1) / 2) Null

(**)
let pv_length = Array.make (max_pv_length + 1) 0

(*Variable used to forcefully stop the search*)
let out_of_time = ref false

(*Node counter*)
let node_counter = ref 0

(*Node limit*)
let node_limit = ref max_int

(*TT entries tracker*)
let transposition_counter = ref 0

(*"Go" counter*)
let go_counter = ref 0

let zugzwang = ref true

let (position_aspects : (bool * move * (bool * bool * bool * bool) * int list * int * int) array) = Array.make (max_depth + 40) (true, Null, (true, true, true, true), [], 0, 0)

let board = Array.copy chessboard