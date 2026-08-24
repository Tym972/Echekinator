(*Module implémentant le type Mouvement, les constantes et les fonctions de bases du programme*)

open Bitboards

(*Program version*)
let project_name = "Echekinator 1.1"

(*Table of coordinates of a chessboard*)
let coord = [|
  "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1";
  "a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2";
  "a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3";
  "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4";
  "a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5";
  "a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6";
  "a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7";
  "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8"
|]

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

let max_moves = 2048

(**)
let max_pv_length = max_depth

(**)
let pv_table = Array.make ((max_pv_length) * (max_pv_length + 1) / 2) 0

(**)
let pv_length = Array.make max_pv_length 0

(*SMP variables*)
let threads_number = ref 1
let min_threads_number = 1
let max_threads_number = 1024

(*Variables MultiPV*)
let multipv = ref 1
let min_multipv = 1
let max_multipv = 256

(*Variable used to forcefully stop the search*)
let stop_search = Array.make max_threads_number false

let total_counter counter =
  let total = ref counter.(0) in
  for thread = 1 to !threads_number - 1 do
    total := !total + counter.(thread)
  done;
  !total

(*Node counter*)
let node_counter = Array.make max_threads_number 0

(*Node limit*)
let node_limit = ref max_int

(*TT entries tracker*)
let transposition_counter = Array.make max_threads_number 0

(*"Go" counter*)
let go_counter = ref 0

let start_time = ref (Mtime_clock.counter ())
let soft_bound = ref Mtime.Span.max_span
let hard_bound = ref Mtime.Span.max_span
let ponder_time = ref Mtime.Span.max_span

let chess_960 = ref false

let create_empty_state () = {
  ep_square = (-1);
  castling_rights = 15;
  half_moves = 0;
  zobrist = 0L;
  captured_piece = 0;
  in_check = false
}

let copy_state state = {
  ep_square = state.ep_square;
  castling_rights = state.castling_rights;
  half_moves = state.half_moves;
  zobrist = state.zobrist;
  captured_piece = state.captured_piece;
  in_check = state.in_check
}

let create_position () = {
  white_to_move = 0;
  game_ply = 0;
  state_array = Array.init max_moves (fun _ -> create_empty_state ());
  pieces = Array.make 13 0L;
  occupancy = Array.make 2 0L;
  mailbox = Array.make 64 0;
  moves = Array.init (max_depth + 40) (fun _ -> Array.make 218 0);
  number_of_moves = Array.make (max_depth + 40) 0
}

let copy_position position = {
  white_to_move = position.white_to_move;
  game_ply = position.game_ply;
  state_array = Array.map copy_state position.state_array;
  pieces = Array.copy position.pieces;
  occupancy = Array.copy position.occupancy;
  mailbox = Array.copy position.mailbox;
  moves = Array.map Array.copy position.moves;
  number_of_moves = Array.copy position.number_of_moves
  }

let startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"