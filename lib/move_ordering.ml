open Board
open Bitboards

(*Valeur des pièces pour le tri*)
let tabvalue = [|0; 10; 32; 33; 51; 88; 950; 10; 32; 33; 51; 88; 950|]

let get_all_attackers target pieces_bitboards total_occupancy =
  ((generate_pawn_attacks target 1) &&& pieces_bitboards.(pawn)) |||
  ((generate_pawn_attacks target 0) &&& pieces_bitboards.(black_pawn)) |||
  (knight_table.(target) &&& (pieces_bitboards.(knight) ||| pieces_bitboards.(black_knight))) |||
  ((generate_bishop_attacks target total_occupancy) &&&
    (pieces_bitboards.(queen) ||| pieces_bitboards.(bishop) |||
    pieces_bitboards.(black_queen) ||| pieces_bitboards.(black_bishop))) |||
  ((generate_rook_attacks target total_occupancy) &&&
    (pieces_bitboards.(queen) ||| pieces_bitboards.(rook) |||
    pieces_bitboards.(black_queen) ||| pieces_bitboards.(black_rook))) |||
  (king_table.(target) &&& (pieces_bitboards.(king) ||| pieces_bitboards.(black_king)))

let get_least_valuable_piece attackers pieces_bitboards player_pieces attacker_type =
  let lvp_bitboard = ref 0L in
  let piece = ref 1 in (* 0 = pion, 1 = cavalier, etc. dans ton tableau player_pieces *)
  while !lvp_bitboard = 0L && !piece <= 6 do
    let subset = attackers &&& pieces_bitboards.(player_pieces.(!piece)) in
    if subset <> 0L then begin
      lvp_bitboard := lsb subset;
      attacker_type := player_pieces.(!piece)
    end;
    incr piece
  done;
  !lvp_bitboard

let see position move =
  let from = get_move_from move in
  let to_ = get_move_to move in
  let flag = get_move_flag move in
  let total_occupancy = ref (position.occupancy.(0) ||| position.occupancy.(1)) in
  let pieces_bitboards = position.pieces in
  let current_side = ref position.white_to_move in
  let attackers = ref (get_all_attackers to_ pieces_bitboards !total_occupancy) in
  let gain = Array.make 20 0 in
  if flag <> 5 then begin
    gain.(0) <- tabvalue.(position.board.(to_))
  end
  else begin
    gain.(0) <- tabvalue.(position.board.(from));
    total_occupancy := !total_occupancy ^^^ single_bitboards_tab.(to_ - push_vects.(!current_side))
  end;
  let depth = ref 1 in
  let from_bitboard = ref (single_bitboards_tab.(from)) in
  let attacker = ref (position.board.(from)) in
  while !from_bitboard <> 0L do
    total_occupancy := !total_occupancy ^^^ !from_bitboard;
    attackers := !attackers &&& (Int64.lognot !from_bitboard);
    gain.(!depth) <- tabvalue.(!attacker) - gain.(!depth - 1);
    incr depth;
    current_side := !current_side lxor 1;
    if !attacker mod 6 <> knight then begin
      let sliders =
        ((generate_bishop_attacks to_ !total_occupancy) &&& 
          (pieces_bitboards.(bishop) ||| pieces_bitboards.(black_bishop) ||| 
            pieces_bitboards.(queen)  ||| pieces_bitboards.(black_queen))) |||
        ((generate_rook_attacks to_ !total_occupancy) &&& 
          (pieces_bitboards.(rook)  ||| pieces_bitboards.(black_rook) ||| 
            pieces_bitboards.(queen) ||| pieces_bitboards.(black_queen))) 
      in attackers := !attackers ||| (sliders &&& !total_occupancy)
    end;
    from_bitboard := get_least_valuable_piece !attackers pieces_bitboards pieces_rep.(!current_side) attacker
  done;
  for i = (!depth - 2) downto 1 do
    gain.(i - 1) <- - (max (-gain.(i - 1)) gain.(i))
  done;
  gain.(0)

let killer_moves = Array.make (2 * max_depth) 0
let history_moves = Array.make 8192 0
let working_array = Array.init (max_depth + 40) (fun _ -> Array.make 218 0)

type ordering_tables = {
  killer_moves : int array;
  history_moves : int array;
  working_array : int array array
}

let history_index white_to_move move =
  4096 * white_to_move + 64 * (get_move_from move) + get_move_to move

let move_ordering ordering_tables position moves number_of_moves search_ply hash_move ordering_array =
  let score move move_index =
    if move = hash_move then begin
      ordering_array.(move_index) <- - 1000000
    end
    else if isquiet move then begin
      if ordering_tables.killer_moves.(2 * search_ply) = move land 0xfff then begin
        ordering_array.(move_index) <- 2000000
      end
      else if ordering_tables.killer_moves.(2 * search_ply + 1) = move land 0xfff then begin
        ordering_array.(move_index) <- 1000000
      end
      else begin
        ordering_array.(move_index) <- ordering_tables.history_moves.(history_index position.white_to_move move)
      end
    end
    else begin
      let see_score = see position move in
      if see_score >= 0 then
        ordering_array.(move_index) <- 3000000 + see_score
      else
        ordering_array.(move_index) <- see_score
    end;
  in for i = 0 to number_of_moves - 1 do
    score moves.(i) i;
  done

let move_picker moves ordering_array number_of_moves =
  let max_index = ref (-1) in
  let max_value = ref (- 10000) in
  for i = 0 to number_of_moves - 1 do
    if ordering_array.(i) > !max_value then begin
      max_value := ordering_array.(i);
      max_index := i
    end
  done;
  if !max_index <> (-1) then begin
    ordering_array.(!max_index) <- - 100000;
    moves.(!max_index)
  end
  else begin
    0
  end