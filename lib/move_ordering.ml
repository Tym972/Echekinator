open Board
open Bitboards

(*Valeur des pièces pour le tri*)
let tabvalue = [|10; 32; 33; 51; 88; 950|]

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

let get_least_valuable_piece attackers pieces_bitboards attacker_type white_to_move =
  let lvp_bitboard = ref 0L in
  let piece = ref 0 in
  while !lvp_bitboard = 0L && !piece <= king do
    let subset = attackers &&& pieces_bitboards.(!piece + 6 * white_to_move) in
    if subset <> 0L then begin
      lvp_bitboard := lsb subset;
      attacker_type := !piece
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
    gain.(0) <- tabvalue.(position.board.(to_) mod 6)
  end
  else begin
    gain.(0) <- tabvalue.(position.board.(from) mod 6);
    total_occupancy := !total_occupancy ^^^ single_bitboards_tab.(to_ - push_vects.(!current_side))
  end;
  let depth = ref 1 in
  let from_bitboard = ref (single_bitboards_tab.(from)) in
  let attacker = ref (position.board.(from) mod 6) in
  while !from_bitboard <> 0L do
    total_occupancy := !total_occupancy ^^^ !from_bitboard;
    attackers := !attackers &&& (Int64.lognot !from_bitboard);
    gain.(!depth) <- tabvalue.(!attacker) - gain.(!depth - 1);
    incr depth;
    current_side := !current_side lxor 1;
    if !attacker <> knight then begin
      let sliders =
        ((generate_bishop_attacks to_ !total_occupancy) &&& 
          (pieces_bitboards.(bishop) ||| pieces_bitboards.(black_bishop) ||| 
            pieces_bitboards.(queen)  ||| pieces_bitboards.(black_queen))) |||
        ((generate_rook_attacks to_ !total_occupancy) &&& 
          (pieces_bitboards.(rook)  ||| pieces_bitboards.(black_rook) ||| 
            pieces_bitboards.(queen) ||| pieces_bitboards.(black_queen))) 
      in attackers := !attackers ||| (sliders &&& !total_occupancy)
    end;
    from_bitboard := get_least_valuable_piece !attackers pieces_bitboards attacker !current_side
  done;
  for i = (!depth - 2) downto 1 do
    gain.(i - 1) <- - (max (-gain.(i - 1)) gain.(i))
  done;
  gain.(0)

let history_index white_to_move move =
  4096 * white_to_move + 64 * (get_move_from move) + get_move_to move

let mvv_lva_tab = [|[|15; 25; 35; 45; 55|]; [|14; 24; 34; 44; 54|]; [|13; 23; 33; 43; 53|]; [|12; 22; 32; 42; 52|]; [|11; 21; 31; 41; 51|]; [|10; 20; 30; 40; 50|]|]

type search_tables = {
  history_moves : int array;
  pickers : picker array
}

let create_search_tables () = { 
  history_moves = Array.make 8192 0;
  pickers = Array.init (max_depth + 40) (fun _ -> {
    hash_move = 0;
    killer1 = 0;
    killer2 = 0;
    quiet_moves = Array.make 218 0;
    number_of_quiets = 0;
    quiet_scores = Array.make 218 0;
    capture_moves = Array.make 100 0;
    number_of_captures = 0;
    captures_scores = Array.make 100 0;
    stage = Stage_TT;
  })
}

let copy_search_tables search_tables = {
  history_moves = Array.copy search_tables.history_moves;
  pickers =
  Array.map (fun picker -> {
    hash_move = picker.hash_move;
    killer1 = picker.killer1;
    killer2 = picker.killer2;
    quiet_moves = Array.copy picker.quiet_moves;
    number_of_quiets = picker.number_of_quiets;
    quiet_scores = Array.copy picker.quiet_scores;
    capture_moves = Array.copy picker.capture_moves;
    number_of_captures = picker.number_of_captures;
    captures_scores = Array.copy picker.captures_scores;
    stage = picker.stage;
  }) search_tables.pickers
}

(*Search :
Phase 1 : TT move
Phase 2 : Good Captures
Phase 3 : Killers
Phase 4 : History moves
Phase 5 : Bad Captures*)

let move_picker moves number_of_moves scores min_score =
  let max_index = ref (-1) in
  let max_score = ref min_score in
  for i = 0 to number_of_moves - 1 do
    if scores.(i) > !max_score then begin
      max_score := scores.(i);
      max_index := i
    end
  done;
  if !max_index <> (-1) then begin
    scores.(!max_index) <- - 100000;
    moves.(!max_index)
  end
  else begin
    0
  end

let rec next_move position picker search_tables = match picker.stage with
  |Stage_TT ->
    picker.stage <- Stage_Good_Captures;
    let hash_move = picker.hash_move in
    if hash_move <> 0 then begin
      hash_move
    end
    else begin
      next_move position picker search_tables
    end
  |Stage_Good_Captures ->
    let hash_move = picker.hash_move in
    let capture_moves = picker.capture_moves in
    let captures_scores = picker.captures_scores in
    if picker.number_of_captures = 0 then begin
      legal_moves position picker phase_all;
      let board = position.board in
      for index = 0 to picker.number_of_captures - 1 do
        let capture = capture_moves.(index) in
        if capture <> hash_move then begin
          let to_square = get_move_to capture in
          let from_square = get_move_from capture in
          let victim_piece = board.(to_square) mod 6 in
          let attacker_piece = board.(from_square) mod 6 in
          let victim_value = tabvalue.(victim_piece) in
          let attacker_value = tabvalue.(attacker_piece) in
          let flag = get_move_flag capture in
          if flag > 7 then begin
            captures_scores.(index) <- 20000 + 100 * (flag lxor 4) + mvv_lva_tab.(attacker_piece).(victim_piece)
          end
          else if victim_value >= attacker_value then begin
            captures_scores.(index) <- 10000 + mvv_lva_tab.(attacker_piece).(victim_piece)
          end
          else begin
            let see_capture = see position capture in
            if see_capture >= 0 then begin
              captures_scores.(index) <- 10000 + mvv_lva_tab.(attacker_piece).(victim_piece)
            end
            else begin
              captures_scores.(index) <- 1000 + see_capture
            end
          end
        end
        else begin
          captures_scores.(index) <- - 1000
        end
      done
    end;
    let move = move_picker capture_moves picker.number_of_captures captures_scores 9999 in
    if move = 0 then begin
      picker.stage <- Stage_Killers;
      let quiet_moves = picker.quiet_moves in
      let quiet_scores = picker.quiet_scores in
      let hash_move = picker.hash_move in
      let killer1 = picker.killer1 in
      let killer2 = picker.killer2 in
      for index = 0 to picker.number_of_quiets - 1 do
        let quiet_move = quiet_moves.(index) in
        if quiet_move <> hash_move then begin
          if quiet_move land 0xfff = killer1 then begin
            quiet_scores.(index) <- 200000
          end
          else if quiet_move land 0xfff = killer2 then begin
            quiet_scores.(index) <- 100000
          end
          else begin
            quiet_scores.(index) <- search_tables.history_moves.(history_index position.white_to_move quiet_move)
          end
        end
        else begin
          quiet_scores.(index) <- - 1000
        end
      done;
      next_move position picker search_tables
    end
    else begin
      move
    end
  |Stage_Killers ->
    let move = move_picker picker.quiet_moves picker.number_of_quiets picker.quiet_scores 99999 in
    if move = 0 then begin
      picker.stage <- Stage_History;
      next_move position picker search_tables
    end
    else begin
      move
    end
  |Stage_History -> 
    let move = move_picker picker.quiet_moves picker.number_of_quiets picker.quiet_scores (-1) in
    if move = 0 then begin
      picker.stage <- Stage_Bad_Captures;
      next_move position picker search_tables
    end
    else begin
      move
    end
  |Stage_Bad_Captures ->
    let move = move_picker picker.capture_moves picker.number_of_captures picker.captures_scores 0 in
    if move = 0 then begin
      picker.stage <- Stage_Done;
      next_move position picker search_tables
    end
    else begin
      move
    end
  |Stage_Done -> 0

(*Qsearch (when not in check) :
Phase 1 : TT move
Phase 2 : Good Captures*)

let rec qsearch_next_move position picker search_tables = match picker.stage with
  |Stage_TT ->
    picker.stage <- Stage_Good_Captures;
    let hash_move = picker.hash_move in
    if not (isquiet hash_move) then begin
      hash_move
    end
    else begin
      qsearch_next_move position picker search_tables
    end
  |Stage_Good_Captures ->
    let hash_move = picker.hash_move in
    let capture_moves = picker.capture_moves in
    let captures_scores = picker.captures_scores in
    if picker.number_of_captures = 0 then begin
      legal_moves position picker phase_capture;
      let board = position.board in
      for index = 0 to picker.number_of_captures - 1 do
        let capture = capture_moves.(index) in
        if capture <> hash_move then begin
          let to_square = get_move_to capture in
          let from_square = get_move_from capture in
          let victim_piece = board.(to_square) mod 6 in
          let attacker_piece = board.(from_square) mod 6 in
          let victim_value = tabvalue.(victim_piece) in
          let attacker_value = tabvalue.(attacker_piece) in
          let flag = get_move_flag capture in
          if flag > 7 then begin
            captures_scores.(index) <- 20000 + 100 * (flag lxor 4) + mvv_lva_tab.(attacker_piece).(victim_piece)
          end
          else if victim_value >= attacker_value then begin
            captures_scores.(index) <- 10000 + mvv_lva_tab.(attacker_piece).(victim_piece)
          end
          else begin
            let see_capture = see position capture in
            if see_capture >= 0 then begin
              captures_scores.(index) <- 10000 + mvv_lva_tab.(attacker_piece).(victim_piece)
            end
            else begin
              captures_scores.(index) <- - 1000
            end
          end
        end
        else begin
          captures_scores.(index) <- - 1000
        end
      done
    end;
    let move = move_picker capture_moves picker.number_of_captures captures_scores 9999 in
    if move = 0 then begin
      picker.stage <- Stage_Done;
      qsearch_next_move position picker search_tables
    end
    else begin
      move
    end
  |_ -> 0