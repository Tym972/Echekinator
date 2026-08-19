let (&&&) = Int64.logand
let (|||) = Int64.logor
let (^^^) = Int64.logxor

let [@inline] lsb bitboard =
  bitboard &&& (Int64.neg bitboard)

let de_bruijn_constant = 0x03f79d71b4cb0a89L

let index64 = [|
    0;  1; 48;  2; 57; 49; 28;  3;
   61; 58; 50; 42; 38; 29; 17;  4;
   62; 55; 59; 36; 53; 51; 43; 22;
   45; 39; 33; 30; 24; 18; 12;  5;
   63; 47; 56; 27; 60; 41; 37; 16;
   54; 35; 52; 21; 44; 32; 23; 11;
   46; 26; 40; 15; 34; 20; 31; 10;
   25; 14; 19;  9; 13;  8;  7;  6
|]

let [@inline] lsb_index bitboard = 
  index64.(Int64.to_int (Int64.shift_right_logical (Int64.mul de_bruijn_constant (lsb bitboard)) 58))

let [@inline] population_count bitboard =
  let count = ref 0 in
  let bb = ref bitboard in
  while !bb <> 0L do
    incr count;
    bb := !bb ^^^ (lsb !bb)
  done;
  !count

(*Renvoie l'indice du lsb et le dégage du bitboard*)
let [@inline] pop_lsb bitboard =
  let lsb_bitboard = lsb bitboard in
  lsb_index lsb_bitboard, (bitboard ^^^ lsb_bitboard)

let rec lsb_list bitboard =
  if bitboard <> 0L then begin
    let lsb_bb = lsb bitboard in
    lsb_bb :: lsb_list (bitboard ^^^ lsb_bb)
  end
  else begin
    []
  end

let rec index_list bitboard =
  if bitboard <> 0L then begin
    let lsb_bb = lsb bitboard in
    lsb_index lsb_bb :: index_list (bitboard ^^^ lsb_bb)
  end
  else begin
    []
  end

let single_bitboards_tab = Array.init 64 (fun i -> Int64.shift_left 1L i)

let pawn = 1
let knight = 2
let bishop = 3
let rook = 4
let queen = 5
let king = 6

let black_pawn = 7
let black_knight = 8
let black_bishop = 9
let black_rook = 10
let black_queen = 11
let black_king = 12

let white_pieces = [|0; 1; 2; 3;  4;  5; 6 |]
let black_pieces = [|0; 7; 8; 9; 10; 11; 12|]

let pieces_rep = [|white_pieces; black_pieces|]

let [@inline] is_pawn piece = piece mod 6 = 1
let [@inline] is_knight piece = piece mod 6 = 2
let [@inline] is_bishop piece = piece mod 6 = 3
let [@inline] is_rook piece = piece mod 6 = 4
let [@inline] is_queen piece = piece mod 6 = 5
let [@inline] is_king piece = piece mod 6 = 0

(*          binary move bits                       hexidecimal constants
    
    0000 0000 0011 1111    from square                 0x3f
    0000 1111 1100 0000    to square                   0xfc0
    1111 0000 0000 0000    flag                        0xf000

*)

(*        flags                         hexidecimal constants
    
    0000      quiet move                      0x0
    0001      double pawn push                0x1
    0010      short castle                    0x2
    0011      long castle                     0x3
    0100      capture                         0x4
    0101      ep capture                      0x5

    1000      knight promotion                0x8
    1001      bishop promotion                0x9
    1010      rook promotion                  0xa
    1011      queen promotion                 0xb
    1100      knight promotion capture        0xc
    1101      bishop promotion capture        0xd
    1110      rook promotion capture          0xe
    1111      queen promotion capture         0xf

*)

let [@inline] encode_move from to_ flag =
  from lor (Int.shift_left to_  6) lor (Int.shift_left flag 12)

let [@inline] get_move_from move =
  move land 0x3f

let [@inline] get_move_to move =
  (move land 0xfc0) lsr 6

let [@inline] get_move_flag move =
  (move land 0xf000) lsr 12

let [@inline] is_capture flag =
  flag land 4 <> 0

let [@inline] isquiet move =
  (move land 0xf000) lsr 12 < 4

let push_vects = [|8; -8|]

let rows =
  [|0x00000000000000FFL; 0x000000000000FF00L; 0x0000000000FF0000L; 0x00000000FF000000L;
    0x000000FF00000000L; 0x0000FF0000000000L; 0x00FF000000000000L; 0xFF00000000000000L|]

let columns =
  [|0x8080808080808080L; 0x4040404040404040L; 0x2020202020202020L; 0x1010101010101010L;
    0x0808080808080808L; 0x0404040404040404L; 0x0202020202020202L; 0x0101010101010101L|]

let double_push_ranks = [|rows.(1); rows.(6)|]
let promotion_ranks = [|rows.(7); rows.(0)|]


(*        castling rights                             

    0000          no castling possible
    0001          white 0-0
    0010          white 0-0-0
    0100          black 0-0
    1000          black 0-0-0

*)

type player_castlings_info = {
  short_castling : int;
  long_castling : int;
  mutable from_king : int;
  mutable from_short_rook : int;
  mutable from_long_rook : int;
  to_short_king : int;
  to_long_king : int;
  to_short_rook : int;
  to_long_rook : int;
  mutable short_castling_mask : int64;
  mutable long_castling_mask : int64;
  mutable short_castling_safe_mask : int64;
  mutable long_castling_safe_mask : int64;
  mutable short_castling_empty_mask : int64;
  mutable long_castling_empty_mask : int64;
}

let white_castling_info = {
  short_castling = 1;
  long_castling = 2;
  from_king = 4;
  from_short_rook = 7;
  from_long_rook = 0;
  to_short_king = 6;
  to_long_king = 2;
  to_short_rook = 5;
  to_long_rook = 3;
  short_castling_mask = 0L;
  long_castling_mask = 0L;
  short_castling_safe_mask = 0L;
  long_castling_safe_mask = 0L;
  short_castling_empty_mask = 0L;
  long_castling_empty_mask = 0L
}

let black_castling_info = {
  short_castling = 4;
  long_castling = 8;
  from_king = 60;
  from_short_rook = 63;
  from_long_rook = 56;
  to_short_king = 62;
  to_long_king = 58;
  to_short_rook = 61;
  to_long_rook = 59;
  short_castling_mask = 0L;
  long_castling_mask = 0L;
  short_castling_safe_mask = 0L;
  long_castling_safe_mask = 0L;
  short_castling_empty_mask = 0L;
  long_castling_empty_mask = 0L
}

let castling_rights_masks = Array.make 64 15

let castling_infos = [|white_castling_info; black_castling_info|]

(*Squares between the rook and the edge*)
let ambiguity_masks = [|0L; 0L; 0L; 0L|]

type state = {
  mutable ep_square : int;
  mutable castling_rights : int;
  mutable half_moves : int;
  mutable zobrist : int64;
  mutable captured_piece : int;
  mutable in_check : bool
}

type position = {
  mutable white_to_move : int;
  mutable ply : int;
  mutable pieces : int64 array;
  mutable occupancy : int64 array;
  mutable state : state array;
  mailbox : int array;
  moves : int array array;
  number_of_moves : int array
}

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

let white_pawn_attacks_table = Array.make 64 0L
let black_pawn_attacks_table = Array.make 64 0L
let pawn_attack_tables = [|white_pawn_attacks_table; black_pawn_attacks_table|]

let enpassant_table = Array.make 64 0L

let bishop_masks = Array.make 64 0L
let rook_masks = Array.make 64 0L

let knight_table = Array.make 64 0L
let king_table = Array.make 64 0L

let bishop_blockers = Array.make 64 [||]
let rook_blockers = Array.make 64 [||]

let bishop_moves = Array.make 64 [||]
let rook_moves = Array.make 64 [||]

let bishop_magics = Array.make 64 0L
let rook_magics = Array.make 64 0L

let bishop_shifts = Array.make 64 0
let rook_shifts = Array.make 64 0

let bishop_table = Array.make 64 [||]
let rook_table = Array.make 64 [||]

let init_castling_info () =
  let aux white_to_move castling_info =
    let from_king = castling_info.from_king in
    let from_short_rook = castling_info.from_short_rook in
    let from_long_rook = castling_info.from_long_rook in
    castling_rights_masks.(from_king) <- 15 lxor castling_info.short_castling lxor castling_info.long_castling;
    castling_rights_masks.(from_short_rook) <- 15 lxor castling_info.short_castling;
    castling_rights_masks.(from_long_rook) <- 15 lxor castling_info.long_castling;
    castling_info.short_castling_mask <- single_bitboards_tab.(from_king) ||| single_bitboards_tab.(from_short_rook);
    castling_info.long_castling_mask <- single_bitboards_tab.(from_king) ||| single_bitboards_tab.(from_long_rook);
    for square = from_king + 1 to castling_info.to_short_king do
      castling_info.short_castling_safe_mask <- castling_info.short_castling_safe_mask ||| single_bitboards_tab.(square);
      castling_info.short_castling_empty_mask <- castling_info.short_castling_empty_mask ||| single_bitboards_tab.(square)
    done;
    for square = from_short_rook - 1 downto castling_info.to_short_rook do
      castling_info.short_castling_empty_mask <- castling_info.short_castling_empty_mask ||| single_bitboards_tab.(square)
    done;
    for square = from_king - 1 downto castling_info.to_long_king do
      castling_info.long_castling_safe_mask <- castling_info.long_castling_safe_mask ||| single_bitboards_tab.(square);
      castling_info.long_castling_empty_mask <- castling_info.long_castling_empty_mask ||| single_bitboards_tab.(square)
    done;
    for square = from_long_rook + 1 to castling_info.to_long_rook do
      castling_info.long_castling_empty_mask <- castling_info.long_castling_empty_mask ||| single_bitboards_tab.(square)
    done;
    let a_square, h_square = if white_to_move = 0 then
      0, 7
    else
      56, 63
    in for square = a_square to from_long_rook - 1 do
      ambiguity_masks.(white_to_move) <- ambiguity_masks.(white_to_move) ||| single_bitboards_tab.(square)
    done;
    for square = h_square downto from_short_rook + 1 do
      ambiguity_masks.(white_to_move + 1) <- ambiguity_masks.(white_to_move + 1) ||| single_bitboards_tab.(square)
    done;
  in aux 0 white_castling_info;
  aux 1 black_castling_info

let init_pawn () =
  let aux attack_table vect square =
    let pawn_to = tab120.(tab64.(square) + vect) 
    in if pawn_to <> (-1) then begin
      attack_table.(square) <- attack_table.(square) ||| single_bitboards_tab.(pawn_to)
    end
  in for square = 0 to 63 do
    aux white_pawn_attacks_table 9 square;
    aux white_pawn_attacks_table 11 square;
    aux black_pawn_attacks_table (-9) square;
    aux black_pawn_attacks_table (-11) square
  done;
  for square = 24 to 39 do
    aux enpassant_table 1 square;
    aux enpassant_table (-1) square;
  done

let init_not_slidings () =
  for square = 0 to 63 do
    let tab64_square = tab64.(square) in
    for i = 0 to 7 do
      let knight_to = tab120.(tab64_square + knight_vect.(i)) in
      if knight_to <> (-1) then begin
        knight_table.(square) <- knight_table.(square) ||| single_bitboards_tab.(knight_to)
      end;
      let king_to = tab120.(tab64_square + king_vect.(i)) in
      if king_to <> (-1) then begin
        king_table.(square) <- king_table.(square) ||| single_bitboards_tab.(king_to)
      end;
    done;
  done

let init_sliding_masks () =
  let aux vect masks =
    for square = 0 to 63 do
      let tab64_square = tab64.(square) in
      for i = 0 to 3 do
        let direction = vect.(i) in
        let distance = ref 1 in
        while (tab120.(tab64_square + (!distance * direction)) <> (-1)) && (tab120.(tab64_square + ((!distance + 1) * direction)) <> (-1)) do
          masks.(square) <- masks.(square) ||| single_bitboards_tab.(tab120.(tab64_square + (!distance * direction)));
          incr distance
        done
      done
    done
  in aux bishop_vect bishop_masks;
  aux rook_vect rook_masks

let init_blockers () =
  let aux masks blockers =
    for square = 0 to 63 do
      let pattern_bits = Array.of_list (lsb_list masks.(square)) in
      let n = Array.length pattern_bits in
      let total = 1 lsl n in
      let arr = Array.make total 0L in
      for i = 0 to total - 1 do
        let subset = ref 0L in
        for j = 0 to n - 1 do
          if (i land (1 lsl j)) <> 0 then
            subset := !subset ||| pattern_bits.(j)
        done;
        arr.(i) <- !subset
      done;
      blockers.(square) <- arr
    done
  in aux bishop_masks bishop_blockers;
  aux rook_masks rook_blockers

let init_sliding_moves () =
  let aux vect square blocker =
    let moves_bitboard = ref 0L in
    let tab64_square = tab64.(square) in
    for i = 0 to 3 do
      let direction = vect.(i) in
      let distance = ref 1 in
      let continue = ref true in
      while (!continue && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let to_ = tab120.(tab64_square + (!distance * direction)) in
        moves_bitboard := !moves_bitboard ||| single_bitboards_tab.(to_);
        if blocker &&& (Int64.shift_left 1L to_) <> 0L then begin
          continue := false
        end
        else begin
          incr distance
        end
      done;
    done;
    !moves_bitboard
  in for square = 0 to 63 do
    bishop_moves.(square) <- Array.make (Array.length bishop_blockers.(square)) 0L;
    rook_moves.(square)   <- Array.make (Array.length rook_blockers.(square)) 0L;
    Array.iteri (fun i blocker -> bishop_moves.(square).(i) <- aux bishop_vect square blocker) bishop_blockers.(square);
    Array.iteri (fun i blocker -> rook_moves.(square).(i) <- aux rook_vect square blocker) rook_blockers.(square);
  done

let init_magic () =
  let bishop_file = open_in "magic_bishop.txt" in
  let rook_file = open_in "magic_rook.txt" in
  let aux masks shifts magics file  =
    for square = 0 to 63 do
      shifts.(square) <- 64 - population_count masks.(square);
      magics.(square) <- Int64.of_string (input_line file)
    done
  in aux bishop_masks bishop_shifts bishop_magics bishop_file;
  aux rook_masks rook_shifts rook_magics rook_file

let [@inline] index magic blocker shift =
  Int64.to_int (Int64.shift_right_logical (Int64.mul magic blocker) shift)

let init_tables () =
  let aux masks blockers magics shifts moves table =
    for square = 0 to 63 do
      let size = 1 lsl (population_count masks.(square)) in
      table.(square) <- Array.make size (-1L);
      for i = 0 to Array.length blockers.(square) - 1 do
        let magic_index = index magics.(square) blockers.(square).(i) shifts.(square) in
        table.(square).(magic_index) <- moves.(square).(i)
      done
    done;
  in aux bishop_masks bishop_blockers bishop_magics bishop_shifts bishop_moves bishop_table;
  aux rook_masks rook_blockers rook_magics rook_shifts rook_moves rook_table

let () =
  init_pawn ();
  init_not_slidings ();
  init_castling_info ();
  init_sliding_masks ();
  init_blockers ();
  init_sliding_moves ();
  begin try
    init_magic () with _ -> print_endline "No magic bitboards file"
  end;
  init_tables ()

let [@inline] add_pawn_moves moves number_of_moves moves_bitboard from flag =
  let bitboard = ref moves_bitboard in
  while !bitboard <> 0L do
    let to_, other_moves_bitboard = pop_lsb !bitboard in
    moves.(!number_of_moves) <- encode_move from to_ flag;
    incr number_of_moves;
    bitboard := other_moves_bitboard
  done

let [@inline] add_promotion_moves moves number_of_moves moves_bitboard from capture =
  for promotion = (8 lor capture) to (11 lor capture) do
    add_pawn_moves moves number_of_moves moves_bitboard from promotion
  done

let [@inline] generate_pawn_attacks from white_to_move =
  pawn_attack_tables.(white_to_move).(from)

let [@inline] generate_knight_attacks from  =
  knight_table.(from)

let [@inline] generate_king_attacks from =
  king_table.(from)

let [@inline] generate_bishop_attacks from occupancy =
  let blocker = bishop_masks.(from) &&& occupancy in
  bishop_table.(from).(index bishop_magics.(from) blocker bishop_shifts.(from))

let [@inline] generate_rook_attacks from occupancy =
  let blocker = rook_masks.(from) &&& occupancy in
  rook_table.(from).(index rook_magics.(from) blocker rook_shifts.(from))

let [@inline] generate_queen_attacks from occupancy =
  (generate_bishop_attacks from occupancy) ||| (generate_rook_attacks from occupancy)

let ray_table = Array.init 64 (fun _ -> Array.make 64 0L)

let () =
  for from = 0 to 63 do
    for to_ = 0 to 63 do
      if from <> to_ then begin
        let bishop_intersect = generate_bishop_attacks from single_bitboards_tab.(to_) &&& generate_bishop_attacks to_ single_bitboards_tab.(from) in
        let rook_intersect = generate_rook_attacks from single_bitboards_tab.(to_) &&& generate_rook_attacks to_ single_bitboards_tab.(from) in
        if (generate_bishop_attacks from 0L &&& single_bitboards_tab.(to_) <> 0L) then begin
          ray_table.(from).(to_) <- bishop_intersect ||| single_bitboards_tab.(to_)
        end
        else if (generate_rook_attacks from 0L &&& single_bitboards_tab.(to_) <> 0L) then begin
          ray_table.(from).(to_) <- rook_intersect ||| single_bitboards_tab.(to_)
        end
      end
    done
  done

let [@inline] is_attacked square white_to_move occupancy pieces_bitboard oponent_pieces =
  (generate_pawn_attacks square white_to_move) &&& pieces_bitboard.(oponent_pieces.(pawn)) <> 0L ||
  knight_table.(square) &&& pieces_bitboard.(oponent_pieces.(knight)) <> 0L ||
  king_table.(square) &&& pieces_bitboard.(oponent_pieces.(king)) <> 0L ||
  (generate_bishop_attacks square occupancy) &&& (pieces_bitboard.(oponent_pieces.(queen)) ||| pieces_bitboard.(oponent_pieces.(bishop))) <> 0L ||
  (generate_rook_attacks square occupancy) &&& (pieces_bitboard.(oponent_pieces.(queen)) ||| pieces_bitboard.(oponent_pieces.(rook))) <> 0L

let [@inline] is_sniped square occupancy pieces_bitboard oponent_pieces =
  (generate_bishop_attacks square occupancy) &&& (pieces_bitboard.(oponent_pieces.(queen)) ||| pieces_bitboard.(oponent_pieces.(bishop))) <> 0L ||
  (generate_rook_attacks square occupancy) &&& (pieces_bitboard.(oponent_pieces.(queen)) ||| pieces_bitboard.(oponent_pieces.(rook))) <> 0L

let [@inline] generate_all_attacks pieces_bitboards oponent_pieces occupancy white_to_move =
  let all_attacks = ref 0L in
  let [@inline] aux pieces_bitboards generate_piece_attacks =
    let bitboard = ref pieces_bitboards in
    while !bitboard <> 0L do
      let from, other_pieces_bitboards = pop_lsb !bitboard in
      all_attacks := !all_attacks ||| (generate_piece_attacks from);
      bitboard := other_pieces_bitboards
    done
  in aux pieces_bitboards.(oponent_pieces.(pawn)) (fun [@inline] from -> generate_pawn_attacks from white_to_move);
  aux pieces_bitboards.(oponent_pieces.(knight)) generate_knight_attacks;
  aux pieces_bitboards.(oponent_pieces.(bishop)) (fun [@inline] from -> generate_bishop_attacks from occupancy);
  aux pieces_bitboards.(oponent_pieces.(rook)) (fun [@inline] from -> generate_rook_attacks from occupancy);
  aux pieces_bitboards.(oponent_pieces.(queen)) (fun [@inline] from -> generate_queen_attacks from occupancy);
  aux pieces_bitboards.(oponent_pieces.(king)) generate_king_attacks;
  !all_attacks

let [@inline] generate_pawn_moves pieces_bitboards white_to_move ep_square total_occupancy not_occupancy oponent_occupancy moves number_of_moves from king_square in_check check_mask pin_masks =
  let legality_mask = check_mask &&& pin_masks.(from) in
  let promotion_rank = promotion_ranks.(white_to_move) in
  let pawn_attacks = generate_pawn_attacks from white_to_move in
  let single_push = single_bitboards_tab.(from + push_vects.(white_to_move)) &&& not_occupancy in
  let double_push =
    begin
      if single_push = 0L || single_bitboards_tab.(from) &&& double_push_ranks.(white_to_move) = 0L then begin
        0L
      end
      else begin
        single_bitboards_tab.(from + 2 * push_vects.(white_to_move)) &&& not_occupancy &&& legality_mask
      end
    end
  in let captures = pawn_attacks &&& oponent_occupancy &&& legality_mask in
  let enpassant =
    if ep_square <> (-1) && not (is_sniped king_square (total_occupancy ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(ep_square - push_vects.(white_to_move)) ||| single_bitboards_tab.(ep_square)) pieces_bitboards pieces_rep.(white_to_move lxor 1)) && (not in_check || check_mask = single_bitboards_tab.(ep_square - push_vects.(white_to_move))) then begin
      pawn_attacks &&& single_bitboards_tab.(ep_square) &&& pin_masks.(from)
    end
    else begin
      0L
    end
  in let promotion_push = promotion_rank &&& single_push &&& legality_mask in
  let promotion_captures = promotion_rank &&& captures in
  if single_push &&& legality_mask <> 0L && promotion_push = 0L then begin
    moves.(!number_of_moves) <- encode_move from (lsb_index single_push) 0;
    incr number_of_moves
  end;
  if double_push <> 0L then begin
    moves.(!number_of_moves) <- encode_move from (lsb_index double_push) 1;
    incr number_of_moves
  end;
  if captures <> 0L && promotion_captures = 0L then begin
    add_pawn_moves moves number_of_moves captures from 4
  end;
  if enpassant <> 0L then begin
    add_pawn_moves moves number_of_moves enpassant from 5
  end;
  if promotion_push <> 0L then begin
    add_promotion_moves moves number_of_moves promotion_push from 0
  end;
  if promotion_captures <> 0L then begin
    add_promotion_moves moves number_of_moves promotion_captures from 4
  end

let [@inline] generate_castling_moves in_check all_attacks castling_rights white_to_move occupancy moves number_of_moves from =
  let player_castling_info = castling_infos.(white_to_move) in
  let [@inline] aux to_king castling_rights castling castling_flag empty_mask safe_mask =
    if castling_rights land castling = castling && (empty_mask &&& occupancy = 0L) && (all_attacks &&& safe_mask = 0L) then begin
      moves.(!number_of_moves) <- encode_move from to_king castling_flag;
      incr number_of_moves;
    end;
  in
  if not (in_check || from <> player_castling_info.from_king) then begin
    aux player_castling_info.to_short_king castling_rights player_castling_info.short_castling 2 player_castling_info.short_castling_empty_mask player_castling_info.short_castling_safe_mask;
    aux player_castling_info.to_long_king castling_rights player_castling_info.long_castling 3 player_castling_info.long_castling_empty_mask player_castling_info.long_castling_safe_mask
  end

let [@inline] generate_normal_moves sliding_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves from =
  let captures_bitboard = sliding_attacks &&& oponent_occupancy in
  let [@inline] aux moves_bitboard capture =
    let bitboard = ref moves_bitboard in
    while !bitboard <> 0L do
      let to_, other_moves_bitboard = pop_lsb !bitboard in
      moves.(!number_of_moves) <- encode_move from to_ capture;
      incr number_of_moves;
      bitboard := other_moves_bitboard
    done;
  in aux captures_bitboard 4;
  aux (sliding_attacks &&& (Int64.logxor not_friendly_occupancy captures_bitboard)) 0

let [@inline] zobrist_index square piece =
  square * 12 + piece - 1

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de capture en passant*)
let tab_zobrist = Array.make 781 0L

let tab_zobrist_castling = Array.make 16 0L

let legal_moves position =
  let ply = position.ply in
  let state = position.state.(position.ply) in
  let white_to_move = position.white_to_move in
  let pieces_bitboards = position.pieces in
  let player_pieces = pieces_rep.(white_to_move) in
  let oponent_pieces = pieces_rep.(white_to_move lxor 1) in
  let occupancy = position.occupancy in
  let total_occupancy = occupancy.(0) ||| occupancy.(1) in
  let king_square = lsb_index pieces_bitboards.(player_pieces.(king)) in
  let in_check = state.in_check in
  let all_attacks = generate_all_attacks pieces_bitboards oponent_pieces (total_occupancy ^^^ single_bitboards_tab.(king_square)) (white_to_move lxor 1) in
  
  let check_mask =
    if in_check then begin
    let direct_checkers = 
      ((generate_pawn_attacks king_square white_to_move) &&& pieces_bitboards.(oponent_pieces.(pawn))) |||
      (knight_table.(king_square) &&& pieces_bitboards.(oponent_pieces.(knight)))
    in let all_checkers =
      direct_checkers |||
      (generate_bishop_attacks king_square total_occupancy &&& (pieces_bitboards.(oponent_pieces.(queen)) ||| pieces_bitboards.(oponent_pieces.(bishop)))) |||
      (generate_rook_attacks king_square total_occupancy &&& (pieces_bitboards.(oponent_pieces.(queen)) ||| pieces_bitboards.(oponent_pieces.(rook))))
    in let number_of_cheks = population_count all_checkers in
      if number_of_cheks > 1 then begin
        0L
      end
      else begin
        let checker_square = lsb_index all_checkers in
        if direct_checkers <> 0L then begin
          single_bitboards_tab.(checker_square)
        end
        else begin
          ray_table.(king_square).(checker_square)
        end
      end
    end
    else begin
      0xFFFFFFFFFFFFFFFFL
    end

  in let pin_masks = Array.make 64 0xFFFFFFFFFFFFFFFFL in
  let pin_candidates = ref (
    (generate_bishop_attacks king_square 0L &&& (pieces_bitboards.(oponent_pieces.(bishop)) ||| pieces_bitboards.(oponent_pieces.(queen)))) |||
    (generate_rook_attacks king_square 0L &&& (pieces_bitboards.(oponent_pieces.(rook)) ||| pieces_bitboards.(oponent_pieces.(queen))))
    )
  in while !pin_candidates <> 0L do
    let attacker_square, other_candidates = pop_lsb !pin_candidates in
    let ray =  ray_table.(king_square).(attacker_square) in
    let blockers = ray &&& total_occupancy in
    let number_of_blockers = population_count blockers in
    if number_of_blockers = 2 then begin
      let pinned_square = lsb_index (blockers ^^^ single_bitboards_tab.(attacker_square)) in
      if occupancy.(white_to_move) &&& single_bitboards_tab.(pinned_square) <> 0L then begin
        pin_masks.(pinned_square) <- ray
      end
    end;
    pin_candidates := other_candidates
  done;
  
  let moves = position.moves.(ply) in
  let number_of_moves = ref 0 in
  let friendly_occupancy = position.occupancy.(white_to_move) in
  let oponent_occupancy = position.occupancy.(white_to_move lxor 1)  in
  let not_occupancy = Int64.lognot total_occupancy in
  let not_friendly_occupancy = Int64.lognot friendly_occupancy in
  let player_pieces = pieces_rep.(white_to_move) in
  let pieces_bitboards = position.pieces in

  (*Generate pawns moves*)
  let pawns_bitboard = ref pieces_bitboards.(player_pieces.(pawn)) in
  while !pawns_bitboard <> 0L do
    let from, other_pieces_bitboard = pop_lsb !pawns_bitboard in
    generate_pawn_moves pieces_bitboards white_to_move state.ep_square total_occupancy not_occupancy oponent_occupancy moves number_of_moves from king_square state.in_check check_mask pin_masks;
    pawns_bitboard := other_pieces_bitboard
  done;

  (*Generate castling moves*)
  generate_castling_moves state.in_check all_attacks state.castling_rights white_to_move total_occupancy moves number_of_moves (lsb_index pieces_bitboards.(player_pieces.(king)));

  (*Generate normal moves*)
  let bitboard = ref pieces_bitboards.(player_pieces.(knight)) in
  while !bitboard <> 0L do
    let from, other_pieces_bitboard = pop_lsb !bitboard in
    let piece_attacks = generate_knight_attacks from &&& check_mask &&& pin_masks.(from) in
    generate_normal_moves piece_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves from;
    bitboard := other_pieces_bitboard
  done;

  bitboard := pieces_bitboards.(player_pieces.(bishop));
  while !bitboard <> 0L do
    let from, other_pieces_bitboard = pop_lsb !bitboard in
    let piece_attacks = generate_bishop_attacks from total_occupancy &&& check_mask &&& pin_masks.(from) in
    generate_normal_moves piece_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves from;
    bitboard := other_pieces_bitboard
  done;

  bitboard := pieces_bitboards.(player_pieces.(rook));
  while !bitboard <> 0L do
    let from, other_pieces_bitboard = pop_lsb !bitboard in
    let piece_attacks = generate_rook_attacks from total_occupancy &&& check_mask &&& pin_masks.(from) in
    generate_normal_moves piece_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves from;
    bitboard := other_pieces_bitboard
  done;

  bitboard := pieces_bitboards.(player_pieces.(queen));
  while !bitboard <> 0L do
    let from, other_pieces_bitboard = pop_lsb !bitboard in
    let piece_attacks = generate_queen_attacks from total_occupancy &&& check_mask &&& pin_masks.(from) in
    generate_normal_moves piece_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves from;
    bitboard := other_pieces_bitboard
  done;

  let piece_attacks = generate_king_attacks king_square &&& (Int64.lognot all_attacks) in
  generate_normal_moves piece_attacks oponent_occupancy not_friendly_occupancy moves number_of_moves king_square;
  position.number_of_moves.(ply) <- !number_of_moves 

let make_null position =
  let state = position.state.(position.ply) in
  let new_state = position.state.(position.ply + 1) in
  let white_to_move = position.white_to_move in
  let castling_rights = state.castling_rights in
  new_state.captured_piece <- 0;
  position.white_to_move <- white_to_move lxor 1;
  new_state.castling_rights <- castling_rights;
  new_state.zobrist <- state.zobrist ^^^ tab_zobrist.(768);
  if state.ep_square <> (-1) then begin
    new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(769 + (state.ep_square land 7));
  end;
  new_state.ep_square <- (-1);
  new_state.half_moves <- state.half_moves + 1;
  position.ply <- position.ply + 1;
  new_state.in_check <- false

let unmake_null position =
  position.white_to_move <- position.white_to_move lxor 1;
  position.ply <- position.ply - 1

let make position move =
  let state = position.state.(position.ply) in
  let new_state = position.state.(position.ply + 1) in
  let mailbox = position.mailbox in
  let white_to_move = position.white_to_move in
  let pieces_bitboards = position.pieces in
  let occupancy = position.occupancy in
  let from = get_move_from move in
  let to_ = get_move_to move in
  let piece = mailbox.(from) in
  let castling_rights = state.castling_rights in
  let player_pieces = pieces_rep.(white_to_move) in
  let oponent_pieces = pieces_rep.(white_to_move lxor 1) in
  let captured_piece = mailbox.(to_) in
  new_state.captured_piece <- captured_piece;
  position.white_to_move <- white_to_move lxor 1;
  new_state.castling_rights <- castling_rights;
  new_state.zobrist <- state.zobrist ^^^ tab_zobrist.(768);
  if state.ep_square <> (-1) then begin
    new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(769 + (state.ep_square land 7));
  end;
  new_state.ep_square <- (-1);
  new_state.half_moves <- state.half_moves + 1;
  position.ply <- position.ply + 1;
  let flag = get_move_flag move in
  begin match flag with
    |0 ->
      mailbox.(from) <- 0;
      mailbox.(to_) <- piece;
      new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ piece);
      pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      let new_rights = castling_rights land castling_rights_masks.(from) in
      if castling_rights <> new_rights then begin
        new_state.castling_rights <- new_rights;
        new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist_castling.(castling_rights) ^^^ tab_zobrist_castling.(new_rights)
      end;
      if is_pawn piece then begin
        new_state.half_moves <- 0
      end;
    |4 ->
      mailbox.(from) <- 0;
      mailbox.(to_) <- piece;
      new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ piece) ^^^ tab_zobrist.(zobrist_index to_ captured_piece);
      pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      pieces_bitboards.(captured_piece) <- pieces_bitboards.(captured_piece) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(to_);
      let new_rights = castling_rights land castling_rights_masks.(from) land castling_rights_masks.(to_) in
      if castling_rights <> new_rights then begin
        new_state.castling_rights <- new_rights;
        new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist_castling.(castling_rights) ^^^ tab_zobrist_castling.(new_rights)
      end;
      new_state.half_moves <- 0
    |1 ->
      mailbox.(from) <- 0;
      mailbox.(to_) <- piece;
      if (pieces_bitboards.(oponent_pieces.(pawn)) &&& enpassant_table.(to_) <> 0L) then begin
        new_state.ep_square <- (from + to_) / 2;
        new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(769 + (from land 7))
      end;
      new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ piece);
      pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      new_state.half_moves <- 0;
    |2 ->
      let player_rook = player_pieces.(rook) in
      let player_king = player_pieces.(king) in
      let player_castling_info = castling_infos.(white_to_move) in
      let from_rook = player_castling_info.from_short_rook in
      let to_rook = player_castling_info.to_short_rook in
      mailbox.(from) <- 0;
      mailbox.(to_) <- player_king;
      mailbox.(from_rook) <- 0;
      mailbox.(to_rook) <- player_rook;
      pieces_bitboards.(player_king) <- pieces_bitboards.(player_king) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      pieces_bitboards.(player_rook) <- pieces_bitboards.(player_rook) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
      let new_rights = castling_rights land castling_rights_masks.(from) in
      new_state.castling_rights <- new_rights;
      new_state.zobrist <-
        new_state.zobrist ^^^ tab_zobrist_castling.(castling_rights) ^^^ tab_zobrist_castling.(new_rights) ^^^
        tab_zobrist.(zobrist_index from player_king) ^^^ tab_zobrist.(zobrist_index to_ player_king) ^^^
        tab_zobrist.(zobrist_index from_rook player_rook) ^^^ tab_zobrist.(zobrist_index to_rook player_rook)
    |3 ->
      let player_rook = player_pieces.(rook) in
      let player_king = player_pieces.(king) in
      let player_castling_info = castling_infos.(white_to_move) in
      let from_rook = player_castling_info.from_long_rook in
      let to_rook = player_castling_info.to_long_rook in
      mailbox.(from) <- 0;
      mailbox.(to_) <- player_king;
      mailbox.(from_rook) <- 0;
      mailbox.(to_rook) <- player_rook;
      pieces_bitboards.(player_king) <- pieces_bitboards.(player_king) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      pieces_bitboards.(player_rook) <- pieces_bitboards.(player_rook) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
      let new_rights = castling_rights land castling_rights_masks.(from) in
      new_state.castling_rights <- new_rights;
      new_state.zobrist <-
        new_state.zobrist ^^^ tab_zobrist_castling.(castling_rights) ^^^ tab_zobrist_castling.(new_rights) ^^^
        tab_zobrist.(zobrist_index from player_king) ^^^ tab_zobrist.(zobrist_index to_ player_king) ^^^
        tab_zobrist.(zobrist_index from_rook player_rook) ^^^ tab_zobrist.(zobrist_index to_rook player_rook)
    |5 ->
      mailbox.(from) <- 0;
      mailbox.(to_) <- piece;
      let captured_pawn_square = (to_ - push_vects.(white_to_move)) in
      mailbox.(captured_pawn_square) <- 0;
      let captured_pawn = oponent_pieces.(pawn) in
      new_state.captured_piece <- captured_pawn;
      mailbox.(captured_pawn_square) <- 0;
      new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ piece) ^^^ tab_zobrist.(zobrist_index captured_pawn_square captured_pawn);
      pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      pieces_bitboards.(captured_pawn) <- pieces_bitboards.(captured_pawn) ^^^ single_bitboards_tab.(captured_pawn_square);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(captured_pawn_square);
      new_state.half_moves <- 0
    |_ ->
      begin
        let promotion_piece = (flag lor 4) + - 10 +  6 * white_to_move in
        mailbox.(from) <- 0;
        mailbox.(to_) <- promotion_piece;
        pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from);
        pieces_bitboards.(promotion_piece) <- pieces_bitboards.(promotion_piece) ^^^ single_bitboards_tab.(to_);
        occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
        if flag land 4 <> 0 then begin
          new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ promotion_piece) ^^^ tab_zobrist.(zobrist_index to_ captured_piece);
          pieces_bitboards.(captured_piece) <- pieces_bitboards.(captured_piece) ^^^ single_bitboards_tab.(to_);
          occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(to_);
        end
        else begin
          new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist.(zobrist_index from piece) ^^^ tab_zobrist.(zobrist_index to_ promotion_piece)
        end;
        let new_rights = castling_rights land castling_rights_masks.(from) land castling_rights_masks.(to_) in
        if castling_rights <> new_rights then begin
          new_state.castling_rights <- new_rights;
          new_state.zobrist <- new_state.zobrist ^^^ tab_zobrist_castling.(castling_rights) ^^^ tab_zobrist_castling.(new_rights)
        end;
        new_state.half_moves <- 0
      end
  end;
  new_state.in_check <- is_attacked (lsb_index pieces_bitboards.(oponent_pieces.(king))) (white_to_move lxor 1) (occupancy.(0) ||| occupancy.(1)) pieces_bitboards player_pieces

let unmake position move =
  let mailbox = position.mailbox in
  let white_to_move = position.white_to_move lxor 1 in
  let pieces_bitboards = position.pieces in
  let occupancy = position.occupancy in
  let from = get_move_from move in
  let to_ = get_move_to move in
  let piece = mailbox.(to_) in
  position.white_to_move <- white_to_move;
  let captured_piece = position.state.(position.ply).captured_piece in
  position.ply <- position.ply - 1;
  let flag = get_move_flag move in match flag with
  |0|1 ->
    mailbox.(from) <- piece;
    mailbox.(to_) <- 0;
    pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_)
  |4 ->
    mailbox.(from) <- piece;
    mailbox.(to_) <- captured_piece;
    pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    pieces_bitboards.(captured_piece) <- pieces_bitboards.(captured_piece) ^^^ single_bitboards_tab.(to_);
    occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(to_)
  |2->
    let player_pieces = pieces_rep.(white_to_move) in
    let player_rook = player_pieces.(rook) in
    let player_king = player_pieces.(king) in
    let player_castling_info = castling_infos.(white_to_move) in
    let from_rook = player_castling_info.from_short_rook in
    let to_rook = player_castling_info.to_short_rook in
    mailbox.(from) <- piece;
    mailbox.(to_) <- 0;
    mailbox.(from_rook) <- player_rook;
    mailbox.(to_rook) <- 0;
    pieces_bitboards.(player_king) <- pieces_bitboards.(player_king) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    pieces_bitboards.(player_rook) <- pieces_bitboards.(player_rook) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
    occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook)
  |3 ->
    let player_pieces = pieces_rep.(white_to_move) in
    let player_rook = player_pieces.(rook) in
    let player_king = player_pieces.(king) in
    let player_castling_info = castling_infos.(white_to_move) in
    let from_rook = player_castling_info.from_long_rook in
    let to_rook = player_castling_info.to_long_rook in
    mailbox.(from) <- piece;
    mailbox.(to_) <- 0;
    mailbox.(from_rook) <- player_rook;
    mailbox.(to_rook) <- 0;
    pieces_bitboards.(player_king) <- pieces_bitboards.(player_king) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    pieces_bitboards.(player_rook) <- pieces_bitboards.(player_rook) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook);
    occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_) ^^^ single_bitboards_tab.(from_rook) ^^^ single_bitboards_tab.(to_rook)
  |5 ->
    mailbox.(from) <- piece;
    mailbox.(to_) <- 0;
    let captured_pawn_square = to_ - push_vects.(white_to_move) in
    mailbox.(captured_pawn_square) <- captured_piece;
    pieces_bitboards.(piece) <- pieces_bitboards.(piece) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    pieces_bitboards.(captured_piece) <- pieces_bitboards.(captured_piece) ^^^ single_bitboards_tab.(captured_pawn_square);
    occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
    occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(captured_pawn_square)
  |_ ->
    begin
      let promotion_piece = (flag lor 4) - 10 +  6 * white_to_move in
      let player_pawn = 1 + white_to_move * 6 in
      mailbox.(from) <- player_pawn;
      mailbox.(to_) <- captured_piece;
      pieces_bitboards.(player_pawn) <- pieces_bitboards.(player_pawn) ^^^ single_bitboards_tab.(from);
      pieces_bitboards.(promotion_piece) <- pieces_bitboards.(promotion_piece) ^^^ single_bitboards_tab.(to_);
      occupancy.(white_to_move) <- occupancy.(white_to_move) ^^^ single_bitboards_tab.(from) ^^^ single_bitboards_tab.(to_);
      if flag land 4 <> 0 then begin
        pieces_bitboards.(captured_piece) <- pieces_bitboards.(captured_piece) ^^^ single_bitboards_tab.(to_);
        occupancy.(white_to_move lxor 1) <- occupancy.(white_to_move lxor 1) ^^^ single_bitboards_tab.(to_);
      end
    end

    (*Array used in print_board*)
let tab_print = [|"   |"; " P |"; " N |"; " B |"; " R |"; " Q |"; " K |"; " p |"; " n |"; " b |"; " r |"; " q |"; " k |"|]

let mailbox_of_bitboard pieces_bitboards =
  let mailbox = Array.make 64 0 in
  let rec aux index piece = match index with
    |[] -> ()
    |h::t ->
      mailbox.(h) <- piece;
      aux t piece
  in for i = 1 to 12 do
    aux (index_list pieces_bitboards.(i)) i
  done;
  mailbox

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

let zouk mailbox st =
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  begin
    let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt"
    in output_string fichier_sortie (st ^ "\n" ^ !display ^ "     a   b   c   d   e   f   g   h\n");
    close_out fichier_sortie
  end

let print_board mailbox =
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")


let print_bitboard bitboard =
  let mailbox = Array.make 64 0 in
  let rec aux_1 mailbox index = match index with
    |[] -> ()
    |h::t ->
      mailbox.(h) <- 6;
      aux_1 mailbox t
  in aux_1 mailbox (index_list bitboard);
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt"
  in output_string fichier_sortie (!display ^ "     a   b   c   d   e   f   g   h\n");
  close_out fichier_sortie
end;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")