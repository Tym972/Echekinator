open Board

let encode move = match move with
  |Castling {sort} -> sort
  |Enpassant  {from; to_} -> 49152 + 64 * from + to_
  |Normal {piece; from; to_} ->
    let adjusted_piece = if piece > 0 then piece - 1 else 5 - piece in
    49152 * 2 + 4096 * adjusted_piece + 64 * from + to_
  |Promotion {from; to_; promotion} ->
    let adjusted_promotion = if promotion > 0 then promotion - 1 else 5 - promotion in
    49152 * 3 + 4096 * adjusted_promotion + 64 * from + to_
  |Null -> 0

let decode intmove =
  if intmove = 0 then begin
    Null
  end
  else begin
    let move_type = intmove / 49152 in
    if move_type = 0 then begin
      Castling {sort = intmove mod 49152}
    end
    else if move_type = 1 then begin
      let k = intmove - 49152 in
      Enpassant  {from = k / 64; to_ = k mod 64}
    end
    else if move_type = 2 then begin
      let k = intmove - 2 * 49152 in
      let j = k / 4096 in
      let l = k mod 4096 in
      let adjusted_piece = if j < 6 then j + 1 else 5 - j in
      Normal {piece = adjusted_piece; from = l/64; to_ = l mod 64 }
    end
    else begin
      let k = intmove - 3 * 49152 in
      let j = k / 4096 in
      let l = k mod 4096 in
      let adjusted_promotion = if j < 6 then j + 1 else 5 - j in
      Promotion {from = l / 64; to_ = l mod 64; promotion = adjusted_promotion}
    end
  end

type entry = int * int * int * int * int * int

let empty_depth = (-40)

let (empty_entry : entry) = (0, empty_depth, - max_int, max_int, 0(*, (-infinity)*), 0)

let entry_size =
  let size_in_words x = Obj.size (Obj.repr x) in
  size_in_words empty_entry * Sys.word_size / 8

(*Variables for Hash size in MB*)
let hash_size = ref 16
let min_hash_size = 1
let max_hash_size = 33554432

let slots = ref ((!hash_size * 1024 * 1024) / entry_size)

let transposition_table = ref (Array.make !slots empty_entry)

let is_loss score = score < (-90000)

let is_win score = score > 90000

let is_decisive score = abs score > 90000

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let hash_treatment hash_lower_bound hash_upper_bound alpha beta best_score no_cut ply =
  let adjust_value bound =
    if abs bound = max_int || not (is_decisive bound) then begin
      bound
    end
    else if is_win bound then begin
      bound - ply
    end
    else begin
      bound + ply
    end
  in let adjusted_low = adjust_value hash_lower_bound in
  let adjusted_up = adjust_value hash_upper_bound in
  beta := min !beta adjusted_up;
  alpha := max !alpha adjusted_low;
  if !alpha >= !beta then begin
    best_score := !alpha;
    no_cut := false
  end

(*Fonction vidant la TT*)
let clear () =
  for i = 0 to !slots - 1 do
    !transposition_table.(i) <- empty_entry
  done;
  for i = 0 to !threads_number do
    transposition_counter.(i) <- 0
  done

let score_node lower_bound upper_bound =
  if lower_bound = upper_bound then 3
  else if lower_bound > - max_int then begin
    if upper_bound < max_int then 2
    else 1
  end
  else 0

let store thread key depth lower_bound upper_bound move (*static_eval*) generation =
  let index = key mod !slots in
  let encoded_move = encode move in
  let old_key, old_depth, old_lower_bound, old_upper_bound, old_best_move(*, old_static_eval*), old_generation = !transposition_table.(index) in
  if old_depth = empty_depth then begin
    !transposition_table.(index) <- (key, depth, lower_bound, upper_bound, encoded_move(*, static_eval*), generation);
    transposition_counter.(thread) <- transposition_counter.(thread) + 1
  end
  else if (!go_counter - old_generation > 5) || (depth > old_depth) || (depth = old_depth && (key = old_key || score_node lower_bound upper_bound > score_node old_lower_bound old_upper_bound)) then begin
    let stored_lower_bound = ref lower_bound in
    let stored_upper_bound = ref upper_bound in
    let stored_move = ref encoded_move in
    if key = old_key then begin
      if encoded_move = 0 then begin
        stored_move := old_best_move
      end;
      if depth = old_depth then begin
        stored_lower_bound := max lower_bound old_lower_bound;
        stored_upper_bound := min upper_bound old_upper_bound
      end
    end;
    !transposition_table.(index) <- (key, depth, !stored_lower_bound, !stored_upper_bound, !stored_move(*, static_eval*), generation)
  end
  else if old_best_move = 0 && encoded_move <> 0 && key = old_key then begin
    !transposition_table.(index) <- (old_key, old_depth, old_lower_bound, old_upper_bound, encoded_move(*, old_static_eval*), generation)
  end

let verif board move = match move with
  |Normal {piece; from; to_} -> board.(from) = piece && board.(to_) * piece <= 0
  |Enpassant {from; to_} -> board.(from) = (if from < 32 then 1 else -1) && board.(to_) = 0
  |Castling {sort} ->
    if sort < 3 then
      board.(!from_white_king) = 6
    else
      board.(!from_black_king) = (-6)
  |Promotion {from; to_; promotion} -> board.(from) = (if promotion > 0 then 1 else -1) && board.(to_) * promotion <= 0
  |Null -> true

let probe position =
  let index = position.zobrist_position mod !slots in
  let old_key, old_depth, old_lower_bound, old_upper_bound, old_best_move(*, old_static_eval*), _ = !transposition_table.(index) in
  let decoded_move = decode old_best_move in
  if position.zobrist_position = old_key && verif position.board decoded_move then
    old_depth, old_lower_bound, old_upper_bound, decoded_move(*, old_static_eval*)
  else
    (empty_depth, - max_int, max_int, Null(*, (-infinity)*))