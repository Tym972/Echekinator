open Board

(*let encode move = match move with
  |Castling {sort} -> 0
  |Enpassant  {from; to_} -> 0
  |Normal {piece; from; to_} -> 0
  |Promotion {from; to_; promotion} -> 0
  |Null -> 0

let decode intmove =

*)

type entry = int * int * int * int * move * int

let empty_depth = (-40)

let empty_entry = (0, empty_depth, - max_int, max_int, Null(*, (-infinity)*), 0)

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
  let old_key, old_depth, old_lower_bound, old_upper_bound, old_best_move(*, old_static_eval*), old_generation = !transposition_table.(index) in
  if old_depth = empty_depth then begin
    !transposition_table.(index) <- (key, depth, lower_bound, upper_bound, move(*, static_eval*), generation);
    transposition_counter.(thread) <- transposition_counter.(thread) + 1
  end
  else if (!go_counter - old_generation > 5) || (depth > old_depth) || (depth = old_depth && (key = old_key || score_node lower_bound upper_bound > score_node old_lower_bound old_upper_bound)) then begin
    let stored_lower_bound = ref lower_bound in
    let stored_upper_bound = ref upper_bound in
    let stored_move = ref move in
    if key = old_key then begin
      if move = Null then begin
        stored_move := old_best_move
      end;
      if depth = old_depth then begin
        stored_lower_bound := max lower_bound old_lower_bound;
        stored_upper_bound := min upper_bound old_upper_bound
      end
    end;
    !transposition_table.(index) <- (key, depth, !stored_lower_bound, !stored_upper_bound, !stored_move(*, static_eval*), generation)
  end
  else if old_best_move = Null && move <> Null && key = old_key then begin
    !transposition_table.(index) <- (old_key, old_depth, old_lower_bound, old_upper_bound, move(*, old_static_eval*), generation)
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
  if position.zobrist_position = old_key && verif position.board old_best_move then
    old_depth, old_lower_bound, old_upper_bound, old_best_move(*, old_static_eval*)
  else
    (empty_depth, - max_int, max_int, Null(*, (-infinity)*))