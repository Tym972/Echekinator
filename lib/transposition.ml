open Board

type node =
  |Pv
  |Cut
  |All

(*let encode move = match move with
  |Castling {sort} -> 0
  |Enpassant  {from; to_} -> 0
  |Normal {piece; from; to_} -> 0
  |Promotion {from; to_; promotion} -> 0
  |Null -> 0

let decode intmove =

*)

type entry = int * node * int * int * move * int

let empty_depth = (-300)

let empty_entry = (0, All, empty_depth, 0, Null(*, (-infinity)*), 0)

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

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let hash_treatment (hash_node_type : node) (hash_value : int) alpha beta best_score no_cut ply =
  let adjusted_value =
    if is_win hash_value then begin
      hash_value - ply
    end
    else if is_loss hash_value then begin
      hash_value + ply
    end
    else begin
      hash_value
    end
  in match hash_node_type with
    |Pv ->
      best_score := adjusted_value;
      no_cut := false
    |Cut ->
      if adjusted_value >= !beta then begin
        best_score := adjusted_value;
        no_cut := false
      end
    |All ->
      if !alpha >= adjusted_value then begin
        best_score := adjusted_value;
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

let score_node node_type = match node_type with
  |Pv -> 10
  |Cut -> 5
  |All -> 0

let store thread key node_type depth value move (*static_eval*) generation =
  let index = key mod !slots in
  let old_key, old_node_type, old_depth, old_value, old_best_move(*, old_static_eval*), old_generation = !transposition_table.(index) in
  if old_depth = empty_depth then begin
    !transposition_table.(index) <- (key, node_type, depth, value, move(*, static_eval*), generation);
    transposition_counter.(thread) <- transposition_counter.(thread) + 1
  end
  else if (!go_counter - old_generation > 5) || (depth > old_depth) || (depth = old_depth && score_node node_type > score_node old_node_type) then begin
    let stored_move =
      if move = Null && key = old_key then
        old_best_move
      else
        move
    in !transposition_table.(index) <- (key, node_type, depth, value, stored_move(*, static_eval*), generation)
  end
  else if old_best_move = Null && move <> Null && key = old_key then begin
    !transposition_table.(index) <- (old_key, old_node_type, old_depth, old_value, move(*, old_static_eval*), generation)
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

let probe tt (position : position) =
  let index = position.zobrist_position mod !slots in
  let old_key, old_node_type, old_depth, old_value, old_best_move(*, old_static_eval*), _ = tt.(index) in
  if position.zobrist_position = old_key && verif position.board old_best_move then
    old_node_type, old_depth, old_value, old_best_move(*, old_static_eval*)
  else
    (All, empty_depth, 0, Null(*, (-infinity)*))