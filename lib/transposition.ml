open Board
open Bigarray
open Bitboards

type tt = {
  key : (int64, int64_elt, c_layout) Array1.t;
  depth : (int, int_elt, c_layout) Array1.t;
  lower_bound : (int, int_elt, c_layout) Array1.t;
  upper_bound  : (int, int_elt, c_layout) Array1.t;
  move : (int, int_elt, c_layout) Array1.t;
  static_eval : (int, int_elt, c_layout) Array1.t;
  generation : (int, int_elt, c_layout) Array1.t
}

let empty_depth = (-40)

let entry_size = 7 * 8

(*Variables for Hash size in MB*)
let hash_size = ref 16
let min_hash_size = 1
let max_hash_size = 33554432

let slots = ref (Int64.of_int ((!hash_size * 1024 * 1024) / entry_size))

let clear_entry tt i =
  tt.key.{i} <- 0L;
  tt.depth.{i} <- empty_depth;
  tt.lower_bound.{i} <- (-max_int);
  tt.upper_bound.{i} <- max_int;
  tt.move.{i} <- 0;
  tt.static_eval.{i} <- (-max_int);
  tt.generation.{i} <- 0

let clear tt =
  for i = 0 to Int64.to_int !slots - 1 do
    clear_entry tt i
  done

let create_tt size =
  let tt =
  {
    key  = Array1.create Int64 C_layout size;
    depth = Array1.create Int C_layout size;
    lower_bound = Array1.create Int C_layout size;
    upper_bound = Array1.create Int C_layout size;
    move  = Array1.create Int C_layout size;
    static_eval = Array1.create Int C_layout size;
    generation   = Array1.create Int C_layout size;
  }
  in clear tt;
  tt 

let tt = ref (create_tt (Int64.to_int !slots))

let is_loss score = score < (-90000)

let is_win score = score > 90000

let is_decisive score = abs score > 90000

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let hash_treatment hash_lower_bound hash_upper_bound alpha beta best_score static_eval no_cut search_ply =
  let adjust_value bound =
    if abs bound = max_int || not (is_decisive bound) then begin
      bound
    end
    else if is_win bound then begin
      bound - search_ply
    end
    else begin
      bound + search_ply
    end
  in let adjusted_low = adjust_value hash_lower_bound in
  let adjusted_up = adjust_value hash_upper_bound in
  if !beta > adjusted_up then
    beta := adjusted_up;
  if !alpha < adjusted_low then
    alpha := adjusted_low;
  if !alpha >= !beta then begin
    best_score := !alpha;
    no_cut := false
  end
  else if not (is_decisive hash_lower_bound) && hash_lower_bound >= !static_eval then begin
    static_eval := hash_lower_bound
  end
  else if not (is_decisive hash_upper_bound) && hash_upper_bound <= !static_eval then begin
    static_eval := hash_upper_bound
  end

let score_node lower_bound upper_bound =
  if lower_bound = upper_bound then
    3
  else if lower_bound > - max_int then begin
    if upper_bound < max_int then
      2
    else
      1
  end
  else
    0

let store thread key depth lower_bound upper_bound move static_eval generation =
  let index = Int64.to_int (Int64.rem key !slots) in
  let old_key = !tt.key.{index} in
  let old_depth = !tt.depth.{index}  in
  let old_lower_bound  = !tt.lower_bound.{index}  in
  let old_upper_bound = !tt.upper_bound.{index}  in
  let old_best_move  = !tt.move.{index}  in
  let old_generation = !tt.generation.{index}  in
  if old_depth = empty_depth then begin
    !tt.depth.{index} <- depth;
    !tt.lower_bound.{index} <- lower_bound;
    !tt.upper_bound.{index} <- upper_bound;
    !tt.move.{index} <- move;
    !tt.static_eval.{index} <- static_eval;
    !tt.generation.{index} <- generation;
    !tt.key.{index} <- key;
    transposition_counter.(thread) <- transposition_counter.(thread) + 1
  end
  else if (!go_counter - old_generation > 5) || (depth > old_depth) || (depth = old_depth && (key = old_key || score_node lower_bound upper_bound > score_node old_lower_bound old_upper_bound)) then begin
    let stored_lower_bound = ref lower_bound in
    let stored_upper_bound = ref upper_bound in
    let stored_move = ref move in
    if key = old_key then begin
      if move = 0 then begin
        stored_move := old_best_move
      end;
      if depth = old_depth then begin
        stored_lower_bound := max lower_bound old_lower_bound;
        stored_upper_bound := min upper_bound old_upper_bound
      end
    end;
    !tt.depth.{index} <- depth;
    !tt.lower_bound.{index} <- !stored_lower_bound;
    !tt.upper_bound.{index} <- !stored_upper_bound;
    !tt.move.{index} <- !stored_move;
    !tt.static_eval.{index} <- static_eval;
    !tt.generation.{index} <- generation;
    !tt.key.{index} <- key
  end
  else if old_best_move = 0 && move <> 0 && key = old_key then begin
    !tt.depth.{index} <- old_depth;
    !tt.lower_bound.{index} <- old_lower_bound;
    !tt.upper_bound.{index} <- old_upper_bound;
    !tt.move.{index} <- move;
    !tt.static_eval.{index} <- static_eval;
    !tt.generation.{index} <- generation;
    !tt.key.{index} <- key
  end

let probe state =
  let zobrist = state.zobrist in
  let index = Int64.to_int (Int64.rem zobrist !slots) in
  let old_key = !tt.key.{index}  in
  let old_best_move = !tt.move.{index}  in
  if zobrist = old_key && not state.excluded then begin
    !tt.depth.{index}, !tt.lower_bound.{index}, !tt.upper_bound.{index}, old_best_move, !tt.static_eval.{index} 
  end
  else begin
    empty_depth, - max_int, max_int, 0, (- max_int)
  end