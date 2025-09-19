open Board

type node =
  |Pv
  |Cut
  |All

type entry = int * node * int * int * move * int

let transposition_size = 10000000

let transposition_table = Array.make transposition_size (0, All, (-1), 0, Null, 0)

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let hash_treatment (hash_node_type : node) (hash_depth : int) (hash_value : int) (hash_move : move) depth alpha beta best_score best_move no_cut ply =
  if depth <= hash_depth then begin
    let adjusted_value =
      if abs hash_value > 99000 then begin
        if hash_value >= 0 then
          hash_value - ply
        else
          hash_value + ply
      end
      else begin
        hash_value
      end
    in match hash_node_type with
      |Pv ->
        best_score := adjusted_value;
        best_move := hash_move;
        no_cut := false
      |Cut ->
        alpha := max !alpha adjusted_value;
        if adjusted_value >= !beta then begin
          best_score := adjusted_value;
          no_cut := false
        end
      |All ->
        beta := min !beta adjusted_value;
        if !alpha >= adjusted_value then begin
          best_score := adjusted_value;
          no_cut := false
        end 
  end

(*Fonction vidant la TT*)
let clear table =
  for i = 0 to transposition_size - 1 do
    table.(i) <- (0, All, (-1), 0, Null, 0)
  done;
  transposition_counter := 0

let score_node node_type = match node_type with
  |Pv -> 10
  |Cut -> 5
  |All -> 0

let store tt key node_type depth value move generation =
  let index = key mod transposition_size in
  let old_key, old_node_type, old_depth, old_value, old_best_move, old_generation = tt.(index) in
  if old_depth = (-1) then begin
    tt.(index) <- (key, node_type, depth, value, move, generation);
    incr transposition_counter
  end
  else if (!go_counter - old_generation > 5) || (depth > old_depth) || (depth = old_depth && score_node node_type > score_node old_node_type) then begin
    let stored_move =
      if move = Null && key = old_key then
        old_best_move
      else
        move
    in tt.(index) <- (key, node_type, depth, value, stored_move, generation)
  end
  else if old_best_move = Null && move <> Null && key = old_key then begin
    tt.(index) <- (old_key, old_node_type, old_depth, old_value, move, generation)
  end

let probe tt key =
  let index = key mod transposition_size in
  let old_key, old_node_type, old_depth, old_value, old_best_move, _ = tt.(index) in
  if key = old_key then
    old_node_type, old_depth, old_value, old_best_move
  else
    (All, (-1), 0, Null)