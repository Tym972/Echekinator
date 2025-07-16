open Board

let transposition_size = 10000000

module ZobristHash =
  struct
    type t = int
    let equal i j = i = j
    let hash i = i mod transposition_size
  end

module ZobristHashtbl = Hashtbl.Make(ZobristHash)

type node =
  |Pv
  |Cut
  |All

let (table : (node * int * int * move * int) ZobristHashtbl.t) =  ZobristHashtbl.create transposition_size

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let hash_treatment (hash_node_type : node) (hash_depth : int) (hash_value : int) (hash_move : move) depth alpha beta best_score best_move no_tt_cut ply =
  if depth <= hash_depth then begin
    let score = ref hash_value in
    if abs hash_value > 99000 then begin
      if !score >= 0 then score := !score - ply else score := !score + ply
    end;
    match hash_node_type with
      |Pv ->
        best_score := !score;
        best_move := hash_move;
        no_tt_cut := false
      |Cut ->
        alpha := max !alpha !score;
        if !score >= !beta then begin
          best_score := !score;
          no_tt_cut := false
        end
      |All ->
        beta := min !beta !score;
        if !alpha >= !score then begin
          best_score := !score;
          no_tt_cut := false
        end 
  end

(*Fonction retirant les entrées de la hash table datant d'avant le n-ième coup.*)
let update table n =
  ZobristHashtbl.iter (fun key value -> let _, _, _, _, coup = value in if coup < n then begin ZobristHashtbl.remove table key; transposition_counter:= !transposition_counter - 1 end) table;
