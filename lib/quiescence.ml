(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Generator
open Move_ordering

let rec detecte_extension liste_coups = match liste_coups with
  |[] -> []
  |Normal {piece; from; to_; capture} :: t when capture <> 0 -> Normal {piece; from; to_; capture} :: detecte_extension t
  |Promotion x :: t -> Promotion x :: detecte_extension t
  |Enpassant x :: t -> Enpassant x :: detecte_extension t
  |_ :: t -> detecte_extension t

let rec adapte_delta liste_coups = match liste_coups with
  |[] -> 0
  |Promotion _ :: _  -> 8000
  |_ :: t -> adapte_delta t

let compteur_quiescent = ref 0

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search board white_to_move alpha beta evaluation cap depth king_position in_check = incr compteur_quiescent;
  let delta = evaluation board white_to_move king_position in_check alpha beta in
  let best_score = ref delta in
  if depth = 0 then begin
    best_score := delta
  end
  else if delta >= beta then begin
    best_score := beta
  end
  else if delta + 9100 + adapte_delta cap < alpha then begin
    best_score := alpha
  end
  else begin
    let cps = ref (tri_see cap board white_to_move) in
    let b = ref true in
    let alpha0 = ref (max delta alpha) in
    while (!b && !cps <> []) do
      let coup = List.hd !cps in
      make board coup;
      cps := List.tl !cps;
      let new_to_move = not white_to_move in
      let opponent_king_position = index_array board (king new_to_move) in
      let score = - quiescence_search board (not white_to_move) (- beta) (- !alpha0) evaluation (captures board new_to_move coup) (depth - 1) opponent_king_position (threatened board opponent_king_position new_to_move) 
      in if score > !best_score then begin
        best_score := score;
        if score >= beta then begin
          b := false
        end
        else begin
          alpha0 := max !alpha0 score
        end
      end;
      unmake board coup
    done
  end;
  !best_score

(*Fonction permettant d'évaluer un board à la depth 0*)
let quiescence_treatment_depth_0 ply evaluation board white_to_move last_move alpha beta king_position in_check =
  let legal_moves = legal_moves board white_to_move last_move (false, false, false, false) king_position in_check
  in if legal_moves = [] then begin
    if in_check then begin
      (ply - 99999)
    end
    else begin
      0
    end
  end
  else begin
    let cap = detecte_extension legal_moves in
    let in_check = threatened board king_position white_to_move in
    if cap = [] then begin
      quiescence_search board white_to_move alpha beta evaluation cap 0 king_position in_check
    end
    else begin
      quiescence_search board white_to_move alpha beta evaluation cap (-1) king_position in_check
    end
  end