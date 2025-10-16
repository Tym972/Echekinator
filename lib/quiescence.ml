(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Generator
open Move_ordering

let detecte_extension moves number_of_moves =
  let list = ref [] in
  for i = 0 to number_of_moves - 1 do
    if not (isquiet moves.(i)) then begin
      list := moves.(i) :: !list
    end
  done;
  !list
  
let rec adapte_delta liste_coups = match liste_coups with
  |[] -> 0
  |Promotion _ :: _  -> 8000
  |_ :: t -> adapte_delta t

let compteur_quiescent = ref 0

(*open Evaluation*)

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search board white_to_move alpha beta evaluation cap depth =
  incr compteur_quiescent;
  let delta = evaluation board white_to_move in
  (*let _ = evaluate () in*)
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
      let move = List.hd !cps in
      make board move;
      cps := List.tl !cps;
      let new_to_move = not white_to_move in
      let score = - quiescence_search board (not white_to_move) (- beta) (- !alpha0) evaluation (captures board new_to_move move) (depth - 1)
      in if score > !best_score then begin
        best_score := score;
        if score >= beta then begin
          b := false
        end
        else begin
          alpha0 := max !alpha0 score
        end
      end;
      unmake board move
    done
  end;
  !best_score

(*Fonction permettant d'évaluer un board à la depth 0*)
let quiescence_treatment_depth_0 ply evaluation board white_to_move last_move castling_rights half_moves alpha beta king_position in_check =
  let legal_moves, number_of_legal_moves = legal_moves board white_to_move last_move castling_rights king_position in_check
  in if !number_of_legal_moves = 0 then begin
    if in_check then begin
      (ply - 99999)
    end
    else begin
      0
    end
  end
  else if half_moves = 100 then begin
    0
  end
  else begin
    let cap = detecte_extension legal_moves !number_of_legal_moves in
    if cap = [] then begin
      quiescence_search board white_to_move alpha beta evaluation cap 0
    end
    else begin
      quiescence_search board white_to_move alpha beta evaluation cap (-1)
    end
  end