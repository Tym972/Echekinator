(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Generator
open Move_ordering
open Transposition
open Zobrist

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
let rec quiescence_search depth ply alpha beta evaluation ispv =

  (*Check search limit*)
  if !out_of_time || !node_counter >= !node_limit then begin
    0
  end

  else begin
    let white_to_move, last_move, castling_rights, board_record, half_moves, zobrist_position = position_aspects.(ply) in
    let king_position = index_array board (king white_to_move) in
    let in_check = threatened board king_position white_to_move in

    (*Check repetion or fifty moves rule*)
    if repetition board_record 3 || (half_moves = 100 && (not in_check || (let _, number_of_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in !number_of_moves <> 0))) then begin
      0
    end

    else begin
      let best_move = ref Null in
      let hash_node_type, hash_depth, hash_value, hash_move(*, hash_static_eval*) = probe transposition_table zobrist_position in
      let no_cut = ref true in
      let best_score = ref (-infinity) in
      let alpha0 = ref alpha in
      let beta0 = ref beta in
      (*Use TT informations*)
      if hash_depth <> empty_depth && not ispv then begin
        hash_treatment hash_node_type hash_depth hash_value hash_move depth alpha0 beta0 best_score best_move no_cut ply
      end;
      if !no_cut then begin

        (*Static eval*)
        best_score := evaluation board white_to_move;

        (*Stand pat*)
        if !best_score >= beta then begin
          no_cut := false
        end

        else begin
          if !best_score > !alpha0 then begin
            alpha0 := !best_score
          end;

          let counter = ref 0 in
          let move_loop move =
            let new_castling_right = castling_modification move castling_rights in
            let new_zobrist = new_zobrist move last_move zobrist_position castling_rights new_castling_right board in
            let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
            make board move;
            position_aspects.(ply + 1) <- (not white_to_move, move, new_castling_right, new_record, new_half_moves, new_zobrist);
            let score = - quiescence_search (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation ispv
            in if score > !best_score then begin
              best_score := score;
              if score > !alpha0 then begin
                best_move := move;
              end;
              if score > !alpha0 then begin
                alpha0 := score;
              end;
              if score >= !beta0 then begin
                no_cut := false;
              end
            end;
            unmake board move;
            incr counter
          in if hash_move <> Null && not (isquiet hash_move) then begin
            move_loop hash_move;
            if !no_cut then begin
              (*let legal_moves, number_of_legal_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in
              let ordering_array = Array.make !number_of_legal_moves 0 in
              move_ordering board white_to_move legal_moves number_of_legal_moves ply hash_move ordering_array;
              while !no_cut && !number_of_legal_moves > 0 do
                move_loop (move_picker legal_moves ordering_array number_of_legal_moves)
              done;*)
              let captures = ref (tri_see (captures board white_to_move last_move) board white_to_move hash_move) in
              while !no_cut && !captures <> [] do
                move_loop (List.hd !captures);
                captures := List.tl !captures
              done
            end
          end
          else begin
            (*let legal_moves, number_of_legal_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in
            let ordering_array = Array.make !number_of_legal_moves 0 in
            move_ordering board white_to_move legal_moves number_of_legal_moves ply Null ordering_array;
            while !no_cut && !number_of_legal_moves > 0 do
              move_loop (move_picker legal_moves ordering_array number_of_legal_moves)
            done;*)
            let captures = ref (tri_see (captures board white_to_move last_move) board white_to_move hash_move) in
            while !no_cut && !captures <> [] do
              move_loop (List.hd !captures);
              captures := List.tl !captures
            done
          end;

        end
      end;

      (*Storing in TT*)
      if not (!out_of_time || !node_counter >= !node_limit) then begin
        let node_type =
          if !best_score <= alpha then begin
            All
          end
          else if !best_score >= beta then begin
            Cut
          end
          else begin
            Pv
          end
          in let stored_value =
            if abs !best_score < 99000 then begin
              !best_score
            end
            else begin
              if !best_score >= 0 then begin
                !best_score + ply
              end
              else begin
                !best_score - ply
              end
            end
          in store transposition_table zobrist_position node_type depth stored_value !best_move (*hash_static_eval*) !go_counter
        end;
    !best_score
    end
  end