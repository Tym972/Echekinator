(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Generator
open Move_ordering
open Transposition
open Evaluation

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition stack ply =
  let index = ref (ply - 2) in
  let zobrist_position = stack.(ply).zobrist_position in
  let repeat = ref false in
  let limit = max (ply - stack.(ply).half_moves) (- !initial_half_moves) in
  while !index >= limit && not !repeat do
    if !index >= 0 then begin
      if stack.(!index).zobrist_position = zobrist_position then begin
        repeat := true
      end
    end
    else begin
      if board_record.(!initial_half_moves + !index) = zobrist_position then begin
        repeat := true
      end
    end;
    index := !index - 2;
  done;
  !repeat

(*open Evaluation*)

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search stack thread depth ply alpha beta ispv =

  (*Check search limit*)
  if stop_search.(thread) then begin
    0
  end

  else begin
    let position = stack.(ply) in
    let king_position = index_array position.board (king position.white_to_move) in
    let in_check = threatened position.board king_position in

    (*Check repetion or fifty moves rule*)
    if repetition stack ply || (position.half_moves = 100 && (not in_check || (let _, number_of_moves = legal_moves position king_position in_check in !number_of_moves <> 0))) then begin
      0
    end

    else begin
      let best_move = ref Null in
      let hash_depth, hash_lower_bound, hash_upper_bound, hash_move(*, hash_static_eval*) = probe position in
      let no_cut = ref true in
      let best_score = ref (- max_int) in
      let alpha0 = ref alpha in
      let beta0 = ref beta in

      (*Use TT informations*)
      if not (ispv || depth > hash_depth) then begin
        hash_treatment hash_lower_bound hash_upper_bound alpha0 beta0 best_score no_cut ply
      end;
      if !no_cut then begin

        (*Static eval*)
        if not in_check then begin
          best_score := hce position;
        end;

        (*Stand pat verification then move loop*)
        if !best_score < beta then begin
          
          if !best_score > !alpha0 then begin
            alpha0 := !best_score
          end;

          let counter = ref 0 in
          let move_loop move =
            make position stack.(ply + 1) move;
            let score = - quiescence_search stack thread (depth - 1) (ply + 1) (- !beta0) (- !alpha0) ispv
            in if score > !best_score then begin
              best_score := score;
              if score > !alpha0 then begin
                best_move := move
              end;
              if score > !alpha0 then begin
                alpha0 := score
              end;
              if score >= !beta0 then begin
                no_cut := false
              end
            end;
            unmake position.board move stack.(ply+ 1).last_capture;
            incr counter

          (*If in check search for all moves*)
          in if in_check then begin
            let move_loop_in_check () =
              let legal_moves, number_of_legal_moves = legal_moves position king_position in_check in
              let i = ref 0 in
              while !no_cut && !i < !number_of_legal_moves do
                move_loop legal_moves.(!i);
                incr i
              done
            in if hash_move <> Null then begin
              move_loop hash_move;
              if !no_cut then begin
                move_loop_in_check ()
              end
            end
            else begin
              move_loop_in_check ()
            end;

            (*Check for mate*)
            if !best_score = (- max_int) then begin
              best_score := ply - 99999
            end

          end

          (*Else only search for captures and promotions*)
          else begin
            let move_loop_normal () =
              let captures = ref (tri_see (captures position) position.board hash_move) in
              while !no_cut && !captures <> [] do
                move_loop (List.hd !captures);
                captures := List.tl !captures
              done
            in if hash_move <> Null && not (isquiet hash_move position.board.(to_ hash_move)) then begin
              move_loop hash_move;
              if !no_cut then begin
                move_loop_normal ()
              end
            end
            else begin
              move_loop_normal ()
            end
          end

        end
      end;

      (*Storing in TT*)
      if not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
        let lower_bound = ref (- max_int) in
        let upper_bound = ref max_int in
        let stored_value =
          if is_win !best_score then begin
            !best_score + ply
          end
          else if is_loss !best_score then begin
            !best_score - ply
          end
          else begin
            !best_score
          end
        in if !best_score <= alpha then begin
          upper_bound := stored_value
        end
        else if !best_score >= beta then begin
          lower_bound := stored_value
        end
        else begin
          lower_bound := stored_value;
          upper_bound := stored_value
        end;
        store thread position.zobrist_position depth !lower_bound !upper_bound !best_move (*hash_static_eval*) !go_counter
      end;
    !best_score
    end
  end