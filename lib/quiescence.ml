(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Generator
open Move_ordering
open Transposition
open Zobrist
open Evaluation

(*open Evaluation*)

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search position thread depth ply alpha beta ispv =

  (*Check search limit*)
  if stop_search.(thread) then begin
    0
  end

  else begin
    let king_position = index_array position.board (king position.white_to_move) in
    let in_check = threatened position.board king_position position.white_to_move in

    (*Check repetion or fifty moves rule*)
    if repetition position.board_record 3 || (position.half_moves = 100 && (not in_check || (let _, number_of_moves = legal_moves position.board position.white_to_move position.last_move position.castling_rights king_position in_check in !number_of_moves <> 0))) then begin
      0
    end

    else begin
      let best_move = ref Null in
      let hash_node_type, hash_depth, hash_value, hash_move(*, hash_static_eval*) = probe !transposition_table position.zobrist_position position.board in
      let no_cut = ref true in
      let best_score = ref (- max_int) in
      let alpha0 = ref alpha in
      let beta0 = ref beta in

      (*Use TT informations*)
      if not (ispv || depth > hash_depth) then begin
        hash_treatment hash_node_type hash_value alpha0 beta0 best_score no_cut ply
      end;
      if !no_cut then begin

        (*Static eval*)
        if not in_check then begin
          best_score := hce position.board position.white_to_move;
        end;

        (*Stand pat verification then move loop*)
        if !best_score < beta then begin
          
          if !best_score > !alpha0 then begin
            alpha0 := !best_score
          end;

          let counter = ref 0 in
          let move_loop move =
            let new_castling_rights = castling_modification move position.castling_rights in
            let new_zobrist = new_zobrist move position.last_move position.zobrist_position position.castling_rights new_castling_rights position.board in
            let new_record, new_half_moves = adapt_record new_zobrist move depth position.board_record position.half_moves in
            make position.board move;
            let new_position = {
              board = position.board;
              white_to_move = not position.white_to_move;
              last_move = move;
              castling_rights = new_castling_rights;
              board_record = new_record;
              half_moves = new_half_moves;
              zobrist_position = new_zobrist;
            }
            in let score = - quiescence_search new_position thread (depth - 1) (ply + 1) (- !beta0) (- !alpha0) ispv
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
            unmake position.board move;
            incr counter

          (*If in check search for all moves*)
          in if in_check then begin
            let move_loop_in_check () =
              let legal_moves, number_of_legal_moves = legal_moves position.board position.white_to_move position.last_move position.castling_rights king_position in_check in
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
              let captures = ref (tri_see (captures position.board position.white_to_move position.last_move) position.board position.white_to_move hash_move) in
              while !no_cut && !captures <> [] do
                move_loop (List.hd !captures);
                captures := List.tl !captures
              done
            in if hash_move <> Null && not (isquiet hash_move) then begin
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
      if not (stop_search.(thread) || total_node_counter () >= !node_limit) then begin
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
          if is_win !best_score then begin
            !best_score + ply
          end
          else if is_loss !best_score then begin
            !best_score - ply
          end
          else begin
            !best_score
          end
        in store !transposition_table position.zobrist_position node_type depth stored_value !best_move (*hash_static_eval*) !go_counter
      end;
    !best_score
    end
  end