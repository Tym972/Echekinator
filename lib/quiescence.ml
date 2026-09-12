(*Module implémentant les fonctions permettant la recherche quiescente*)

open Board
open Bitboards
open Move_ordering
open Transposition
open Evaluation

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition state game_ply =
  let index = ref (game_ply - 2) in
  let zobrist_position = state.(game_ply).zobrist in
  let repeat = ref false in
  let limit = (game_ply - state.(game_ply).half_moves) in
  while !index >= limit && not !repeat do
    if state.(!index).zobrist = zobrist_position then begin
      repeat := true
    end;
    index := !index - 2;
  done;
  !repeat

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search position search_tables thread search_ply alpha beta ispv =

  (*Check search limit*)
  if stop_search.(thread) then begin
    0
  end

  else begin
    let picker = search_tables.pickers.(search_ply) in
    let game_ply = position.game_ply in
    let state = position.state_array.(game_ply) in
    let in_check = state.in_check in
    
    (*Check repetion or fifty moves rule*)
    if repetition position.state_array game_ply || (state.half_moves = 100 && (not in_check || (legal_moves position picker phase_all; picker.number_of_captures + picker.number_of_quiets <> 0))) then begin
      0
    end

    else begin
      let best_move = ref 0 in
      let _, hash_lower_bound, hash_upper_bound, hash_move, hash_static_eval = probe state.zobrist in
      let static_eval = ref hash_static_eval in
      let no_cut = ref true in
      let best_score = ref (- max_int) in
      let alpha0 = ref alpha in
      let beta0 = ref beta in

      (*Use TT informations*)
      if not ispv then begin
        hash_treatment hash_lower_bound hash_upper_bound alpha0 beta0 best_score no_cut search_ply
      end;
      if !no_cut then begin

        (*Static eval*)
        if not (in_check || hash_static_eval <> (- max_int)) then begin
          static_eval := hce position
        end;
        best_score := !static_eval;

        (*Stand pat verification then move loop*)
        if !best_score < beta then begin
          if !best_score > !alpha0 then begin
            alpha0 := !best_score
          end;

          let counter = ref 0 in
          picker.stage <- Stage_TT;
          picker.hash_move <- hash_move;
          let move_loop move =
            make position move;
            let score = - quiescence_search position search_tables thread (search_ply + 1) (- !beta0) (- !alpha0) ispv
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
            unmake position move;
            incr counter

          (*If in check search for all moves*)
          in if in_check then begin
            
            while !no_cut do
              let move = next_move position picker search_tables search_ply in
              if move <> 0 then begin
                move_loop move 
              end 
              else begin
                no_cut := false
              end
            done;

            (*Check for mate*)
            if !best_score = (- max_int) then begin
              best_score := search_ply - 99999
            end

          end

          (*Else only search for captures and promotions*)
          else begin
            
            while !no_cut do
              let move = qsearch_next_move position picker search_tables in
              if move <> 0 then begin
                move_loop move
              end 
              else begin
                no_cut := false
              end
            done

          end

        end
      end;

      (*Storing in TT*)
      if not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
        let lower_bound = ref (- max_int) in
        let upper_bound = ref max_int in
        let stored_value =
          if is_win !best_score then begin
            !best_score + search_ply
          end
          else if is_loss !best_score then begin
            !best_score - search_ply
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
      store thread state.zobrist 0 !lower_bound !upper_bound !best_move !static_eval !go_counter
      end;
    !best_score
    end
  end