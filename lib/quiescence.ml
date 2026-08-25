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
    if !index >= 0 then begin
      if state.(!index).zobrist = zobrist_position then begin
        repeat := true
      end
    end
    else begin
      if state.(!index).zobrist = zobrist_position then begin
        repeat := true
      end
    end;
    index := !index - 2;
  done;
  !repeat

(*let repetition state game_ply search_ply =
  let index = ref (game_ply - 2) in
  let zobrist_position = state.(game_ply).zobrist in
  let repeat = ref 0 in
  let limit = (game_ply - state.(game_ply).half_moves) in
  while !index >= limit && !repeat < 2 do
    if !index >= search_ply - game_ply then begin
      if state.(!index).zobrist = zobrist_position then begin
        repeat := 2
      end
    end
    else begin
      if state.(!index).zobrist = zobrist_position then begin
      incr repeat
      end
    end;
    index := !index - 2;
  done;
  !repeat > 1*)

let captures position moves number hash_move =
  let list = ref [] in
  for i = 0 to number - 1 do
    if get_move_flag moves.(i) > 3 && moves.(i) <> hash_move then
      list := moves.(i) :: !list
  done;
  let rec aux list = match list with
    |[] -> []
    |move :: t ->
      let note = see position move in
      if note >= 0 then
        (note, move) :: aux t
      else
        aux t
    in List.map snd (merge_sort (aux !list))

(*Fonction implémentant la recherche quiescente*)
let rec quiescence_search position ordering_tables thread depth search_ply alpha beta ispv =

  (*Check search limit*)
  if stop_search.(thread) then begin
    0
  end

  else begin
    let game_ply = position.game_ply in
    let state = position.state_array.(game_ply) in
    let in_check = state.in_check in
    
    (*Check repetion or fifty moves rule*)
    if repetition position.state_array game_ply || (state.half_moves = 100 && (not in_check || (legal_moves position search_ply; position.number_of_moves.(search_ply) <> 0))) then begin
      0
    end

    else begin
      let best_move = ref 0 in
      let hash_depth, hash_lower_bound, hash_upper_bound, hash_move, hash_static_eval = probe state.zobrist in
      let static_eval = ref hash_static_eval in
      let no_cut = ref true in
      let best_score = ref (- max_int) in
      let alpha0 = ref alpha in
      let beta0 = ref beta in

      (*Use TT informations*)
      if not (ispv || depth > hash_depth) then begin
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
          let moves = position.moves.(search_ply) in
          if !best_score > !alpha0 then begin
            alpha0 := !best_score
          end;

          let counter = ref 0 in
          let move_loop move =
            make position move;
            let score = - quiescence_search position ordering_tables thread (depth - 1) (search_ply + 1) (- !beta0) (- !alpha0) ispv
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
            let move_loop_in_check () =
              legal_moves position search_ply;
              let ordering_array = ordering_tables.working_array.(search_ply) in
              move_ordering ordering_tables position moves position.number_of_moves.(search_ply) search_ply hash_move ordering_array;
              while !no_cut do
                let move = move_picker moves ordering_array position.number_of_moves.(search_ply) in
                  if move <> 0 then begin
                    move_loop move 
                  end 
                  else begin
                    no_cut := false
                  end
              done
            in if hash_move <> 0 then begin
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
              best_score := search_ply - 99999
            end

          end

          (*Else only search for captures and promotions*)
          else begin
            let move_loop_normal () =
              legal_moves position search_ply;
              let captures = ref (captures position moves position.number_of_moves.(search_ply) hash_move) in
              while !no_cut && !captures <> [] do
                move_loop (List.hd !captures);
                captures := List.tl !captures
              done
            in if hash_move <> 0 && not (isquiet hash_move) then begin
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
      store thread state.zobrist depth !lower_bound !upper_bound !best_move !static_eval !go_counter
      end;
    !best_score
    end
  end