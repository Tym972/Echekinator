(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Board
open Bitboards
open Move_ordering
open Transposition
open Quiescence
open Evaluation

type pv_info = {
  depth : int;
  score : int;
  bestmove : int;
  }

let results = ref (Array.init !multipv (fun _ ->  {depth = 0; score = 0; bestmove = 0}))

let zugzwang pieces white_to_move =
  pieces.(knight + 6 * white_to_move) = 0L &&
  pieces.(bishop + 6 * white_to_move) = 0L &&
  pieces.(rook + 6 * white_to_move) = 0L &&
  pieces.(queen + 6 * white_to_move) = 0L

let lmr_table =
  Array.init 64
  (fun depth ->
    Array.init 64 
    (fun legal_count ->
      let reduction = 1.35 +. ((log (float_of_int depth) *. log (float_of_int legal_count)) /. 2.75)
      in if reduction > 0. then
        int_of_float reduction
      else
        0))

let nmp_min_depth = 3
let razoring_max_depth = 3
let rfp_max_depth = 7
let lmp_max_depth = 5

let rec search position search_tables thread multi depth search_ply alpha beta was_null =
  let game_ply = position.game_ply in
  let state = position.state_array.(game_ply) in
  let in_check = state.in_check in
  let ispv = beta - alpha <> 1 in
  node_counter.(thread) <- node_counter.(thread) + 1;
  if node_counter.(0) mod 1000 = 0 then begin
    if Mtime.Span.compare (Mtime_clock.count !start_time) !hard_bound > 0 then begin
      stop_search.(0) <- true
    end
  end;
  
  (*Check search limit*)
  if stop_search.(thread) || total_counter node_counter >= !node_limit then begin
    0
  end

  (*Quiescense search*)
  else if depth <= 0 then begin
    quiescence_search position search_tables thread search_ply alpha beta
  end

  (*Normal search*)
  else begin
    let picker = search_tables.pickers.(search_ply) in
    (*Check repetion or fifty moves rule*)
    if search_ply > 0 && (repetition position.state_array game_ply || (state.half_moves = 100 && (not in_check || (legal_moves position picker phase_all; picker.number_of_captures + picker.number_of_quiets <> 0)))) then begin
      0
    end

    else begin
      let alpha0 = ref (max alpha (search_ply - 99999)) in
      let beta0 = ref (min beta (99998 - search_ply)) in

      (*Mate distance pruning*)
      if !alpha0 >= !beta0 then begin
        !alpha0
      end
      
      else begin
        let best_move = ref 0 in
        let hash_depth, hash_lower_bound, hash_upper_bound, hash_move, hash_static_eval = probe state.zobrist in
        let static_eval = if hash_static_eval = - max_int then hce position else hash_static_eval in
        let no_search_cut = ref true in
        let best_score = ref (- max_int) in

        (*Use TT informations*)
        if not (ispv || depth > hash_depth) then begin
          hash_treatment hash_lower_bound hash_upper_bound alpha0 beta0 best_score no_search_cut search_ply
        end;

        if !no_search_cut then begin
          
          (*Reverse futility pruning razoring and null move pruning*)
          if not (in_check || ispv || is_loss !beta0 || zugzwang position.pieces position.white_to_move) then begin
            (*Reverse futility pruning*)
            if depth <= rfp_max_depth && static_eval - 70 * depth >= !beta0 then begin
              best_score := static_eval - 70 * depth;
              no_search_cut := false
            end;

            (*Razoring*)
            if !no_search_cut && (depth <= razoring_max_depth && static_eval + 300 + 60 * depth < !alpha0) then begin
              best_score := quiescence_search position search_tables thread search_ply alpha beta;
              no_search_cut := false
            end;

            (*Null move pruning*)
            if !no_search_cut && depth >= nmp_min_depth && not was_null && static_eval >= !beta0 + 30 then begin
              make_null position;
              let reduction = ref (3 + depth / 4) in
              if !reduction > 6 then reduction := 6;
              if !reduction > depth - 1 then reduction := depth - 1;
              let score = - search position search_tables thread multi (depth - 1 - !reduction) (search_ply + 1) (- !beta0) (- !beta0 + 1) true
              in if score >= !beta0 then begin
                if is_win score then begin
                  best_score := beta  
                end
                else begin
                  best_score := score
                end;
                no_search_cut := false
              end;
              unmake_null position
            end
          end;

          (*Move loop*)
          if !no_search_cut then begin
            let legal_counter = ref 0 in
            picker.hash_move <- hash_move;
            picker.stage <- Stage_TT;
            while !no_search_cut do
              let move = next_move position picker search_tables search_ply in
              if move <> 0 then begin
                let is_noisy = not (isquiet move) in
                let no_move_cut = ref true in

                (*Late Move Pruning*)
                if not ispv && not in_check && !best_score > -max_int && depth <= lmp_max_depth && not is_noisy && !legal_counter > 3 + depth * depth then begin
                  no_move_cut := false
                end;

                (*Futility pruning*)
                (*if not ispv && not in_check && !best_score > -max_int && depth < 4 && not is_noisy && static_eval + 90 * depth < !alpha0 then begin
                  no_move_cut := false
                end;*)

                (*See Pruning*)
                (*if not ispv && not in_check && !best_score > -max_int && depth < 2 then begin
                  let margin = if is_noisy then -120 * depth else -60 * depth in
                  if see position move < margin then
                    no_move_cut := false
                end;*)

                (*History Pruning*)
                (*if not ispv && not in_check && search_tables.history.(index move) < Margin * depth then begin
                  no_move_cut := false
                end;*)

                if !no_move_cut then begin
                  let score = ref 0 in
                  make position move;
                  incr legal_counter;
                  let gives_checks = position.state_array.(game_ply + 1).in_check in
                  let is_killer = picker.killer1 = move land 0xfff || picker.killer2 = move land 0xfff in

                  (*Late Move Reduction*)
                  if depth > 1 && !legal_counter > 1 && not (ispv && is_noisy) then begin
                    let reduction = ref lmr_table.(min depth 63).(min !legal_counter 63) in
                    if not ispv then reduction := !reduction + 2;
                    if is_killer then reduction := !reduction - 2;
                    if gives_checks then reduction := !reduction - 1;
                    if in_check then reduction := !reduction - 1;
                    if !reduction < 1 then reduction := 1;
                    if !reduction > depth - 1 then reduction := depth - 1;
                    score := - search position search_tables thread multi (depth - 1 - !reduction) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false;
                    if !score > !alpha0 then
                      score := - search position search_tables thread multi (depth - 1) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                  end
                  else if not ispv || !legal_counter > 1 then begin
                    score := - search position search_tables thread multi (depth - 1) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                  end;
                  if ispv && (!legal_counter = 1 || (!score > !alpha0 && !score < !beta0)) then begin
                    score:= - search position search_tables thread multi (depth - 1) (search_ply + 1) (- !beta0) (- !alpha0) false
                  end;
                  unmake position move;
                  if !score > !best_score then begin
                    best_score := !score;
                    if !score > !alpha0 then begin
                      best_move := move;
                      alpha0 := !score;
                      if thread + search_ply = 0 && not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
                        !results.(multi) <- {depth = depth; score = !score; bestmove = move}
                      end
                    end;
                    if !score >= !beta0 then begin
                      no_search_cut := false;
                      if not is_noisy then begin
                        search_tables.history_moves.(history_index position.white_to_move move) <- depth * depth;
                        let quiet_move = move land 0xfff in
                        let killer1 = picker.killer1 in
                        if quiet_move <> killer1 then begin
                          picker.killer1 <- quiet_move;
                          picker.killer2 <- killer1
                        end
                      end
                    end
                  end
                end
              end
              else begin
                no_search_cut := false
              end
            done;
            if !legal_counter = 0 then begin
              if in_check then begin
                best_score := search_ply - 99999
              end 
              else begin
                best_score := 0
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
          store thread state.zobrist depth !lower_bound !upper_bound !best_move static_eval !go_counter
        end;
      !best_score
      end
    end
  end