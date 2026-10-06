(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Board
open Bitboards
open Move_ordering
open Transposition
open Quiescence
open Evaluation

type search_result =
  {score : int;
  bestmove : int}

type nodes_fraction =
  {move : int;
  nodes : int}

let search_record = ref (Array.init !multipv (fun _ -> Array.init (max_depth + 1) (fun _ -> {score = -max_int; bestmove = 0})))

let nodes_fraction = ref (Array.init !multipv (fun _ -> Array.init 218 (fun _ -> {move = 0; nodes = 0})))

let zugzwang pieces white_to_move =
  pieces.(knight + 6 * white_to_move) = 0L &&
  pieces.(bishop + 6 * white_to_move) = 0L &&
  pieces.(rook + 6 * white_to_move) = 0L &&
  pieces.(queen + 6 * white_to_move) = 0L

let lmr_quiet =
  Array.init 64
  (fun depth ->
    Array.init 64 
    (fun legal_count ->
      let reduction = Float.round ((log (float_of_int depth) *. log (float_of_int legal_count)) /. 1.6)
      in if reduction > 5. then 5 else if reduction > 0. then int_of_float reduction else 0))

let lmr_noisy =
  Array.init 64
  (fun depth ->
    Array.init 64 
    (fun legal_count ->
      let reduction = 0.20 +. ((log (float_of_int depth) *. log (float_of_int legal_count)) /. 3.35)
      in if reduction > 0. then int_of_float reduction else 0))

let nmp_min_depth = 3
let iir_min_depth = 4
let lmr_min_depth = 3
let razoring_max_depth = 3
let rfp_max_depth = 7
let lmp_max_depth = 3
let fp_max_depth = 3

let rec search position search_tables thread multi depth search_ply alpha beta was_null =
  let game_ply = position.game_ply in
  let state = position.state_array.(game_ply) in
  let in_check = state.in_check in
  let white_to_move = position.white_to_move in
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
    if search_ply > 0 && begin
        repetition position.state_array game_ply ||
        (state.half_moves = 100 &&
          (not in_check ||
          (legal_moves position picker phase_all; picker.number_of_captures + picker.number_of_quiets <> 0))) ||
        is_material_draw position
      end
    then begin
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
              let reduction = min 6 (min (depth - 1) (3 + depth / 4)) in
              let score = - search position search_tables thread multi (depth - 1 - reduction) (search_ply + 1) (- !beta0) (- !beta0 + 1) true in
              if score >= !beta0 then begin
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


          if !no_search_cut then begin

            (*Internal Iterative Reduction*)
            let depth = if depth >= iir_min_depth && hash_move = 0 && not in_check && search_ply > 0 then depth - 1 else depth in

            let legal_counter = ref 0 in
            let quiet_counter = ref 0 in
            let quiet_moves = ref [] in
            picker.hash_move <- hash_move;
            picker.stage <- Stage_TT;
            let initial_nodes = ref node_counter.(thread) in

            (*Move loop*)
            while !no_search_cut do
              let move = next_move position picker search_tables search_ply in
              if move <> 0 then begin
                let is_quiet = isquiet move in
                if is_quiet then
                  incr quiet_counter;
                let no_move_cut = ref true in

                (*SEE Pruning*)
                if not ispv && not in_check && !best_score > -max_int && (not is_quiet) then begin
                  let from = get_move_from move in
                  let to_ = get_move_to move in
                  let moving_piece = position.board.(from) in
                  let target_piece = position.board.(to_) in
                  if tabvalue.(moving_piece mod 6) > tabvalue.(target_piece mod 6) && see position move < -100 * depth then begin
                    no_move_cut := false
                  end
                end;

                if !no_move_cut then begin
                  let score = ref 0 in
                  make position move;
                  incr legal_counter;
                  let gives_checks = position.state_array.(game_ply + 1).in_check in

                  (*Late quiet moves pruning*)
                  if not (not ispv && not in_check && not gives_checks && is_quiet && depth <= lmp_max_depth && !quiet_counter > depth * depth + 3) then begin
                    
                    (*Search Extension*)
                    let extension = if gives_checks then 1 else 0 in

                    (*Late Move Reduction*)
                    if depth >= lmr_min_depth && !legal_counter > 4 && is_quiet && not gives_checks then begin
                      let reduction  = min (depth - 2) (lmr_quiet.(min depth 63).(min !legal_counter 63)) in

                      (*Reduced depth search with null window*)
                      score := - search position search_tables thread multi (depth - 1 + extension - reduction) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false;
                      
                      (*No fail low : research at full depth with null window*)
                      if !score > !alpha0 && reduction > 0 then begin
                        score := - search position search_tables thread multi (depth - 1 + extension) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                      end

                    end

                    (*Full depth, Null window Search*)
                    else if not ispv || !legal_counter > 1 then begin
                      score := - search position search_tables thread multi (depth - 1 + extension) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                    end;

                    (*Full depth, Normal Window Search*)
                    if ispv && (!legal_counter = 1 || (!score > !alpha0 && !score < !beta0)) then begin
                      score := - search position search_tables thread multi (depth - 1 + extension) (search_ply + 1) (- !beta0) (- !alpha0) false
                    end;

                    if is_quiet then begin
                      quiet_moves := move :: !quiet_moves
                    end;

                    (*New best move*)
                    if !score > !best_score then begin

                      best_score := !score;

                      (*Score is not below then window*)
                      if !score > !alpha0 then begin
                        best_move := move;
                        alpha0 := !score;

                        (*Recover bestmove at root*)
                        if thread + search_ply = 0 && not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
                          !search_record.(multi).(depth) <- {score = !score; bestmove = move}
                        end

                      end;

                      (*Cutoff*)
                      if !score >= !beta0 then begin
                        no_search_cut := false;

                        (*History and killer heuristic*)
                        if is_quiet then begin

                          (*Bonus for cutoff move*)
                          let bonus = depth * depth in
                          let index = history_index white_to_move move in
                          let prev_history = search_tables.history_moves.(index) in
                          let history = prev_history + bonus - prev_history * bonus / 16000 in
                          search_tables.history_moves.(index) <- min 16000 history;

                          (*Malus for precedent moves*)
                          List.iter (
                            fun quiet_move -> 
                              let index = history_index white_to_move quiet_move in
                              let prev_history = search_tables.history_moves.(index) in
                              let history = prev_history - bonus - prev_history * bonus / 16000 in
                              search_tables.history_moves.(index) <- max (-16000) history)
                            (List.tl !quiet_moves);

                          (*Killers update*)
                          let quiet_move = move land 0xfff in
                          let killer1 = picker.killer1 in
                          if quiet_move <> killer1 then begin
                            picker.killer1 <- quiet_move;
                            picker.killer2 <- killer1
                          end

                        end
                      end
                    end
                  end;
                  unmake position move;
                  if search_ply + thread = 0 then begin
                    let nodes_count = node_counter.(0) in
                    !nodes_fraction.(multi).(!legal_counter - 1) <- {
                      move = move;
                      nodes = nodes_count - !initial_nodes
                      };
                    initial_nodes := nodes_count
                  end
                end
              end
              else begin
                no_search_cut := false
              end
            done;

            (*No legal move*)
            if !legal_counter = 0 then begin

              (*Checkmate*)
              if in_check then begin
                best_score := search_ply - 99999
              end 

              (*Stalemate*)
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