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

let zugzwang position =
  let player_pieces = pieces_rep.(position.white_to_move) in
  position.pieces.(player_pieces.(knight)) = 0L &&
  position.pieces.(player_pieces.(bishop)) = 0L &&
  position.pieces.(player_pieces.(rook)) = 0L &&
  position.pieces.(player_pieces.(queen)) = 0L

let rec pvs position ordering_tables thread multi depth search_ply alpha beta ispv =
  let game_ply = position.game_ply in
  let state = position.state_array.(game_ply) in
  let in_check = state.in_check in
  let grossiere_erreur = in_check in
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
  else if depth = 0 then begin
    quiescence_search position ordering_tables thread depth search_ply alpha beta ispv
  end

  (*Normal search*)
  else begin
    (*let bg = Array.copy accumulator in Array.blit bg 0 accumulator 0 n;*)
    (*vector board;
    if evaluate () <> make_output_layer board_vector then begin
      (*print_endline (string_of_bool (board = board_of_vector board_vector));
      print_board board; print_board (board_of_vector board_vector);*)
      print_endline (string_of_float (evaluate ()) ^ " " ^ string_of_float (make_output_layer board_vector))
    end; *)

    (*Check repetion or fifty moves rule*)
    if search_ply > 0 && (repetition position.state_array game_ply (*search_ply*) || (state.half_moves = 100 && (not in_check || (legal_moves position search_ply; position.number_of_moves.(search_ply) <> 0)))) then begin
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
        let static_eval = ref hash_static_eval in
        let no_cut = ref true in
        let best_score = ref (- max_int) in

        (*Use TT informations*)
        if not (ispv || depth > hash_depth) then begin
          hash_treatment hash_lower_bound hash_upper_bound alpha0 beta0 best_score no_cut search_ply
        end;

        if !no_cut then begin
          
          (*Reverse futility pruning and null move pruning*)
          if not (in_check || ispv || is_loss !beta0 || zugzwang position) then begin
            if hash_static_eval = (-max_int) then begin
              static_eval := hce position
            end;
            (*let _ = evaluate () in*)
            if depth < 3 then begin
              let margin = 100 * depth in
              if !static_eval - margin >= !beta0 then begin
                best_score := !static_eval - margin;
                no_cut := false
              end
            end
            else if !static_eval >= !beta0 then begin
              make_null position;
              let score = - pvs position ordering_tables thread multi (depth - 3) (search_ply + 1) (- !beta0) (- !beta0 + 1) false
              in if score >= !beta0 then begin
                if is_win score then begin
                  best_score := beta  
                end
                else begin
                  best_score := score
                end;
                no_cut := false
              end;
              unmake_null position
            end
          end;

          (*Move loop*)
          if !no_cut then begin
            let counter = ref 0 in
            let moves = position.moves.(search_ply) in
            let ordering_array = ordering_tables.working_array.(search_ply) in
            let move_loop move =
              make position move;
              let score =
                if !counter = 0 then begin
                  - pvs position ordering_tables thread multi (depth - 1) (search_ply + 1) (- !beta0) (- !alpha0) ispv
                end
                else begin
                  let score_lmr =
                    let reduction =
                      let float_depth = float_of_int depth in
                      let float_counter = float_of_int (!counter - 1) in
                      min
                        (int_of_float begin
                          if isquiet move then
                            1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                          else
                            0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                        end)
                        (depth - 1)
                    in if not (grossiere_erreur || depth < 3 || reduction = 0) then begin
                      - pvs position ordering_tables thread multi (depth - 1 - reduction) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                    end
                    else
                      !alpha0 + 1
                  in if score_lmr > !alpha0 then begin
                    let score_0 = - pvs position ordering_tables thread multi (depth - 1) (search_ply + 1) (- !alpha0 - 1) (- !alpha0) false
                    in if (score_0 > !alpha0 && ispv) then begin
                      - pvs position ordering_tables thread multi (depth - 1) (search_ply + 1) (- !beta0) (- !alpha0) ispv
                    end
                    else begin
                      score_0
                    end
                  end
                  else
                    score_lmr
                end
              in if score > !best_score then begin
                best_score := score;
                if score > !alpha0 then begin
                  best_move := move;
                  alpha0 := score;
                  if thread + search_ply = 0 && not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
                    !results.(multi) <- {depth = depth; score = score; bestmove = move}
                  end
                end;
                if score >= !beta0 then begin
                  no_cut := false;
                  if isquiet move then begin
                    ordering_tables.history_moves.(history_index (position.white_to_move lxor 1) move) <- depth * depth;
                    let quiet_move = move land 0xfff in
                    let killer0 = ordering_tables.killer_moves.(2 * search_ply) in
                    if quiet_move <> killer0 then begin
                      ordering_tables.killer_moves.(2 * search_ply) <- quiet_move;
                      ordering_tables.killer_moves.(2 * search_ply + 1) <- killer0
                    end
                  end
                end
              end;
              unmake position move;
              incr counter
            in if hash_move <> 0 then begin
              move_loop hash_move;
              if !no_cut then begin
                if search_ply <> 0 || true then begin
                  legal_moves position search_ply;
                  move_ordering ordering_tables position moves position.number_of_moves.(search_ply) search_ply hash_move ordering_array
                end;
                while !no_cut do
                  let move = move_picker moves ordering_array position.number_of_moves.(search_ply) in
                  if move <> 0 then begin
                    move_loop move 
                  end 
                  else begin
                    no_cut := false
                  end
                done
              end
            end
            else begin
              if search_ply <> 0 || true then begin
                legal_moves position search_ply;
                move_ordering ordering_tables position moves position.number_of_moves.(search_ply) search_ply 0 ordering_array
              end;
              while !no_cut do
                let move = move_picker moves ordering_array position.number_of_moves.(search_ply)in
                if move <> 0 then begin
                  move_loop move 
                end 
                else begin
                  no_cut := false
                end
              done
            end;
            if !counter = 0 then begin
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
          store thread state.zobrist depth !lower_bound !upper_bound !best_move !static_eval !go_counter
        end;
      !best_score
      end
    end
  end