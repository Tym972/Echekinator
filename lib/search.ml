(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Board
open Generator
open Move_ordering
open Transposition
open Quiescence
open Evaluation
open Traduction

let zugzwang board white_to_move =
  let player_sign = if white_to_move then 1 else (-1) in
  let only_pawns = ref true in
  let square = ref 0 in
  while !only_pawns && !square < 64 do
    let piece = player_sign * board.(!square) in
    if piece > 0 && piece <> 1 && piece <> 6 then begin
      only_pawns := false
    end;
    incr square
  done;
  !only_pawns

(*open Evaluation*)

let rec pvs stack ordering_tables thread depth ply alpha beta ispv =
  let main_thread = thread = 0 in
  let position = stack.(ply) in
  node_counter.(thread) <- node_counter.(thread) + 1;
  if node_counter.(0) mod 1000 = 0 then begin
    if Mtime.Span.compare (Mtime_clock.count !start_time) !hard_bound > 0 then begin
      stop_search.(0) <- true
    end
  end;
  
  (*Check search limit*)
  if stop_search.(thread) || total_counter node_counter >= !node_limit then begin
    if main_thread && ispv then begin
      pv_length.(ply) <- 0
    end;
    0
  end

  (*Quiescense search*)
  else if depth = 0 then begin
    quiescence_search stack thread depth ply alpha beta ispv 
  end

  (*Normal search*)
  else begin
    let king_position = index_array position.board (king position.white_to_move) in
    let in_check = threatened position.board king_position in
    (*let bg = Array.copy accumulator in Array.blit bg 0 accumulator 0 n;*)
    (*vector board;
    if evaluate () <> make_output_layer board_vector then begin
      (*print_endline (string_of_bool (board = board_of_vector board_vector));
      print_board board; print_board (board_of_vector board_vector);*)
      print_endline (string_of_float (evaluate ()) ^ " " ^ string_of_float (make_output_layer board_vector))
    end; *)

    (*Check repetion or fifty moves rule*)
    if repetition stack ply || (position.half_moves = 100 && (not in_check || (let _, number_of_moves = legal_moves position king_position in_check in !number_of_moves <> 0))) then begin
      if main_thread && ispv then begin
        pv_length.(ply) <- 0
      end;
      0
    end

    else begin
      let alpha0 = ref (max alpha (ply - 99999)) in
      let beta0 = ref (min beta (99998 - ply)) in

      (*Mate distance pruning*)
      if !alpha0 >= !beta0 then begin
        !alpha0
      end
      
      else begin
        let best_move = ref Null in
        let hash_node_type, hash_depth, hash_value, hash_move(*, hash_static_eval*) = probe !transposition_table position in
        let no_cut = ref true in
        let best_score = ref (- max_int) in

        (*let static_eval =
          if hash_static_eval > (-infinity) then begin
            hash_static_eval
          end
          else begin
            evaluation board white_to_move
            (*let _ = evaluate () in*)
          end
        in*)

        (*Use TT informations*)
        if not (ispv || depth > hash_depth) then begin
          hash_treatment hash_node_type hash_value alpha0 beta0 best_score no_cut ply
        end;

        if !no_cut then begin
          
          (*Reverse futility pruning and null move pruning*)
          if not (in_check || ispv || is_loss !beta0 || zugzwang position.board position.white_to_move) then begin
            let static_eval = hce position in
            (*let _ = evaluate () in*)
            if depth < 3 then begin
              let margin = 100 * depth in
              if static_eval - margin >= !beta0 then begin
                best_score := static_eval - margin;
                no_cut := false
              end
            end
            else if static_eval >= !beta0 then begin
              make position stack.(ply + 1) Null;
              let score = - pvs stack ordering_tables thread (depth - 3) (ply + 1) (- !beta0) (- !beta0 + 1) false
              in if score >= !beta0 then begin
                if is_win score then begin
                  best_score := beta  
                end
                else begin
                  best_score := score
                end;
                no_cut := false
              end
            end
          end;

          (*Move loop*)
          if !no_cut then begin
            let counter = ref 0 in
            let move_loop move =
              make position stack.(ply + 1) move;
              let score =
                if !counter = 0 then begin
                  - pvs stack ordering_tables thread (depth - 1) (ply + 1) (- !beta0) (- !alpha0) ispv
                end
                else begin
                  let score_lmr =
                    let reduction =
                      let float_depth = float_of_int depth in
                      let float_counter = float_of_int (!counter - 1) in
                      min
                        (int_of_float begin
                          if isquiet move stack.(ply + 1).last_capture then
                            1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                          else
                            0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                        end)
                        (depth - 1)
                    in if not (in_check || depth < 3 || reduction = 0) then begin
                      - pvs stack ordering_tables thread (depth - 1 - reduction) (ply + 1) (- !alpha0 - 1) (- !alpha0) false
                    end
                    else
                      !alpha0 + 1
                  in if score_lmr > !alpha0 then begin
                    let score_0 = - pvs stack ordering_tables thread (depth - 1) (ply + 1) (- !alpha0 - 1) (- !alpha0) false
                    in if (score_0 > !alpha0 && ispv) then begin
                      - pvs stack ordering_tables thread (depth - 1) (ply + 1) (- !beta0) (- !alpha0) ispv
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
                  if main_thread && ispv then begin
                    pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2) <- !best_move;
                    for i = 1 to pv_length.(ply + 1) do
                      pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2 + i) <- pv_table.((ply + 1) * (2 * max_pv_length - ply) / 2 + i - 1)
                    done;
                    pv_length.(ply) <- pv_length.(ply + 1) + 1
                  end
                end;
                if score > !alpha0 then begin
                  alpha0 := score
                end;
                if score >= !beta0 then begin
                  no_cut := false;
                  if isquiet move stack.(ply + 1).last_capture then begin
                    ordering_tables.history_moves.(4096 * aux_history position.white_to_move + 64 * from move + to_ move) <- depth * depth;
                    ordering_tables.killer_moves.(2 * ply + 1) <- ordering_tables.killer_moves.(2 * ply);
                    ordering_tables.killer_moves.(2 * ply) <- !best_move
                  end
                end
              end;
              unmake position.board move stack.(ply + 1).last_capture;
              incr counter
            in if hash_move <> Null then begin
              move_loop hash_move;
              if !no_cut then begin
                let legal_moves, number_of_legal_moves = legal_moves position king_position in_check in
                let ordering_array = Array.make !number_of_legal_moves 0 in
                move_ordering ordering_tables position legal_moves number_of_legal_moves ply hash_move ordering_array;
                while !no_cut && !number_of_legal_moves > 0 do
                  move_loop (move_picker legal_moves ordering_array number_of_legal_moves)
                done
              end
            end
            else begin
              let legal_moves, number_of_legal_moves = legal_moves position king_position in_check in
              let ordering_array = Array.make !number_of_legal_moves 0 in
              move_ordering ordering_tables position legal_moves number_of_legal_moves ply Null ordering_array;
              while !no_cut && !number_of_legal_moves > 0 do
                move_loop (move_picker legal_moves ordering_array number_of_legal_moves)
              done
            end;
            if !counter = 0 then begin
              if main_thread && ispv then begin
                pv_length.(ply) <- 0
              end;
              if in_check then begin
                best_score := ply - 99999
              end 
              else begin
                best_score := 0
              end
            end
          end
        end;
        
        (*Storing in TT*)
        if not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
          let node_type =
            if !best_score <= alpha then begin
              if main_thread && ispv then begin
                pv_length.(ply) <- 0
              end;
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
          in store thread position.zobrist_position node_type depth stored_value !best_move (*hash_static_eval*) !go_counter
        end;
      !best_score
      end
    end
  end

let root_search stack ordering_tables thread in_check depth alpha beta first_move legal_moves number_of_legal_moves short_pv_table multi number_of_pv =
  let main_thread = thread = 0 in
  let position = stack.(0) in
  node_counter.(thread) <- node_counter.(thread) + 1;
  let no_cut = ref true in
  let alpha0 = ref alpha in
  let beta0 = ref beta in
  let best_score = ref (- max_int) in
  let best_move = ref Null in
  (*let static_eval = evaluation board white_to_move in*)
  let counter = ref 0 in
  let move_loop move =
    make position stack.(1) move;
    let score =
      if !counter = 0 then begin
        - pvs stack ordering_tables thread (depth - 1) 1 (- !beta0) (- !alpha0) true
      end
      else begin
        let score_lmr =
          let reduction =
            let float_depth = float_of_int depth in
            let float_counter = float_of_int (!counter - 1) in
            min
              (int_of_float begin
                if isquiet move stack.(1).last_capture then
                  1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                else
                  0.20 +. log (float_depth) *. log (float_counter) /. 3.35
              end)
              (depth - 1)
          in if not (in_check || depth < 3 || reduction = 0) then begin
            - pvs stack ordering_tables thread (depth - 1 - reduction) 1 (- !alpha0 - 1) (- !alpha0) false
          end
          else
            !alpha0 + 1
        in if score_lmr > !alpha0 then begin
          let score_0 = - pvs stack ordering_tables thread (depth - 1) 1 (- !alpha0 - 1) (- !alpha0) false
          in if (score_0 > !alpha0) then begin 
            - pvs stack ordering_tables thread (depth - 1) 1 (- !beta0) (- !alpha0) true
          end
          else begin
            score_0
          end
        end
        else
          score_lmr
      end
    in if score > !best_score && not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
      best_score := score;
      if score > !alpha0 || !counter = 0 then begin
        best_move := move;
        if main_thread then begin
          pv_table.(0) <- !best_move;
          for i = 1 to pv_length.(1) do
            pv_table.(i) <- pv_table.(max_pv_length + i - 1)
          done;
          pv_length.(0) <- pv_length.(1) + 1;
        end;
        short_pv_table.(thread * number_of_pv + multi) <- pv_finder depth
      end;
      if score > !alpha0 then begin
        alpha0 := score;
      end;
      if score >= !beta0 then begin
        no_cut := false;
        if isquiet move stack.(1).last_capture then begin
          ordering_tables.history_moves.(4096 * aux_history position.white_to_move + 64 * from move + to_ move) <- depth * depth;
          ordering_tables.killer_moves.(1) <- ordering_tables.killer_moves.(0);
          ordering_tables.killer_moves.(0) <- !best_move
        end
      end
    end;
    unmake position.board move stack.(1).last_capture;
    incr counter
  in if first_move <> Null then begin
    move_loop first_move;
    if !no_cut then begin
      let ordering_array = Array.make !number_of_legal_moves 0 in
      move_ordering ordering_tables position legal_moves number_of_legal_moves 0 first_move ordering_array;
      let moves = ref (merge_sort (List.init !number_of_legal_moves (fun i -> (ordering_array.(i), legal_moves.(i))))) in
      while !no_cut && !moves <> [] do
        move_loop (snd (List.hd !moves));
        moves := List.tl !moves
      done;
    end
  end
  else begin
    let ordering_array = Array.make !number_of_legal_moves 0 in
    move_ordering ordering_tables position legal_moves number_of_legal_moves 0 Null ordering_array;
    let moves = ref (merge_sort (List.init !number_of_legal_moves (fun i -> (ordering_array.(i), legal_moves.(i))))) in
    while !no_cut && !moves <> [] do
      move_loop (snd (List.hd !moves));
      moves := List.tl !moves
    done;
  end;
  if not (stop_search.(thread) || total_counter node_counter >= !node_limit) then begin
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
    in store thread position.zobrist_position node_type depth !best_score !best_move (*hash_static_eval*) !go_counter
  end;
  !best_score