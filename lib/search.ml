(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Board
open Generator
open Zobrist
open Move_ordering
open Transposition
open Quiescence

let adapt_record zobrist_position move depth board_record half_moves =
  if is_irreversible move then begin
    if depth < 8 then begin
      [], 0
    end
    else begin
      [zobrist_position], 0
    end
  end
  else if half_moves + depth < 7 then begin
    [], half_moves + 1
  end
  else begin 
    zobrist_position :: board_record, half_moves + 1
  end

let max_pv_length = max_depth
let pv_table = Array.make ((max_pv_length) * (max_pv_length + 1) / 2) Null
let pv_length = Array.make max_pv_length 0

(*let verif board move = match move with
  |Normal {piece; from; to_; capture} -> board.(from) = piece && board.(to_) = capture
  |Enpassant {from; to_} -> board.(from) = (if from < 32 then 1 else -1)  && board.(to_) = 0
  |Castling {sort} -> if sort < 3 then board.(!from_white_king) = 6 else board.(!from_black_king) = (-6)
  |Promotion {from; to_; capture; promotion} -> board.(from) = (if promotion > 0 then 1 else -1) && board.(to_) = capture
  |Null -> false*)

(*open Evaluation*)

let rec pvs board white_to_move last_move castling_right board_record half_moves depth ply alpha beta evaluation zobrist_position ispv =
  (*let bg = Array.copy accumulator in Array.blit bg 0 accumulator 0 n;*)
  (*vector board;
  if evaluate () <> make_output_layer board_vector then begin
    (*print_endline (string_of_bool (board = board_of_vector board_vector));
    print_board board; print_board (board_of_vector board_vector);*)
    print_endline (string_of_float (evaluate ()) ^ " " ^ string_of_float (make_output_layer board_vector))
  end; *)
  incr node_counter;
  if !stop_calculation || !node_counter >= !node_limit || repetition board_record 3 then begin
    if ispv then begin
      pv_length.(ply) <- 0
    end;
    0
  end
  else begin
    let no_cut = ref true in
    let alpha0 = ref (max alpha (ply - 99999)) in
    let beta0 = ref (min beta (99998 - ply)) in
    let best_score = ref (-infinity) in
    if !alpha0 >= !beta0 then begin
      best_score := !alpha0;
      no_cut := false
    end;
    if !no_cut then begin
      let best_move = ref Null in
      let hash_node_type, hash_depth, hash_value, hash_move = probe transposition_table zobrist_position in
      if hash_depth <> (-1) && not ispv then begin
        hash_treatment hash_node_type hash_depth hash_value hash_move depth alpha0 beta0 best_score best_move no_cut ply
      end;
      if !no_cut then begin
        let king_position = index_array board (king white_to_move) in
        let in_check = threatened board king_position white_to_move in
        if depth = 0 then begin
          best_score := quiescence_treatment_depth_0 ply evaluation board white_to_move last_move !alpha0 !beta0 king_position in_check
        end
        else begin
          if not (in_check || ispv) then begin
            let static_eval = evaluation board white_to_move king_position in_check alpha beta in
            (*let _ = evaluate () in*)
            if not !zugzwang then begin
              if depth < 3 then begin
                let eval_margin = 150 * depth in
                if static_eval - eval_margin >= !beta0 then begin
                  best_score := static_eval - eval_margin;
                  no_cut := false
                end
              end
              else if static_eval >= !beta0 then begin
                let new_zobrist = new_zobrist Null last_move zobrist_position castling_right castling_right board in
                let score = - pvs board (not white_to_move) Null castling_right board_record half_moves (depth - 3) (ply + 1) (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
                in if score >= !beta0 then begin
                  best_score := score;
                  no_cut := false;
                end
              end
            end
          end;
          let hash_ordering = !no_cut && hash_move <> Null (*&& verif board hash_move*) in
          if hash_ordering then begin
            let new_castling_right = castling_modification hash_move castling_right in
            let new_zobrist = new_zobrist hash_move last_move zobrist_position castling_right new_castling_right board in
            let new_record, new_half_moves = adapt_record new_zobrist hash_move depth board_record half_moves in
            make board hash_move;
            let score = - pvs board (not white_to_move) hash_move new_castling_right new_record new_half_moves (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation new_zobrist ispv
            in if score > !best_score then begin
              best_score := score;
              best_move := hash_move;
              if ispv && not (!stop_calculation || !node_counter >= !node_limit) then begin
                pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2) <- !best_move;
                for i = 1 to pv_length.(ply + 1) do
                  pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2 + i) <- pv_table.((ply + 1) * (2 * max_pv_length - ply) / 2 + i - 1)
                done;
                pv_length.(ply) <- pv_length.(ply + 1) + 1
              end;
              alpha0 := max !alpha0 score;
              if score >= !beta0 then begin
                no_cut := false;
                if isquiet !best_move then begin
                  history_moves.(4096 * aux_history white_to_move + 64 * from !best_move + to_ !best_move) <- depth * depth;
                  killer_moves.(2 * ply + 1) <- killer_moves.(2 * ply);
                  killer_moves.(2 * ply) <- !best_move;
                end
              end
            end;
            unmake board hash_move
          end;
          if !no_cut then begin
            let moves =
              if hash_ordering then
                ref (List.filter (fun c -> c <> hash_move) (move_ordering board white_to_move last_move castling_right king_position in_check ply))
              else
                ref (move_ordering board white_to_move last_move castling_right king_position in_check ply)
            in if !moves = [] && not hash_ordering then begin
              if ispv then begin
                pv_length.(ply) <- 0
              end;
              if in_check then begin
                best_score := ply - 99999
              end 
              else begin
                best_score := 0
              end
            end
            else if half_moves = 100 then begin
              if ispv then begin
                pv_length.(ply) <- 0
              end;
              best_score := 0
            end
            else begin
              let counter = ref 0 in
              while (!no_cut && !moves <> []) do
                let move = List.hd !moves in
                let new_castling_right = castling_modification move castling_right in
                let new_zobrist = new_zobrist move last_move zobrist_position castling_right new_castling_right board in
                let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
                make board move;
                moves := List.tl !moves;
                let score =
                  if not (!counter <> 0 || hash_ordering) then begin
                    - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation new_zobrist ispv
                  end
                  else begin
                    let score_lmr =
                      let reduction =
                        let float_depth = float_of_int depth in
                        let float_counter = float_of_int !counter in
                        min
                          (int_of_float begin
                            if isquiet move then
                              1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                            else
                              0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                          end)
                          (depth - 1)
                      in if not (in_check || depth < 3 || reduction = 0) then begin
                        - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1 - reduction) (ply + 1) (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
                      end
                      else
                        !alpha0 + 1
                    in if score_lmr > !alpha0 then begin
                      let score_0 = - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) (ply + 1) (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
                      in if (score_0 > !alpha0 && ispv) then begin 
                        - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation new_zobrist true
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
                  best_move := move;
                  if ispv && not (!stop_calculation || !node_counter >= !node_limit) then begin
                    pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2) <- !best_move;
                    for i = 1 to pv_length.(ply + 1) do
                      pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2 + i) <- pv_table.((ply + 1) * (2 * max_pv_length - ply) / 2 + i - 1)
                    done;
                    pv_length.(ply) <- pv_length.(ply + 1) + 1
                  end;
                  alpha0 := max !alpha0 score;
                  if score >= !beta0 then begin
                    no_cut := false;
                    if isquiet !best_move then begin
                      history_moves.(4096 * aux_history white_to_move + 64 * from !best_move + to_ !best_move) <- depth * depth;
                      killer_moves.(2 * ply + 1) <- killer_moves.(2 * ply);
                      killer_moves.(2 * ply) <- !best_move;
                    end;
                  end
                end;
                unmake board move;
                incr counter
              done
            end
          end
        end
      end;
      if not (!stop_calculation || !node_counter >= !node_limit) then begin
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
        in store transposition_table zobrist_position node_type depth stored_value !best_move !go_counter
      end
    end;
    !best_score
  end

let root_search board white_to_move last_move castling_right board_record half_moves king_position in_check depth evaluation zobrist_position first_move =
  incr node_counter;
  let no_cut = ref true in
  let alpha0 = ref (-infinity) in
  let beta0 = ref infinity in
  let best_score = ref (-infinity) in
  let best_move = ref Null in
  if first_move <> Null then begin
    let new_castling_right = castling_modification first_move castling_right in
    let new_zobrist = new_zobrist first_move last_move zobrist_position castling_right new_castling_right board in
    let new_record, new_half_moves = adapt_record new_zobrist first_move depth board_record half_moves in
    make board first_move;
    let score = - pvs board (not white_to_move) first_move new_castling_right new_record new_half_moves (depth - 1) 1 (- !beta0) (- !alpha0) evaluation new_zobrist true
    in if score > !best_score then begin
      best_score := score;
      best_move := first_move;
      if not (!stop_calculation || !node_counter >= !node_limit) then begin
        pv_table.(0) <- !best_move;
        for i = 1 to pv_length.(1) do
          pv_table.(i) <- pv_table.(max_pv_length + i - 1)
        done;
        pv_length.(0) <- pv_length.(1) + 1
      end;
      alpha0 := max !alpha0 score;
      if score >= !beta0 then begin
        no_cut := false;
        if isquiet !best_move then begin
          history_moves.(4096 * aux_history white_to_move + 64 * from !best_move + to_ !best_move) <- depth * depth;
          killer_moves.(1) <- killer_moves.(0);
          killer_moves.(0) <- !best_move;
        end
      end
    end;
    unmake board first_move
  end;
  if !no_cut then begin
    let moves =
      if first_move <> Null then
        ref (List.filter (fun c -> c <> first_move) (move_ordering board white_to_move last_move castling_right king_position in_check 0))
      else
        ref (move_ordering board white_to_move last_move castling_right king_position in_check 0)
      in if !moves = [] && first_move = Null then begin
      pv_length.(0) <- 0;
      if in_check then begin
        best_score := - 99999
      end 
      else begin
        best_score := 0
      end
    end
    else if half_moves = 100 then begin
      pv_length.(0) <- 0;
      best_score := 0
    end
    else begin
      let counter = ref 0 in
      while (!no_cut && !moves <> []) do
        let move = List.hd !moves in
        let new_castling_right = castling_modification move castling_right in
        let new_zobrist = new_zobrist move last_move zobrist_position castling_right new_castling_right board in
        let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
        make board move;
        moves := List.tl !moves;
        let score =
          if not (!counter <> 0 || first_move <> Null)then begin
            - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) 1 (- !beta0) (- !alpha0) evaluation new_zobrist true
          end
          else begin
            let score_lmr =
              let reduction =
                let float_depth = float_of_int depth in
                let float_counter = float_of_int !counter in
                min
                  (int_of_float begin
                    if isquiet move then
                      1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                    else
                      0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                  end)
                  (depth - 1)
              in if not (in_check || depth < 3 || reduction = 0) then begin
                - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1 - reduction) 1 (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
              end
              else
                !alpha0 + 1
            in if score_lmr > !alpha0 then begin
              let score_0 = - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) 1 (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
              in if (score_0 > !alpha0) then begin 
                - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) 1 (- !beta0) (- !alpha0) evaluation new_zobrist true
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
          best_move := move;
          if not (!stop_calculation || !node_counter >= !node_limit) then begin
            pv_table.(0) <- !best_move;
            for i = 1 to pv_length.(1) do
              pv_table.(i) <- pv_table.(max_pv_length + i - 1)
            done;
            pv_length.(0) <- pv_length.(1) + 1
          end;
          alpha0 := max !alpha0 score;
          if score >= !beta0 then begin
          no_cut := false;
            if isquiet !best_move then begin
              history_moves.(4096 * aux_history white_to_move + 64 * from !best_move + to_ !best_move) <- depth * depth;
              killer_moves.(1) <- killer_moves.(0);
              killer_moves.(0) <- !best_move;
            end;
          end
        end;
        unmake board move;
        incr counter
      done
    end
  end;
  if not (!stop_calculation || !node_counter >= !node_limit) then begin
    store transposition_table zobrist_position Pv depth !best_score !best_move !go_counter
  end;
  !best_score