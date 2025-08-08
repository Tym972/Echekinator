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

let verif board move = match move with
  |Normal {piece; from; to_; capture} -> piece = board.(from) && board.(to_) = capture
  |Enpassant {from; to_} -> (if from < 32 then 1 else -1) = board.(from) && board.(to_) = 0
  |Castling {sort} -> if sort < 3 then 6 = board.(!from_white_king) else (-6) = board.(!from_black_king)
  |Promotion {from; to_; capture; promotion} -> (if promotion > 0 then 1 else -1) = board.(from) && board.(to_) = capture
  |Null -> false

let rec pvs board white_to_move last_move castling_right board_record half_moves depth initial_depth alpha beta evaluation zobrist_position ispv =
  incr node_counter;
  let ply = initial_depth - depth in
  if !stop_calculation || repetition board_record 3 then begin
    if ispv then begin
      pv_length.(ply) <- 0
    end;
    0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let best_score = ref (-infinity) in
    let best_move = ref Null in
    let hash_node_type, hash_depth, hash_value, hash_move =  probe transposition_table zobrist_position in
    let no_tt_cut = ref true in
    if hash_depth <> (-1) && not ispv then begin
      hash_treatment hash_node_type hash_depth hash_value hash_move depth alpha0 beta0 best_score best_move no_tt_cut ply
    end;
    if !no_tt_cut then begin
      let king_position = index_array board (king white_to_move) in
      let in_check = threatened board king_position white_to_move in
      if depth = 0 then begin
        best_score := quiescence_treatment_depth_0 initial_depth evaluation board white_to_move last_move !alpha0 !beta0 king_position in_check
      end
      else begin
        let no_cut = ref true in
        if not (in_check || ispv || !zugzwang) then begin
          if depth < 3 then begin
            let static_eval = evaluation board white_to_move king_position in_check alpha beta in
            let eval_margin = 150 * depth in
            if static_eval - eval_margin >= !beta0 then begin
              best_score := static_eval - eval_margin;
              no_cut := false
            end
          end
          else begin
            let new_zobrist = new_zobrist Null last_move zobrist_position castling_right castling_right board in
            let new_record, new_half_moves = adapt_record new_zobrist Null depth board_record half_moves in
            let score = - pvs board (not white_to_move) Null castling_right new_record new_half_moves (depth - 3) initial_depth (- !beta0) (- !alpha0) evaluation new_zobrist false
            in if score > !best_score then begin
              best_score := score;
              alpha0 := max !alpha0 score;
              if score >= !beta0 then begin
                no_cut := false;
              end
            end
          end
        end;
        let hash_ordering = !no_cut && hash_move <> Null (*&& verif board hash_move*) in
        if hash_ordering then begin
          let new_castling_right = modification_roque hash_move castling_right in
          let new_zobrist = new_zobrist hash_move last_move zobrist_position castling_right new_castling_right board in
          let new_record, new_half_moves = adapt_record new_zobrist hash_move depth board_record half_moves in
          make board hash_move;
          let score = - pvs board (not white_to_move) hash_move new_castling_right new_record new_half_moves (depth - 1) initial_depth (- !beta0) (- !alpha0) evaluation new_zobrist ispv
          in if score > !best_score then begin
            best_score := score;
            best_move := hash_move;
            if ispv then begin
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
          unmake board hash_move;
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
            let first_move = ref (not hash_ordering) in
            while (!no_cut && !moves <> []) do
              let move = List.hd !moves in
              let new_castling_right = modification_roque move castling_right in
              let new_zobrist = new_zobrist move last_move zobrist_position castling_right new_castling_right board in
              let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
              make board move;
              moves := List.tl !moves;
              let score =
                if !first_move then begin
                  first_move := false;
                  - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) initial_depth (- !beta0) (- !alpha0) evaluation new_zobrist ispv
                end
                else begin
                  let score_0 = - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) initial_depth (- !alpha0 - 1) (- !alpha0) evaluation new_zobrist false
                  in if (score_0 > !alpha0 && ispv) then begin 
                    - pvs board (not white_to_move) move new_castling_right new_record new_half_moves (depth - 1) initial_depth (- !beta0) (- !alpha0) evaluation new_zobrist true
                  end
                  else begin
                    score_0
                  end
                end 
              in if score > !best_score then begin
                best_score := score;
                best_move := move;
                if ispv then begin
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
              unmake board move
            done
          end
        end
      end
    end;
    if not !stop_calculation then begin
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
    end;
    !best_score
  end