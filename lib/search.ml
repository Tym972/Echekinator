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

(*let verif board move = match move with
  |Normal {piece; from; to_; capture} -> board.(from) = piece && board.(to_) = capture
  |Enpassant {from; to_} -> board.(from) = (if from < 32 then 1 else -1)  && board.(to_) = 0
  |Castling {sort} -> if sort < 3 then board.(!from_white_king) = 6 else board.(!from_black_king) = (-6)
  |Promotion {from; to_; capture; promotion} -> board.(from) = (if promotion > 0 then 1 else -1) && board.(to_) = capture
  |Null -> false*)

(*open Evaluation*)


let rec pvs board white_to_move last_move castling_rights board_record half_moves zobrist_position depth ply alpha beta evaluation ispv =

  (*Check search limit*)
  if !out_of_time || !node_counter >= !node_limit then begin
    if ispv then begin
      pv_length.(ply) <- 0
    end;
    0
  end

  else begin
    incr node_counter;
    let king_position = index_array board (king white_to_move) in
    let in_check = threatened board king_position white_to_move in
    
    (*let bg = Array.copy accumulator in Array.blit bg 0 accumulator 0 n;*)
    (*vector board;
    if evaluate () <> make_output_layer board_vector then begin
      (*print_endline (string_of_bool (board = board_of_vector board_vector));
      print_board board; print_board (board_of_vector board_vector);*)
      print_endline (string_of_float (evaluate ()) ^ " " ^ string_of_float (make_output_layer board_vector))
    end; *)

    (*Check repetion or fifty moves rule*)
    if repetition board_record 3 || (half_moves = 100 && (not in_check || (legal_moves board white_to_move last_move castling_rights king_position in_check <> []))) then begin
      if ispv then begin
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
        let hash_node_type, hash_depth, hash_value, hash_move(*, hash_static_eval*) = probe transposition_table zobrist_position in
        let no_cut = ref true in
        let best_score = ref (-infinity) in

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
        if hash_depth <> (-1) && not ispv then begin
          hash_treatment hash_node_type hash_depth hash_value hash_move depth alpha0 beta0 best_score best_move no_cut ply
        end;

        if !no_cut then begin

          (*Quiescense search*)
          if depth = 0 then begin
            best_score := quiescence_treatment_depth_0 ply evaluation board white_to_move last_move castling_rights half_moves !alpha0 !beta0 king_position in_check;
          end

          else begin

            (*Static evalutation pruning and null move pruning*)
            if not (in_check || ispv) then begin
              let static_eval = evaluation board white_to_move in
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
                  let new_zobrist = new_zobrist Null last_move zobrist_position castling_rights castling_rights board in
                  let score = - pvs board (not white_to_move) Null castling_rights board_record half_moves new_zobrist (depth - 3) (ply + 1) (-beta) (- beta + 1) evaluation false
                  in if score >= !beta0 then begin
                    best_score := score;
                    no_cut := false;
                  end
                end
              end
            end;

            (*Move loop*)
            if !no_cut then begin
              let counter = ref 0 in
              let steelpulse = ref 0 in
              let rec move_loop move_list = match move_list with
                |move :: t when !no_cut ->
                  let new_castling_right = castling_modification move castling_rights in
                  let new_zobrist = new_zobrist move last_move zobrist_position castling_rights new_castling_right board in
                  let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
                  make board move;
                  let score =
                    if !counter = 0 then begin
                      - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation ispv
                    end
                    else begin
                      let score_lmr =
                        let reduction =
                          let float_depth = float_of_int depth in
                          let float_counter = float_of_int (!counter - !steelpulse) in
                          min
                            (int_of_float begin
                              if isquiet move then
                                1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                              else
                                0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                            end)
                            (depth - 1)
                        in if not (in_check || depth < 3 || reduction = 0) then begin
                          - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1 - reduction) (ply + 1) (- !alpha0 - 1) (- !alpha0) evaluation false
                        end
                        else
                          !alpha0 + 1
                      in if score_lmr > !alpha0 then begin
                        let score_0 = - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) (ply + 1) (- !alpha0 - 1) (- !alpha0) evaluation false
                        in if (score_0 > !alpha0 && ispv) then begin 
                          - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) (ply + 1) (- !beta0) (- !alpha0) evaluation ispv
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
                      if ispv then begin
                        pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2) <- !best_move;
                        for i = 1 to pv_length.(ply + 1) do
                          pv_table.(ply * (2 * max_pv_length + 1 - ply) / 2 + i) <- pv_table.((ply + 1) * (2 * max_pv_length - ply) / 2 + i - 1)
                        done;
                        pv_length.(ply) <- pv_length.(ply + 1) + 1
                      end
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
                  unmake board move;
                  incr counter;
                  move_loop t
                |_ -> ()
              in if hash_move <> Null then begin
                move_loop [hash_move];
                if !no_cut then begin
                  steelpulse := 1;
                  move_loop (List.filter (fun c -> c <> hash_move) (move_ordering board white_to_move (legal_moves board white_to_move last_move castling_rights king_position in_check) ply))
                end
              end
              else begin
                move_loop (move_ordering board white_to_move (legal_moves board white_to_move last_move castling_rights king_position in_check) ply)
              end;
              if !counter = 0 then begin
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
            end;
          end
        end;

        (*Storing in TT*)
        if not (!out_of_time || !node_counter >= !node_limit) then begin
          let node_type =
            if !best_score <= alpha then begin
              if ispv then begin
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
          in store transposition_table zobrist_position node_type depth stored_value !best_move (*hash_static_eval*) !go_counter
        end;
      !best_score
      end;
    end
  end

let english_pieces_lowercase = [|""; "p"; "n"; "b"; "r"; "q"; "k"|]

(*Fonction traduisant un move en sa notation UCI*)
let uci_of_mouvement move = match move with
  |Castling {sort} ->
    let from_king, to_short, to_long, from_short_rook, from_long_rook = if sort < 3 then !from_white_king, 62, 58, !from_short_white_rook, !from_long_white_rook else !from_black_king, 6, 2, !from_short_black_rook, !from_long_black_rook in
    let arrivee_roque, depart_tour = if sort mod 2 = 1 then to_short, from_short_rook else to_long, from_long_rook in
    coord.(from_king) ^ coord.(if not !chess_960 then arrivee_roque else depart_tour)
  |Promotion {from = _; to_ = _; capture = _; promotion} -> coord.(from move) ^ coord.(to_ move) ^ english_pieces_lowercase.(abs promotion)
  |Null -> "(none)"
  |_ -> coord.(from move) ^ coord.(to_ move)

let pv_finder depth =
  let pv = ref [] in
  for i = 0 to (min pv_length.(0) depth) - 1 do 
    pv := !pv @ [pv_table.(i)];
  done;
  !pv

let root_search board white_to_move last_move castling_rights board_record half_moves zobrist_position in_check depth alpha beta evaluation first_move player_moves short_pv_table multi =
  incr node_counter;
  let no_cut = ref true in
  let alpha0 = ref alpha in
  let beta0 = ref beta in
  let best_score = ref (-infinity) in
  let best_move = ref Null in
  (*let static_eval = evaluation board white_to_move in*)
  let counter = ref 0 in
  let steelpulse = ref (if first_move = Null then 0 else 1) in
  let rec move_loop move_list = match move_list with
    |move :: t when !no_cut ->
      let new_castling_right = castling_modification move castling_rights in
      let new_zobrist = new_zobrist move last_move zobrist_position castling_rights new_castling_right board in
      let new_record, new_half_moves = adapt_record new_zobrist move depth board_record half_moves in
      make board move;
      let score =
        if !counter = 0 then begin
          - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) 1 (- !beta0) (- !alpha0) evaluation true
        end
        else begin
          let score_lmr =
            let reduction =
              let float_depth = float_of_int depth in
              let float_counter = float_of_int (!counter - !steelpulse) in
              min
                (int_of_float begin
                  if isquiet move then
                    1.35 +. log (float_depth) *. log (float_counter) /. 2.75
                  else
                    0.20 +. log (float_depth) *. log (float_counter) /. 3.35
                end)
                (depth - 1)
            in if not (in_check || depth < 3 || reduction = 0) then begin
              - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1 - reduction) 1 (- !alpha0 - 1) (- !alpha0) evaluation false
            end
            else
              !alpha0 + 1
          in if score_lmr > !alpha0 then begin
            let score_0 = - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) 1 (- !alpha0 - 1) (- !alpha0) evaluation false
            in if (score_0 > !alpha0) then begin 
              - pvs board (not white_to_move) move new_castling_right new_record new_half_moves new_zobrist (depth - 1) 1 (- !beta0) (- !alpha0) evaluation true
            end
            else begin
              score_0
            end
          end
          else
            score_lmr
        end
      in if score > !best_score && not (!out_of_time || !node_counter >= !node_limit) then begin
        best_score := score;
        if score > !alpha0 || !counter = 0 then begin
          best_move := move;
          pv_table.(0) <- !best_move;
          for i = 1 to pv_length.(1) do
            pv_table.(i) <- pv_table.(max_pv_length + i - 1)
          done;
          pv_length.(0) <- pv_length.(1) + 1;
          short_pv_table.(multi) <- pv_finder depth
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
      unmake board move;
      incr counter;
      move_loop t
    |_ -> ()
  in if first_move <> Null then begin
    move_loop [first_move];
    if !no_cut then begin
      steelpulse := 1;
      move_loop (List.filter (fun c -> c <> first_move) (move_ordering board white_to_move player_moves 0))
    end
  end
  else begin
    move_loop (move_ordering board white_to_move player_moves 0)
  end;
  if not (!out_of_time || !node_counter >= !node_limit) then begin
    store transposition_table zobrist_position Pv depth !best_score !best_move (*static_eval*) !go_counter
  end;
  !best_score