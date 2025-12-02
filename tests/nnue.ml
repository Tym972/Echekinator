open Libs.Board
open Libs.Generator
open Libs.Zobrist
open Libs.Uci
open Libs.Fen
open Libs.Traduction
open Libs.Evaluation
open Libs.Quiescence
open Libs.Transposition

let j = ref 0

let nodes_to_search = 50000

let is_quiet_position board white_to_move in_check provisional_score =
  let quiet = ref true in
  let m1 = 60 in
  let m2 = 70 in
  if in_check then begin
    quiet := false
  end
  else begin
    let static_eval = hce board white_to_move in
    if abs (static_eval - quiescence_search 0 0 0 (-max_int) max_int true ) > m1 then begin
      quiet := false
    end
    else begin
      let score =
        if provisional_score > (-max_int) then begin
          provisional_score
        end
        else begin
          (*position fen
          go nodes nodes_to_search
          flush stdout
          let score = in
          *)
          972
        end
      in if abs (static_eval - score) > m2 then begin
        quiet := false
      end
    end;
  end;
  if !quiet then
    incr j;
  !quiet

let func_scores eval_string white_to_move =
  if eval_string.[1] = 'M' then begin
    10000 * (if eval_string.[0] = '-' then -1 else 1) * (if white_to_move then 1 else -1)
  end
  else begin
    if white_to_move then begin
      int_of_float (100. *. float_of_string (eval_string))
    end
    else begin
      int_of_float (-. 100. *. float_of_string (eval_string))
    end
  end

(*let exploit_position white_to_move last_move castling_rights king_position in_check zobrist_position board_record half_moves =
  let legal_moves, number_of_legal_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in
  for i = 0 to !number_of_legal_moves - 1 do
    let move = legal_moves.(i) in

    make board move;
    
    if is_quiet_position board !white_to_move !in_check (-max_int) then begin
      let a = Printf.sprintf "%s | %i | %s" initial_fen score result in
      print_endline a
    end;

    unmake board move
  done*)

let select_position moves_string evals initial_fen result =
  let white_to_move = ref true in
  let last_move = ref Null in
  let castling_rights = ref (true, true, true, true) in
  let king_position = ref !from_white_king in
  let in_check = ref false in
  let moves_record = ref [] in
  let zobrist_position = ref (zobrist chessboard true Null (true, true, true, true)) in
  let board_record = ref [!zobrist_position] in
  let half_moves = ref 0 in
  let board = boards.(0) in
  position_of_fen initial_fen board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves;
  let moves = move_list_of_algebric_list (List.map remove moves_string) !white_to_move !last_move !castling_rights board in
  let score = func_scores (List.hd evals) !white_to_move in
  if is_quiet_position board !white_to_move !in_check score then begin
    let a = Printf.sprintf "%s | %i | %s" initial_fen score result in
    print_endline a
  end;
  let rec func move_list eval_list = match move_list, eval_list with
    |move :: other_moves, eval_string :: other_evals ->
      let new_castling_rights = castling_modification move !castling_rights in
      zobrist_position := new_zobrist move !last_move !zobrist_position !castling_rights new_castling_rights board;
      make board move;
      board_record := new_board_record move board_record !zobrist_position half_moves;
      castling_rights := new_castling_rights;
      last_move := move;
      white_to_move := not !white_to_move;
      king_position := index_array board (king !white_to_move);
      in_check := threatened board !king_position !white_to_move;
      moves_record := move :: !moves_record;
      let fen_position = fen board !white_to_move !last_move !castling_rights !moves_record !half_moves in
      let score = func_scores eval_string !white_to_move in
      if is_quiet_position board !white_to_move !in_check score then begin
        let b = Printf.sprintf "%s | %i | %s" fen_position score result in
        print_endline b
      end;
      func other_moves other_evals
    |_ -> ()
  in func moves (List.tl evals)

let extract data line =
  try
    let _ = Str.search_forward (Str.regexp (data ^ {| "\([^"]*\)"|})) line 0 in
    Str.matched_group 1 line
  with _ -> ""

let extract_all regex line =
  let rec loop pos acc =
    try
      let _ = Str.search_forward regex line pos in
      let g = Str.matched_group 1 line in
      loop (Str.match_end ()) (g :: acc)
    with _ -> List.rev acc
  in loop 0 []

let extract_moves line =
  extract_all (Str.regexp {|\([KQRBN]?[a-h]?[1-8]?x?[a-h][1-8]\(=[QRBN]\)?\|[a-h][1-8][a-h][1-8][qrbn]?\|O-O-O\|O-O\)|}) line

let extract_evals line =
  extract_all (Str.regexp {|{\(\([+-]?[0-9]*\.[0-9]+\)\|\([+-]M[0-9]+\)\)/[0-9][0-9]*[^}]*}|}) line

let process_pgn_file filename =
  let ic = open_in filename in
  let rec read_games () =
    try
      let line = input_line ic in
      (* Détecte le début d'une partie *)
      if String.starts_with ~prefix:"[Event" line then
        process_game ic
      else
        read_games ()
    with End_of_file -> close_in ic
  
  and process_game ic =
    let fen = ref "" in
    let result = ref "" in
    let moves = ref [] in
    let evals = ref [] in
    
    (* Lire les métadonnées *)
    let rec read_headers () =
      try
        let line = input_line ic in

        (*Fen reading*)
        if String.starts_with ~prefix:"[FEN" line then begin
          fen := extract "FEN" line;
          read_headers ()
        end

        (*Result reading*)
        else if String.starts_with ~prefix:"[Result" line then begin
          result := extract "Result" line;
          read_headers ()
        end

        (*Beginning of the moves*)
        else if line = "" then begin
          read_moves ()
        end
        
        (*Continue reading headers*)
        else begin
          read_headers ()
        end
        
      with End_of_file -> ()
    
    and read_moves () =
      try
        let line = input_line ic in

        (*Moves reading*)
        if line <> "" then begin
          moves := !moves @ (extract_moves line);
          evals := !evals @ (extract_evals line);
          read_moves ()
        end
        
        (*Next game*)
        else begin
          read_games ()
        end

      with End_of_file -> close_in ic
    in
    read_headers ();
    select_position !moves !evals !fen !result ;
  in
  read_games ()

let () =
  if false then begin
    process_pgn_file "/home/tym972/Echekinator/Results/Pgn_fastchess.pgn";
    print_endline (string_of_int !j)
  end
  else if false then begin
    let size_in_words x = Obj.size (Obj.repr x)  (* nombre de mots *) in
    let bytes = size_in_words !transposition_table.(1) * Sys.word_size / 8 in
    print_endline (string_of_int bytes);
  end
  else begin
        (* Travail CPU lourd, purement arithmétique *)
    let work n =
      let r = ref 0 in
      for i = 1 to n do
        r := !r + (i land 1)
      done;
      !r

    (* Mesure du temps en secondes *)
    in let time f =
      let t0 = Unix.gettimeofday () in
      let r = f () in
      (Unix.gettimeofday () -. t0), r

    (* Test : 1 domain vs 2 domains *)
    in let test_parallel () =
      let n = 500_000_000 in
      Printf.printf "Testing parallelism with n = %d iterations\n%!" n;

      (* Test séquentiel *)
      let t_seq, _ =
        time (fun () ->
          let _ = work n in
          let _ = work n in
          ()
        )
      in

      Printf.printf "Sequential time: %.3f s\n%!" t_seq;

      (* Test parallèle *)
      let t_par, _ =
        time (fun () ->
          let d = Domain.spawn (fun () -> work n) in
          let _ = work n in
          Domain.join d
        )
      in

      Printf.printf "Parallel time:   %.3f s\n%!" t_par;
      Printf.printf "Speed-up:        %.2fx\n%!" (t_seq /. t_par);
      ()
    in test_parallel ()

  end

let accumulator,n,hidden_weights = [||],0,[||]
let update_acc move = match move with
  |Normal {piece; from; to_; capture} -> begin
    if piece > 0 then begin
      let idx_to = 12 * to_ + (piece - 1) in
      let idx_from = 12 * from + (piece - 1) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - piece) in
      let idx_from = 12 * from + (5 - piece) in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (capture - 1)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done 
      end
    end
  end
  |Castling {sort} -> begin
    match sort with
    |1 ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 749) +. hidden_weights.(i * 768 + 735) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_short_white_rook)
      done
    |2 ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 701) +. hidden_weights.(i * 768 + 711) -. hidden_weights.(i * 768 + !zobrist_from_white_king) -. hidden_weights.(i * 768 + !zobrist_from_long_white_rook)
      done
    |3 ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 83) +. hidden_weights.(i * 768 + 69) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_short_black_rook)
      done
    |_ ->
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + 35) +. hidden_weights.(i * 768 + 45) -. hidden_weights.(i * 768 + !zobrist_from_black_king) -. hidden_weights.(i * 768 + !zobrist_from_long_black_rook)
      done
  end
  |Enpassant {from; to_} -> begin
    if from < 32 then begin
      let idx_to = 12 * to_ in
      let idx_from = 12 * from in
      let idx_capture = 12 * (to_ + 8) + 6 in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
    else begin
      let idx_to = 12 * to_ + 6 in
      let idx_from = 12 * from + 6 in
      let idx_capture = 12 * (to_ - 8) in
      for i = 0 to n - 1 do
        accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
      done
    end
  end
  |Promotion {from; to_; promotion; capture} -> begin
    if to_ < 8 then begin
      let idx_to = 12 * to_ + (promotion - 1) in
      let idx_from = 12 * from in
      if capture = 0 then begin
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = (12 * to_ + (5 - capture)) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
    else begin
      let idx_to = 12 * to_ + (5 - promotion) in
      let idx_from = 12 * from + 6 in
      if capture = 0 then begin 
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from)
        done
      end
      else begin
        let idx_capture = 12 * to_ + (capture - 1) in
        for i = 0 to n - 1 do
          accumulator.(i) <- accumulator.(i) +. hidden_weights.(i * 768 + idx_to) -. hidden_weights.(i * 768 + idx_from) -. hidden_weights.(i * 768 + idx_capture)
        done
      end
    end
  end
  |Null -> ()


let _ = is_quiet_position, update_acc, func_scores, nodes_to_search