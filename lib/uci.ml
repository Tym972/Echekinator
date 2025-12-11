(*Module implémentant la communication UCI*)

open Board
open Generator
open Zobrist
open Traduction
open Fen
open Move_ordering
open Transposition
open Search
open Evaluation

(*Supprime les n premiers éléments d'une list*)
let rec pop list n =
  if n = 0 then begin
    list
  end
  else begin
    match list with
      |[] -> []
      |_ :: t -> pop t (n - 1)
  end

(*Fonction permettant la lecture d'une réponse*)
let lire_entree message suppression =
  print_string message;
  flush stdout;
  let entree = input_line stdin in
  if suppression then
    remove entree
  else
    entree

(*Fonction permettant de jouer une list de moves*)
let make_list record position king_position in_check moves_record =
  let rec func move_list control = match move_list with
    |[] -> ()
    |string_move :: other_moves when !control ->
      let player_legal_moves, number_of_legal_moves = (legal_moves position !king_position !in_check) in
      let move = tolerance position string_move player_legal_moves !number_of_legal_moves in
      if move <> Null then begin
        make position move;
        king_position := index_array position.board (king position.white_to_move);
        in_check := threatened position!king_position;
        moves_record := move :: !moves_record;
        func other_moves control
      end
      else if move = Null then begin
        func other_moves control
      end
      else begin
        control := false
      end
    |_ -> ()
  in func record (ref true)

(*Answer to the command "uci"*)
let uci () =
  print_endline (
    "id name " ^ project_name ^ "\n"
    ^ "id author Timothée Fixy" ^ "\n"
    ^ "\n"
    ^ "option name Clear Hash type button" ^ "\n"
    ^ "option name Hash type spin default 16 min 1 max 33554432" ^ "\n"
    ^ "option name MultiPV type spin default 1 min 1 max 256" ^ "\n"
    ^ "option name Ponder type check default false" ^ "\n"
    ^ "option name Threads type spin default 1 min 1 max 1024" ^ "\n"
    ^ "option name UCI_Chess960 type check default false" ^ "\n"
    ^ "uciok")

(*Variable indication if Pondering is allowed*)
let option_ponder = ref false

let wtime = ref (-. 1.)
let btime = ref (-. 1.)
let winc = ref 0.
let binc = ref 0.
let movestogo = ref 500.
let movetime = ref (9. *. 10e8)

(*Variables MultiPV*)
let multipv = ref 1
let min_multipv = 1
let max_multipv = 256

let reset_hash () =
  clear ();
  go_counter := 0;
  for i = 0 to 8191 do
    history_moves.(i) <- 0
  done

let setoption instructions =
  let type_check instructions boolean =
    match instructions with
    |_ :: _ :: _ :: "value" :: value :: _ -> begin try boolean := (bool_of_string value) with _ -> () end
    |_ -> ()
  in 
  let type_spin instructions variable min_value max_value =
    match instructions with
    |_ :: _ :: _ :: "value" :: value :: _ ->
      let value = try int_of_string value with _ -> !variable
      in if min_value <= value && value <= max_value then begin
        variable := value
      end
    |_ -> ()
  in match (List.tl instructions) with
    |"name" :: "Ponder" :: _ -> type_check instructions option_ponder
    |"name" :: "UCI_Chess960" :: _ -> type_check instructions chess_960
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash ()
    |"name" :: "MultiPV" :: _ -> type_spin instructions multipv min_multipv max_multipv
    |"name" :: "Hash" :: _ ->
      type_spin instructions hash_size min_hash_size max_hash_size;
      slots := (!hash_size * 1024 * 1024) / entry_size;
      transposition_table := Array.make !slots empty_entry;
    |"name" :: "Threads" :: _ -> type_spin instructions threads_number min_threads_number max_threads_number
    |_ -> ()

  let reset_position position king_position in_check moves_record =
    for i = 0 to 63 do
      position.board.(i) <- chessboard.(i);
    done;
    position.white_to_move <- true;
    position.ep_square <- (-1);
    position.castling_rights <- {
      white_short = true;
      white_long = true;
      black_short = true;
      black_long = true
    };
    position.board_record <- [zobrist_chessboard];
    position.half_moves <- 0;
    position.zobrist_position <- zobrist_chessboard;
    king_position := !from_white_king;
    in_check := false;
    moves_record := []

(*Answer to the command "ucinewgame"*)
let ucinewgame position king_position in_check moves_record =
  reset_hash ();
  reset_position position king_position in_check moves_record
  

(*Answer to the command "command"*)
let position_uci instructions position king_position in_check moves_record =
  begin match instructions with
    |"position" :: str :: _ when List.mem str ["fen"; "startpos"] -> begin
        reset_position position king_position in_check moves_record;
        let index_moves = ref 2 in
        let rec aux_fen list  = match list with
        |h::t when h <> "moves" ->
          begin
            incr index_moves;
            h ^ " " ^ aux_fen t
          end
        |_ -> ""
        in if str = "fen" then begin
          position_of_fen (aux_fen (pop instructions 2)) position king_position in_check moves_record;
        end;
        if ((List.length instructions) > !index_moves && List.nth instructions !index_moves = "moves") then begin
          let record = (algebric_list_of_san (String.concat " " (pop instructions (!index_moves + 1))))in
          make_list record position king_position in_check moves_record
        end
      end
    |_ -> ()
  end

(*Fonction mettant en forme le score retourné*)
let score score var_mate =
  if abs score < 99000 then begin
    Printf.sprintf "cp %i" score
  end
  else begin
    if score mod 2 = 0 then begin
      var_mate := (((99999 - score) / 2) + 1);
      Printf.sprintf "mate %i" !var_mate
    end
    else begin
      var_mate := (((99999 + score) / 2));
      Printf.sprintf "mate -%i" !var_mate
    end
  end

let rec algoperft (position : position) depth ply =
  if depth = 0 then begin
    1
  end
  else begin
    let king_position = index_array position.board (king position.white_to_move) in
    let in_check = threatened position king_position in
    let moves, number_of_moves = legal_moves position king_position in_check in
    let nodes = ref 0 in
    for i = 0 to !number_of_moves - 1 do
      let move = moves.(i) in
      let undo_info = {
        ep_square = position.ep_square;
        castling_rights = {
          white_short = position.castling_rights.white_short;
          white_long = position.castling_rights.white_long;
          black_short = position.castling_rights.black_short;
          black_long = position.castling_rights.black_long;
        };
        board_record = position.board_record;
        half_moves = position.half_moves;
        zobrist_position = position.zobrist_position;
        last_capture = position.last_capture
      }
      in make position move;
      let perft = (algoperft position (depth - 1) (ply + 1)) in
      nodes := !nodes + perft;
      if ply = 0 then begin
        print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
      end;
      unmake position undo_info move
    done;
    !nodes
  end

let span_of_milliseconds (s : float) : Mtime.span =
  match Mtime.Span.of_float_ns (s *. 1e6) with
  | Some span -> span
  | None -> failwith "Harry Diboula"

let time_management wtime btime winc binc movetime white_to_move movestogo soft_bound hard_bound =
  let soft_bound_ms, hard_bound_ms =
    if wtime < 0. && btime < 0. then begin
      movetime, movetime
    end
    else begin
      if white_to_move then begin
        (wtime /. (min movestogo 22.)) +. winc /. 2., (wtime /. (min movestogo 18.)) +. winc /. 2.
      end
      else begin
        (btime /. (min movestogo 22.)) +. binc /. 2., (btime /. (min movestogo 18.)) +. binc /. 2.
      end
    end
  in soft_bound := span_of_milliseconds soft_bound_ms;
  hard_bound := span_of_milliseconds hard_bound_ms
  
(*Answer to the command "go"*)
let go instructions position king_position in_check =
  match instructions with
    |_ :: "perft" :: depth :: _ when is_integer_string depth ->
      print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft position (int_of_string depth) 0)));
    |_ ->
      let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
      let legal_moves, number_of_legal_moves = legal_moves position king_position in_check in
      if !number_of_legal_moves = 0 then begin
        let result = if in_check then "mate" else "cp" in
        print_endline (Printf.sprintf "info depth 0 score %s 0" result);
        print_endline "bestmove (none)"
      end
      else begin
        start_time := Mtime_clock.counter ();
        soft_bound := Mtime.Span.max_span;
        hard_bound := Mtime.Span.max_span;
        let ordering_tables = {
          killer_moves = killer_moves;
          history_moves = history_moves
        }
        in for thread = 0 to !threads_number - 1 do
          node_counter.(thread) <- 0
        done;
        for i = 0 to (2 * max_depth) - 1 do
          killer_moves.(i) <- Null
        done;
        incr go_counter;
        let is_pondering = ref false in
        wtime := (-. 1.);
        btime := (-. 1.);
        winc := 0.;
        binc := 0.;
        movestogo := 500.;
        movetime := (9. *. 10e8);
        let depth = ref max_depth in
        let mate = ref (-1) in
        let aux_searchmoves list =
          let index = ref 0 in
          let new_moves = Array.make !number_of_legal_moves Null in
          let control = ref true in
          let rec func moves = match moves with
            |uci_move :: other_moves when !control ->
              let move = tolerance position uci_move legal_moves !number_of_legal_moves in
              if move_array_mem move legal_moves !number_of_legal_moves then begin
                new_moves.(!index) <- move;
                incr index;
              end
              else if List.mem uci_move commands then begin
                control := false
              end;
              func other_moves
            |_ -> ()
          in func list;
          number_of_legal_moves := !index;
          for i = 0 to !index - 1 do
            legal_moves.(i) <- new_moves.(i)
          done
        in let rec aux instruction = match instruction with
          |h :: g :: t ->
            begin match h with
              |"searchmoves" -> aux_searchmoves (g :: t)
              |"ponder" -> is_pondering := true
              |"wtime" -> wtime := (float_of_string g)
              |"btime" -> btime := (float_of_string g)
              |"winc" -> winc := (float_of_string g)
              |"binc" -> binc := (float_of_string g)
              |"movestogo" -> movestogo := (float_of_string g)
              |"depth" -> depth := (int_of_string g)
              |"nodes" -> node_limit := (int_of_string g)
              |"mate" -> mate := (int_of_string g)
              |"movetime" -> movetime := (float_of_string g)
              |_ -> ()
            end;
            aux (g :: t)
          |_ -> ()
        in aux instructions;
        if not !is_pondering then begin
          time_management !wtime !btime !winc !binc !movetime position.white_to_move !movestogo soft_bound hard_bound
        end;
        pv_table.(0) <- (let _, _, _, move(*, _*) = probe !transposition_table position in move);
        let number_of_pv = min !multipv !number_of_legal_moves in
        let print_score_table = Array.make number_of_pv "cp 0" in
        let short_pv_table = Array.make (!threads_number * number_of_pv) [] in
        let info_table = Array.make number_of_pv ((-1), 0, 0) in
        let info = ref [] in
        let iterative_deepening position ordering_tables thread =
          let var_depth = ref 0 in 
          let var_mate = ref max_int in
          let alpha_table = Array.make number_of_pv (- max_int) in
          let beta_table = Array.make number_of_pv max_int in
          while not (stop_search.(thread) || (thread = 0 && Mtime.Span.compare (Mtime_clock.count !start_time) !soft_bound > 0) || !var_depth + 1 > !depth || total_counter node_counter + 1 > !node_limit || !var_mate < !mate + 1 ) do
            incr var_depth;
            let legal_moves_copy = (Array.copy legal_moves) in
            let number_of_legal_moves_copy = (ref !number_of_legal_moves) in
            for multi = 0 to (number_of_pv - 1) do
              let first_move =
                let acc = ref Null in
                let counter = ref 0 in
                while !acc = Null && !counter < number_of_pv do
                  let candidate = try List.hd short_pv_table.(thread * !threads_number + !counter) with _ -> Null in
                  if move_array_mem candidate legal_moves_copy !number_of_legal_moves_copy then begin
                    acc := candidate
                  end;
                  incr counter
                done;
                !acc
              in let new_score =
                let score = ref (root_search position ordering_tables thread in_check !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy legal_moves_copy) (ref !number_of_legal_moves_copy) short_pv_table multi number_of_pv) in
                while not (stop_search.(thread) || total_counter node_counter > !node_limit || (!score > alpha_table.(multi) && !score < beta_table.(multi))) do
                  if !score <= alpha_table.(multi) then begin
                    alpha_table.(multi) <- (-max_int)
                  end
                  else if !score >= beta_table.(multi) then begin
                    beta_table.(multi) <- max_int
                  end;
                  score := root_search position ordering_tables thread in_check !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy legal_moves_copy) (ref !number_of_legal_moves_copy) short_pv_table multi number_of_pv;
                done;
                !score
              in if new_score > (-max_int) then begin
                let bound =
                  if new_score <= alpha_table.(multi) then begin
                    " upperbound"
                  end
                  else if new_score >= beta_table.(multi) then begin
                    " lowerbound"
                  end
                  else begin
                    ""
                  end
                in
                alpha_table.(multi) <- new_score - 25;
                beta_table.(multi) <- new_score + 25;
                if thread = 0 then begin
                  print_score_table.(multi) <- score new_score var_mate ^ bound;
                  info_table.(multi) <- !var_depth, new_score, multi
                end;
                if number_of_pv > multi + 1 then begin
                  for index = 0 to !number_of_legal_moves_copy - 1 do
                    if pv_table.(0) = legal_moves_copy.(index) then begin
                      remove_move index legal_moves_copy number_of_legal_moves_copy
                    end
                  done
                end
              end
            done;
            if thread = 0 then begin
              info := merge_sort (Array.to_list info_table);
              let exec_time =
                let span = Mtime_clock.count !start_time in
                Mtime.Span.to_float_ns span /. 1e9
              in let nps = int_of_float (float_of_int (total_counter node_counter) /. exec_time) in
              let hashfull = min 1000 (int_of_float (1000. *. (float_of_int (total_counter transposition_counter) /. (float_of_int !slots)))) in
              let time =  (int_of_float (1000. *. exec_time)) in
              let rec aux_info list n = match list with
                |[] -> ()
                |(depth, _, multi) :: t ->
                  if !var_depth = depth then begin
                    let score = print_score_table.(multi) in
                    let pv = (String.concat " " (List.map uci_of_mouvement short_pv_table.(thread * number_of_pv + multi))) in
                    print_endline (Printf.sprintf "info depth %i seldepth %i multipv %i score %s nodes %i nps %i hashfull %i time %i pv %s" !var_depth !var_depth n score (total_counter node_counter) nps hashfull time pv)
                  end;
                  aux_info t (n + 1)
              in aux_info !info 1;
            end
          done
        in for thread = 1 to !threads_number - 1 do
          stop_search.(thread) <- false;
          let _ = Domain.spawn (fun () -> iterative_deepening {position with board = Array.copy position.board} {killer_moves = Array.copy ordering_tables.killer_moves; history_moves = Array.copy ordering_tables.history_moves} thread) in ()
        done;
        stop_search.(0) <- false;
        iterative_deepening {position with board = Array.copy position.board} ordering_tables 0;
        for thread = 1 to !threads_number - 1 do
          stop_search.(thread) <- true
        done;
        let _, _, best_number = try List.hd !info with _ -> (0, 0, (-1)) in
        if best_number = (-1) then begin
          print_endline ("info depth 0 score cp 0" ^ "\n" ^ "bestmove (none)");
        end
        else begin
          let print_bestmove = "bestmove " ^ try (uci_of_mouvement (List.hd short_pv_table.(best_number))) with _ -> "(none)" in
          let print_ponder = try (" ponder " ^ uci_of_mouvement (List.nth short_pv_table.(best_number) 1)) with _ -> "" in
          print_endline (print_bestmove ^ print_ponder)
        end
      end

let checkers board white_to_move =
  let position_roi = (index_array board (king white_to_move)) in
  let rec aux list = match list with
    |[] -> ""
    |move :: t -> if to_ move = position_roi then coord.(from move) ^ " " ^ aux t else aux t
  in let moves,_ = (*pseudo_legal_moves board (not white_to_move) (index_array board (king (not white_to_move)))*) [],[]
  in aux moves

let display position moves_record =
  print_board position.board;
  print_endline (Printf.sprintf "Fen: %s" (fen position moves_record));
  print_endline (Printf.sprintf "Key: %i" position.zobrist_position);
  print_endline (Printf.sprintf "Checkers: %s" (checkers position.board position.white_to_move))

(*Fonction lançant le programme*)
let echekinator () =
  let king_position = ref !from_white_king in
  let in_check = ref false in
  let moves_record = ref [] in
  let position = {
    board = Array.copy chessboard;
    white_to_move = true;
    ep_square = (-1);
    castling_rights = {
      white_short = true;
      white_long = true;
      black_short = true;
      black_long = true
    };
    board_record = [zobrist_chessboard];
    half_moves = 0;
    zobrist_position = zobrist_chessboard;
    last_capture = 0
  }
  in let exit = ref false in
  let hot_command = ref false in
  let process instruction =
    while !hot_command do
      ()
    done;
    hot_command := true;
    instruction ();
    hot_command := false
  in while not !exit do
    let command_line = word_detection (lire_entree "" true) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"setoption" -> process (fun () -> setoption command_line)
      |"ucinewgame" -> process (fun () -> ucinewgame position king_position in_check moves_record)
      |"position" -> process (fun () -> position_uci command_line position king_position in_check moves_record)
      |"go" ->
        let _ = Thread.create
          (fun () -> process (fun () -> go command_line {position with board = Array.copy position.board} !king_position !in_check)) ()
        in ()
      |"quit" -> exit := true
      |"stop" -> 
        for thread = 0 to !threads_number - 1 do
          stop_search.(thread) <- true
        done;
      |"d" -> display position !moves_record
      |"eval" ->
        let eval = (float_of_int (hce {position with white_to_move = true})) /. 100. in
        print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval ^ " (white side)")
      |"ponderhit" ->
        start_time := Mtime_clock.counter ();
        time_management !wtime !btime !winc !binc !movetime position.white_to_move !movestogo soft_bound hard_bound
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." command)
  done