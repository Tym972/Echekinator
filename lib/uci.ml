(*Module implémentant la communication UCI*)

open Board
open Generator
open Zobrist
open Translation
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
let make_list record position move_counter =
  let rec func move_list = match move_list with
    |[] -> ()
    |string_move :: other_moves ->
      let player_legal_moves, number_of_legal_moves = (legal_moves position) in
      let move = tolerance position string_move player_legal_moves !number_of_legal_moves in
      if move <> Null then begin
        make position position move;
        initial_half_moves := position.half_moves;
        board_record.(!initial_half_moves) <- position.zobrist_position;
        incr move_counter;
        func other_moves
      end
  in func record

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

let reset_hash () =
  clear ();
  go_counter := 0;
  for i = 0 to 8191 do
    history_moves.(i) <- 0
  done

let boards =
  ref begin
    Array.init 1 (fun _ -> Array.copy chessboard)
  end

let stacks =
  ref begin
    Array.init 1 begin
      fun i ->
        Array.init (max_pv_length + 40)
          (fun _ ->
            { board = !boards.(i);
              white_to_move = true;
              ep_square = (-1);
              castling_rights = {
                white_short = true;
                white_long = true;
                black_short = true;
                black_long = true
              };
              half_moves = 0;
              zobrist_position = zobrist_chessboard;
              last_capture = 0;
              king_positions = {
                king_to_move = !from_white_king;
                king_not_to_move = !from_black_king
              };
              in_check = false
            })
    end
  end

let position =
  {
    board = Array.copy chessboard;
    white_to_move = true;
    ep_square = (-1);
    castling_rights = {
      white_short = true;
      white_long = true;
      black_short = true;
      black_long = true
    };
    half_moves = 0;
    zobrist_position = zobrist_chessboard;
    last_capture = 0;
    king_positions = {
      king_to_move = !from_white_king;
      king_not_to_move = !from_black_king
    };
    in_check = false
  }

let moves, number_of_moves = legal_moves position

let number_of_pv = ref 1

let best_line_id = ref (-1)

let inialize_stacks () =
  for thread = 0 to !threads_number - 1 do
    for i = 0 to 63 do
      !boards.(thread).(i) <- position.board.(i)
    done;
    !stacks.(thread).(0) <- {
      board = !boards.(thread);
      white_to_move = position.white_to_move;
      ep_square = position.ep_square;
      castling_rights = position.castling_rights;
      half_moves = position.half_moves;
      zobrist_position = position.zobrist_position;
      last_capture = position.last_capture;
      king_positions = position.king_positions;
      in_check = position.in_check
    }
  done

let reset_position position move_counter =
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
  position.half_moves <- 0;
  position.zobrist_position <- zobrist_chessboard;
  position.king_positions <- {
      king_to_move = !from_white_king;
      king_not_to_move = !from_black_king
    };
  position.in_check <- false;
  move_counter := 0;
  initial_half_moves := 0;
  board_record.(0) <- zobrist_chessboard

(*Answer to the command "ucinewgame"*)
let ucinewgame position move_counter =
  reset_hash ();
  reset_position position move_counter

(*Answer to the command "command"*)
let position_uci instructions position move_counter =
  begin match instructions with
    |"position" :: str :: _ when List.mem str ["fen"; "startpos"] -> begin
        reset_position position move_counter;
        let index_moves = ref 2 in
        let rec aux_fen list  = match list with
          |h::t when h <> "moves" ->
            begin
              incr index_moves;
              h ^ " " ^ aux_fen t
            end
          |_ -> ""
        in if str = "fen" then begin
          position_of_fen (aux_fen (pop instructions 2)) position move_counter;
        end;
        if ((List.length instructions) > !index_moves && List.nth instructions !index_moves = "moves") then begin
          let record = (algebric_list_of_san (String.concat " " (pop instructions (!index_moves + 1)))) in
          make_list record position move_counter
        end;
        inialize_stacks ();
        let new_moves, new_number_of_moves = legal_moves position in
        for i = 0 to !new_number_of_moves - 1 do
          moves.(i) <- new_moves.(i)
        done;
        number_of_moves := !new_number_of_moves
      end
    |_ -> ()
  end

let rec algoperft stack depth ply =
  if depth = 0 then begin
    1
  end
  else begin
    let position = stack.(ply) in
    let moves, number_of_moves = legal_moves position in
    let nodes = ref 0 in
    for i = 0 to !number_of_moves - 1 do
      let move = moves.(i) in
      make position stack.(ply + 1) move;
      let perft = (algoperft stack (depth - 1) (ply + 1)) in
      nodes := !nodes + perft;
      if ply = 0 then begin
        print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
      end;
      unmake position.board move stack.(ply + 1).last_capture
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

(*Fonction mettant en forme le score retourné*)
let formate_score score var_mate alpha beta =
  let bound =
    if score <= alpha then begin
      " upperbound"
    end
    else if score >= beta then begin
      " lowerbound"
    end
    else begin
      ""
    end
  in
  if abs score < 99000 then begin
    Printf.sprintf "cp %i" score ^ bound
  end
  else begin
    if score mod 2 = 0 then begin
      var_mate := (((99999 - score) / 2) + 1);
      Printf.sprintf "mate %i" !var_mate ^ bound
    end
    else begin
      var_mate := (((99999 + score) / 2));
      Printf.sprintf "mate -%i" !var_mate ^ bound
    end
  end

let iterative_deepening stack ordering_tables depth mate thread =
  let var_depth = ref 0 in 
  let var_mate = ref max_int in
  let alpha_table = Array.make !number_of_pv (- max_int) in
  let beta_table = Array.make !number_of_pv max_int in
  stop_search.(thread) <- false;
  while not (stop_search.(thread) || (thread = 0 && Mtime.Span.compare (Mtime_clock.count !start_time) !soft_bound > 0) || !var_depth + 1 > depth || total_counter node_counter + 1 > !node_limit || !var_mate < mate + 1 ) do
    incr var_depth;
    let moves_copy = (Array.copy moves) in
    let number_of_moves_copy = (ref !number_of_moves) in
    for multi = 0 to (!number_of_pv - 1) do
      let first_move =
        let acc = ref Null in
        let counter = ref 0 in
        while !acc = Null && !counter < !number_of_pv do
          let candidate = try List.hd !results.(thread).pvs.(!counter).pv with _ -> Null in
          if move_array_mem candidate moves_copy !number_of_moves_copy then begin
            acc := candidate
          end;
          incr counter
        done;
        !acc
      in let new_score =
        let score = ref (root_search stack ordering_tables thread !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy moves_copy) (ref !number_of_moves_copy) multi) in
        while not (stop_search.(thread) || total_counter node_counter > !node_limit || (!score > alpha_table.(multi) && !score < beta_table.(multi))) do
          if !score <= alpha_table.(multi) then begin
            alpha_table.(multi) <- (-max_int)
          end
          else if !score >= beta_table.(multi) then begin
            beta_table.(multi) <- max_int
          end;
          score := root_search stack ordering_tables thread !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy moves_copy) (ref !number_of_moves_copy) multi;
        done;
        !score
      in if new_score > (-max_int) then begin
        if (new_score > alpha_table.(multi) && new_score < beta_table.(multi)) then begin
          alpha_table.(multi) <- new_score - 25;
          beta_table.(multi) <- new_score + 25
        end;
        if !number_of_pv > multi + 1 then begin
          for index = 0 to !number_of_moves_copy - 1 do
            if pv_table.(0) = moves_copy.(index) then begin
              remove_move index moves_copy number_of_moves_copy
            end
          done
        end
      end
    done;
    if thread = 0 then begin
      let exec_time =
        let span = Mtime_clock.count !start_time in
        Mtime.Span.to_float_ns span /. 1e9
      in let nps = int_of_float (float_of_int (total_counter node_counter) /. exec_time) in
      let hashfull = min 1000 (int_of_float (1000. *. (float_of_int (total_counter transposition_counter) /. (float_of_int !slots)))) in
      let time =  (int_of_float (1000. *. exec_time)) in
      let order_of_multi = ref [] in
      for multi = 0 to !number_of_pv - 1 do
        if !results.(0).pvs.(multi).depth = !var_depth then begin
          order_of_multi := (!results.(0).pvs.(multi).score, multi) :: !order_of_multi
        end
      done;
      order_of_multi := merge_sort !order_of_multi;
      begin try
        best_line_id := snd (List.hd !order_of_multi) with _ -> ()
      end;
      let rec printer variations already_printed = match variations with
        |[] -> ()
        |(_, multi) :: other_variations ->
          let score = formate_score !results.(0).pvs.(multi).score var_mate alpha_table.(multi) beta_table.(multi) in
          let pv = (String.concat " " (List.map uci_of_mouvement !results.(0).pvs.(multi).pv )) in
          print_endline (Printf.sprintf "info depth %i seldepth %i multipv %i score %s nodes %i nps %i hashfull %i time %i pv %s" !var_depth !var_depth already_printed score (total_counter node_counter) nps hashfull time pv);
          printer other_variations (already_printed + 1)
      in printer !order_of_multi 1
    end;
  done

let (domains : unit Domain.t array ref) = ref [||]

let domain_mutex = Mutex.create ()
let domain_cond = Condition.create ()

let work_available = ref false
let jobs_remaining = ref 0

let current_job = ref 0

let domain_loop thread_id =
  let my_job = ref (-1) in
  while thread_id < !threads_number do
    Mutex.lock domain_mutex;
      while not !work_available || (!current_job = !my_job) do
        Condition.wait domain_cond domain_mutex
      done;
      my_job := !current_job;
    Mutex.unlock domain_mutex;
    iterative_deepening !stacks.(thread_id) {killer_moves = Array.copy killer_moves; history_moves = Array.copy history_moves} max_depth (-1) thread_id;
    Mutex.lock domain_mutex;
      decr jobs_remaining;
      if !jobs_remaining = 0 then begin
        work_available := false;
        Condition.signal domain_cond
      end;
    Mutex.unlock domain_mutex;
  done

let setoption instructions =
  let type_check instructions boolean =
    match instructions with
    |_ :: _ :: _ :: "value" :: value :: _ -> begin try boolean := (bool_of_string value) with _ -> () end
    |_ -> ()
  in
  let value_of_instructions instructions = match instructions with
    |_ :: _ :: _ :: "value" :: value :: _ -> (try int_of_string value with _ -> (-1))
    |_ -> (-1)
  in let type_spin value variable min_value max_value =
    if min_value <= value && value <= max_value then begin
      variable := value
    end
  in match (List.tl instructions) with
    |"name" :: "Ponder" :: _ -> type_check instructions option_ponder
    |"name" :: "UCI_Chess960" :: _ -> type_check instructions chess_960
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash ()
    |"name" :: "MultiPV" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !multipv then begin
        type_spin value multipv min_multipv max_multipv;
        results :=
          (Array.init !threads_number (fun _ ->
            { pvs = Array.make !multipv { depth = 0; score = 0; pv = [] } })
          )
        end
    |"name" :: "Hash" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !hash_size then begin
        type_spin value hash_size min_hash_size max_hash_size;
        slots := (!hash_size * 1024 * 1024) / entry_size;
        transposition_table := Array.make !slots empty_entry;
      end
    |"name" :: "Threads" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !threads_number then begin
        let old_value = !threads_number in
        type_spin value threads_number min_threads_number max_threads_number;
        if value > old_value then begin
          results :=
            (Array.init !threads_number (fun _ ->
              { pvs = Array.make !multipv { depth = 0; score = 0; pv = [] } })
            );
          boards := Array.init !threads_number (fun _ -> Array.copy chessboard);
          stacks :=
            Array.init !threads_number begin
              fun i ->
                Array.init (max_pv_length + 40)
                  (fun _ ->
                    { board = !boards.(i);
                      white_to_move = true;
                      ep_square = (-1);
                      castling_rights = {
                        white_short = true;
                        white_long = true;
                        black_short = true;
                        black_long = true};
                      half_moves = 0;
                      zobrist_position = 0;
                      last_capture = 0;
                      king_positions = {
                        king_to_move = !from_white_king;
                        king_not_to_move = !from_black_king
                      };
                      in_check = false
                    })
              end;
          inialize_stacks ();
          domains := Array.init (!threads_number - old_value) (fun id ->
            Domain.spawn (fun () -> domain_loop (id + old_value))
          )
        end
      end
    |_ -> ()

(*Answer to the command "go"*)
let go instructions position =
  if !number_of_moves = 0 then begin
    let result = if position.in_check then "mate" else "cp" in
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
    node_limit := max_int;
    let depth = ref max_depth in
    let mate = ref (-1) in
    let aux_searchmoves list =
      let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
      let index = ref 0 in
      let new_moves = Array.make !number_of_moves Null in
      let control = ref true in
      let rec func move_list = match move_list with
        |uci_move :: other_moves when !control ->
          let move = tolerance position uci_move moves !number_of_moves in
          if move_array_mem move moves !number_of_moves then begin
            new_moves.(!index) <- move;
            incr index;
          end
          else if List.mem uci_move commands then begin
            control := false
          end;
          func other_moves
        |_ -> ()
      in func list;
      number_of_moves := !index;
      for i = 0 to !index - 1 do
        moves.(i) <- new_moves.(i)
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
    number_of_pv := min !multipv !number_of_moves;
    results :=
      (Array.init !threads_number (fun _ ->
        {pvs = Array.make !number_of_pv {depth = 0; score = 0; pv = []}})
      );
    if !threads_number > 1 then begin
      Mutex.lock domain_mutex;
        incr current_job;
        jobs_remaining := !threads_number - 1;
        work_available := true;
        Condition.broadcast domain_cond;
      Mutex.unlock domain_mutex
    end;
    iterative_deepening !stacks.(0) ordering_tables !depth !mate 0;
    for thread = 1 to !threads_number - 1 do
      stop_search.(thread) <- true
    done;
    if !best_line_id = (-1) then begin
      print_endline ("info depth 0 score cp 0" ^ "\n" ^ "bestmove (none)");
    end
    else begin
      let print_bestmove = "bestmove " ^ try (uci_of_mouvement (List.hd !results.(0).pvs.(!best_line_id).pv)) with _ -> "(none)" in
      let print_ponder = try (" ponder " ^ uci_of_mouvement (List.nth !results.(0).pvs.(!best_line_id).pv 1)) with _ -> "" in
      print_endline (print_bestmove ^ print_ponder)
    end
  end

let checkers position =
  let squares = ref "" in
  let aux_promotion move = match move with
    |Promotion {from = _; to_ = _; promotion} when abs promotion <> 2 -> false
    |_ -> true
  in if position.in_check then begin
    position.white_to_move <- not position.white_to_move;
    let moves, number_of_moves = legal_moves position in
    for i = 0 to !number_of_moves - 1 do
      let move = moves.(i) in
      if to_ move = position.king_positions.king_to_move && aux_promotion move then begin
        squares := !squares ^ coord.(from move) ^ " "
      end;
    done;
    position.white_to_move <- not position.white_to_move;
  end;
  !squares

let display position move_counter =
  print_board position.board;
  print_endline (Printf.sprintf "Fen: %s" (fen position move_counter));
  print_endline (Printf.sprintf "Key: %i" position.zobrist_position);
  print_endline (Printf.sprintf "Checkers: %s" (checkers position))

(*Fonction lançant le programme*)
let echekinator () =
  print_endline (project_name ^ " by Timothée Fixy");
  let move_counter = ref 0 in
  let exit = ref false in
  let hot_command = Mutex.create () in
  let process instruction =
    Mutex.protect hot_command instruction
  in while not !exit do
    let instructions = word_detection (lire_entree "" true) in
    match instructions with
      |"uci" :: _ -> uci ()
      |"isready" :: _ -> print_endline "readyok"
      |"setoption" :: _ -> process (fun () -> setoption instructions)
      |"ucinewgame" :: _ -> process (fun () -> ucinewgame position move_counter)
      |"position" :: _ -> process (fun () -> position_uci instructions position move_counter)
      |"go" :: "perft" :: depth :: _ when is_integer_string depth ->
        print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft !stacks.(0) (int_of_string depth) 0)));
      |"go" :: _ ->
        let _ = Thread.create
          (fun () -> process (fun () -> go instructions {position with board = Array.copy position.board})) ()
        in ()
      |"quit" :: _ -> exit := true
      |"stop" :: _ -> 
        for thread = 0 to !threads_number - 1 do
          stop_search.(thread) <- true
        done;
      |"d" :: _ -> display position !move_counter
      |"eval" :: _ ->
        let eval = (float_of_int (hce {position with white_to_move = true})) /. 100. in
        print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval ^ " (white side)")
      |"ponderhit" :: _ ->
        start_time := Mtime_clock.counter ();
        time_management !wtime !btime !winc !binc !movetime position.white_to_move !movestogo soft_bound hard_bound
      |[] -> ()
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." (List.hd instructions))
  done