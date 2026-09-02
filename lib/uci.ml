(*Module implémentant la communication UCI*)

open Board
open Miscellaneous
open Bitboards
open Translation
open Fen
open Move_ordering
open Quiescence
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
let lire_entree message =
  print_string message;
  flush stdout;
  input_line stdin

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

let reset_hash search_tables =
  clear !tt;
  go_counter := 0;
  for i = 0 to 8191 do
    search_tables.history_moves.(i) <- 0
  done;
  uninitialized := true

(*Fonction permettant de jouer une list de moves*)
let make_list record position =
  let rec func move_list = match move_list with
    |[] -> ()
    |string_move :: other_moves ->
      let move = mouvement_of_uci string_move position in
      if move <> 0 then begin
        make position move;
        func other_moves;
      end
  in func record

let number_of_pv = ref 1

let best_line_id = ref (-1)

(*Answer to the command "command"*)
let position_uci instructions position =
  begin match instructions with
    |"position" :: str :: _ when List.mem str ["fen"; "startpos"] -> begin
        let index_moves = ref 2 in
        let rec aux_fen list  = match list with
          |h::t when h <> "moves" ->
            begin
              incr index_moves;
              h ^ " " ^ aux_fen t
            end
          |_ -> ""
        in if str = "fen" then begin
          position_of_fen (aux_fen (pop instructions 2)) position
        end
        else begin 
          position_of_fen startpos position;
        end;
        if ((List.length instructions) > !index_moves && List.nth instructions !index_moves = "moves") then begin
          let record = (word_detection (String.concat " " (pop instructions (!index_moves + 1)))) in
          make_list record position
        end;
        legal_moves position 0
      end
    |_ -> ()
  end

(*let print_bitboard bitboard =
  let board = Array.make 64 0 in
  let rec aux_1 board index = match index with
    |[] -> ()
    |h::t ->
      board.(h) <- 6;
      aux_1 board t
  in aux_1 board (index_list bitboard);
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = board.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt"
  in output_string fichier_sortie (!display ^ "     a   b   c   d   e   f   g   h\n");
  close_out fichier_sortie
end;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")*)

let rec algoperft position depth search_ply =
  if depth = 0 then begin
    1
  end
  else begin
    legal_moves position search_ply;
    let nodes = ref 0 in
    let moves = position.moves.(search_ply) in
    for i = 0 to position.number_of_moves.(search_ply) - 1 do
      let move = moves.(i) in
      make position move;
      let perft = (algoperft position (depth - 1) (search_ply + 1)) in
      nodes := !nodes + perft;
      if search_ply = 0 then begin
        print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
      end;
      unmake position move
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
      if white_to_move = 0 then begin
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

let pv_finder position bestmove depth =
  let pv = ref [bestmove] in
  let rec aux position d =
    if d > 0 && not (position.state_array.(position.game_ply).half_moves = 100 || repetition position.state_array position.game_ply (*depth - d*)) then begin
      let state = position.state_array.(position.game_ply) in
      let _, _, _, hash_move, _ = probe state.zobrist in
      if hash_move <> 0 then begin
        make position hash_move;
        pv := hash_move :: !pv;
        aux position (d - 1);
        unmake position hash_move
      end
    end
  in make position bestmove; 
  aux position (depth - 1);
  unmake position bestmove;
  List.rev !pv 

let iterative_deepening position search_tables depth mate thread =
  let var_depth = ref 0 in 
  let var_mate = ref max_int in
  let alpha_table = Array.make !number_of_pv (- max_int) in
  let beta_table = Array.make !number_of_pv max_int in
  stop_search.(thread) <- false;
  while not (stop_search.(thread) || (thread = 0 && Mtime.Span.compare (Mtime_clock.count !start_time) !soft_bound > 0) || !var_depth + 1 > depth || total_counter node_counter + 1 > !node_limit || !var_mate < mate + 1 ) do
    incr var_depth;
    (*move_ordering search_tables position position.moves.(0) position.number_of_moves.(0) 0 0 search_tables.ordering_array.(0);*)
    for multi = 0 to (!number_of_pv - 1) do
      let new_score =
        let score = ref (pvs position search_tables thread multi !var_depth 0 alpha_table.(multi) beta_table.(multi) true) in
        while not (stop_search.(thread) || total_counter node_counter > !node_limit || (!score > alpha_table.(multi) && !score < beta_table.(multi))) do
          if !score <= alpha_table.(multi) then begin
            alpha_table.(multi) <- (-max_int)
          end
          else if !score >= beta_table.(multi) then begin
            beta_table.(multi) <- max_int
          end;
          score := pvs position search_tables thread multi !var_depth 0 alpha_table.(multi) beta_table.(multi) true;
        done;
        !score
      in if new_score > (-max_int) then begin
        if (new_score > alpha_table.(multi) && new_score < beta_table.(multi)) then begin
          alpha_table.(multi) <- new_score - 25;
          beta_table.(multi) <- new_score + 25
        end;
        if !number_of_pv > multi + 1 then begin
          (*for index = 0 to number_of_moves_copy.(0) - 1 do
            if pv_table.(0) = moves_copy.(index) then begin
              moves_copy.(index) <- moves_copy.(number_of_moves_copy.(0) - 1);
              number_of_moves_copy.(0) <- number_of_moves.(0) - 1;
            end
          done*)
        end
      end
    done;
    if thread = 0 then begin
      let exec_time =
        let span = Mtime_clock.count !start_time in
        Mtime.Span.to_float_ns span /. 1e9
      in let nps = int_of_float (float_of_int (total_counter node_counter) /. exec_time) in
      let hashfull = min 1000 (int_of_float (1000. *. (float_of_int (total_counter transposition_counter) /. (Int64.to_float !slots)))) in
      let time =  (int_of_float (1000. *. exec_time)) in
      let order_of_multi = ref [] in
      for multi = 0 to !number_of_pv - 1 do
        if !results.(multi).depth = !var_depth then begin
          order_of_multi := (!results.(multi).score, multi) :: !order_of_multi
        end
      done;
      order_of_multi := merge_sort !order_of_multi;
      begin try
        best_line_id := snd (List.hd !order_of_multi) with _ -> ()
      end;
      let rec printer variations already_printed = match variations with
        |[] -> ()
        |(_, multi) :: other_variations ->
          let score = formate_score !results.(multi).score var_mate alpha_table.(multi) beta_table.(multi) in
          let pv = (String.concat " " (List.map uci_of_mouvement (pv_finder position !results.(multi).bestmove depth))) in
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

let domain_loop position search_tables thread_id =
  let my_job = ref (-1) in
  while thread_id < !threads_number do
    Mutex.lock domain_mutex;
      while not !work_available || (!current_job = !my_job) do
        Condition.wait domain_cond domain_mutex
      done;
      my_job := !current_job;
    Mutex.unlock domain_mutex;
    iterative_deepening (copy_position position) (copy_search_tables search_tables) max_depth (-1) thread_id;
    Mutex.lock domain_mutex;
      decr jobs_remaining;
      if !jobs_remaining = 0 then begin
        work_available := false;
        Condition.signal domain_cond
      end;
    Mutex.unlock domain_mutex;
  done

let setoption position search_tables instructions =
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
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash search_tables
    |"name" :: "MultiPV" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !multipv then begin
        type_spin value multipv min_multipv max_multipv;
        results := (Array.init !multipv (fun _ ->  {depth = 0; score = 0; bestmove = 0}))
        end
    |"name" :: "Hash" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !hash_size then begin
        type_spin value hash_size min_hash_size max_hash_size;
        slots := Int64.of_int ((!hash_size * 1024 * 1024) / entry_size);
        tt := create_tt (Int64.to_int !slots);
      end
    |"name" :: "Threads" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !threads_number then begin
        let old_value = !threads_number in
        type_spin value threads_number min_threads_number max_threads_number;
        if value > old_value then begin
          domains := Array.init (!threads_number - old_value) (fun id ->
            Domain.spawn (fun () -> domain_loop position search_tables (id + old_value))
          )
        end
      end
    |_ -> ()

(*Answer to the command "go"*)
let go instructions position search_tables =
  if position.number_of_moves.(0) = 0 then begin
    let result = if true then "mate" else "cp" in
    print_endline (Printf.sprintf "info depth 0 score %s 0" result);
    print_endline "bestmove (none)"
  end
  else begin
    start_time := Mtime_clock.counter ();
    soft_bound := Mtime.Span.max_span;
    hard_bound := Mtime.Span.max_span;
    for thread = 0 to !threads_number - 1 do
      node_counter.(thread) <- 0
    done;
    for i = 0 to (2 * max_depth) - 1 do
      search_tables.killer_moves.(i) <- 0
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
      let index = ref 0 in
      let rec func move_list = match move_list with
        |uci_move :: other_moves ->
          let move = try mouvement_of_uci uci_move position with _ -> 0 in
          if move_array_mem move position.moves.(0) position.number_of_moves.(0) then begin
            position.moves.(0).(!index) <- move;
            incr index;
            func other_moves
          end;
        |_ -> ()
      in func list;
      position.number_of_moves.(0) <- !index
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
    number_of_pv := min !multipv position.number_of_moves.(0);
    results := (Array.init !multipv (fun _ ->  {depth = 0; score = 0; bestmove = 0}));
    if !threads_number > 1 then begin
      Mutex.lock domain_mutex;
        incr current_job;
        jobs_remaining := !threads_number - 1;
        work_available := true;
        Condition.broadcast domain_cond;
      Mutex.unlock domain_mutex
    end;
    iterative_deepening position search_tables !depth !mate 0;
    for thread = 1 to !threads_number - 1 do
      stop_search.(thread) <- true
    done;
    if !best_line_id = (-1) then begin
      print_endline ("info depth 0 score cp 0" ^ "\n" ^ "bestmove (none)");
    end
    else begin
      let print_bestmove = "bestmove " ^ try (uci_of_mouvement (!results.(!best_line_id).bestmove)) with _ -> "(none)" in
      let print_ponder = (*try (" ponder " ^ uci_of_mouvement (List.nth !results.(!best_line_id).pv 1)) with _ ->*) "" in
      print_endline (print_bestmove ^ print_ponder)
    end
  end

let checkers position =
  let checkers = ref "" in
  let white_to_move = position.white_to_move in
  let total_occupancy = (position.occupancy.(0) ||| position.occupancy.(1)) in
  let king_position = (lsb_index position.pieces.(pieces_rep.(white_to_move).(king))) in
  let attackers = ref (get_all_attackers king_position position.pieces total_occupancy &&& position.occupancy.(white_to_move lxor 1)) in
  while !attackers <> 0L do
    let to_, other_atatckers = pop_lsb !attackers in
    checkers := !checkers ^ coord.(to_) ^ " ";
    attackers := other_atatckers
  done;
  !checkers

let display position =
  print_board position.board;
  print_endline (Printf.sprintf "Fen: %s" (fen position));
  print_endline (Printf.sprintf "Key: %LX" position.state_array.(0).zobrist);
  print_endline (Printf.sprintf "Checkers: %s" (checkers position))

(*Fonction lançant le programme*)
let echekinator () =
  let position = create_position () in
  let search_tables = create_search_tables () in
  position_uci ["position"; "startpos"] position;
  uninitialized := true;
  print_endline (project_name ^ " by Timothée Fixy");
  let exit = ref false in
  let hot_command = Mutex.create () in
  let process instruction =
    Mutex.protect hot_command instruction
  in while not !exit do
    let instructions = word_detection (lire_entree "") in
    match instructions with
      |"uci" :: _ -> uci ()
      |"isready" :: _ -> print_endline "readyok"
      |"setoption" :: _ -> process (fun () -> setoption position search_tables instructions)
      |"ucinewgame" :: _ -> process (fun () -> reset_hash search_tables)
      |"position" :: _ -> process (fun () -> position_uci instructions position)
      |"go" :: "perft" :: depth :: _ when is_integer_string depth ->
        print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft position (int_of_string depth) 0)));
      |"go" :: _ ->
        let _ = Thread.create
          (fun () -> process (fun () -> go instructions position search_tables)) ()
        in ()
      |"quit" :: _ -> exit := true
      |"stop" :: _ ->
        for thread = 0 to !threads_number - 1 do
          stop_search.(thread) <- true
        done;
      |"d" :: _ -> display position
      |"eval" :: _ ->
        (*for i = 0 to position.number_of_moves.(0) - 1 do
          print_endline (Printf.sprintf "%s : see %i" (uci_of_mouvement position.moves.(0).(i)) (see position position.moves.(0).(i)))
        done;*)
        let eval =
          if position.white_to_move = 0 then
            (float_of_int (hce position)) /. 100.
          else
            -. (float_of_int (hce position)) /. 100.
        in print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval ^ " (white side)")
      |"ponderhit" :: _ ->
        start_time := Mtime_clock.counter ();
        time_management !wtime !btime !winc !binc !movetime position.white_to_move !movestogo soft_bound hard_bound
      |[] -> ()
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." (List.hd instructions))
  done