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

let add_move move picker =
  if isquiet move then begin
    picker.quiet_moves.(picker.number_of_quiets) <- move;
    picker.number_of_quiets <- picker.number_of_quiets + 1
  end
  else begin
    picker.capture_moves.(picker.number_of_captures) <- move;
    picker.number_of_captures <- picker.number_of_captures + 1
  end

let remove_move move picker =
  let index = ref 0 in
  let exit = ref false in
  if isquiet move then begin
    let quiet_moves = picker.quiet_moves in
    let number_of_quiets = picker.number_of_quiets in
    while !index < number_of_quiets && not !exit do
      if quiet_moves.(!index) = move then begin
        quiet_moves.(!index) <- quiet_moves.(number_of_quiets - 1);
        picker.number_of_quiets <- number_of_quiets - 1;
        exit := true
      end
      else begin
        incr index
      end
    done;
  end
  else begin
    let capture_moves = picker.capture_moves in
    let number_of_captures = picker.number_of_captures in
    while !index < number_of_captures && not !exit do
      if capture_moves.(!index) = move then begin
        capture_moves.(!index) <- capture_moves.(number_of_captures - 1);
        picker.number_of_captures <- number_of_captures - 1;
        exit := true
      end
      else begin
        incr index
      end
    done
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

let is_pondering = ref false
let wtime = ref (-. 1.)
let btime = ref (-. 1.)
let winc = ref 0.
let binc = ref 0.
let movestogo = ref 50.
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

let current_position = ref (create_position ())
let current_search_tables = ref (create_search_tables ())

type bestline =
  {mutable id : int;
  mutable depth : int}

let bestline = {
  id = -1 ;
  depth = 0
}

(*Answer to the command "command"*)
let position_uci instructions position search_tables =
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
        legal_moves position search_tables.pickers.(0) phase_all
      end
    |_ -> ()
  end

let rec algoperft position pickers depth search_ply =
  if depth = 0 then begin
    1
  end
  else begin
    let picker = pickers.(search_ply) in
    legal_moves position picker phase_all;
    let nodes = ref 0 in
    let aux moves number_of_moves =
      for i = 0 to number_of_moves - 1 do
        let move = moves.(i) in
        make position move;
        let perft = (algoperft position pickers (depth - 1) (search_ply + 1)) in
        nodes := !nodes + perft;
        if search_ply = 0 then begin
          print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
        end;
        unmake position move
      done
    in aux picker.capture_moves picker.number_of_captures;
    aux picker.quiet_moves picker.number_of_quiets;
    !nodes
  end

let span_of_milliseconds span =
  match Mtime.Span.of_float_ns (span *. 1e6) with
  | Some span -> span
  | None -> failwith "Harry Diboula"

let miliseconds_of_span miliseconds =
  (Mtime.Span.to_float_ns miliseconds) /. 1e6 

let init_time position number_of_legal wtime btime winc binc movetime movestogo =
  if wtime < 0. && btime < 0. then begin
    soft_bound := span_of_milliseconds movetime;
    hard_bound := span_of_milliseconds movetime
  end
  else begin
    let time, inc = if position.white_to_move = 0 then wtime, winc else btime, binc in
    let base_ms = max 0. ((time /. ((min movestogo 40.) *. (if number_of_legal = 1 then 10. else 1.))) +. inc *. 0.75) in
    let hard_bound_ms = ref (min (4. *. base_ms) time) in
    if !hard_bound_ms +. 25. > time then hard_bound_ms := !hard_bound_ms -. 25.;
    if !hard_bound_ms < 1. then hard_bound_ms := 1.;
    let soft_bound_ms = ref base_ms in
    if !soft_bound_ms > !hard_bound_ms then soft_bound_ms := !hard_bound_ms;
    if !soft_bound_ms < 1. then soft_bound_ms  := 1.;
    soft_bound := span_of_milliseconds !soft_bound_ms;
    hard_bound := span_of_milliseconds !hard_bound_ms
  end

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
    if d > 0 && not (position.state_array.(position.game_ply).half_moves = 100 || repetition position.state_array position.game_ply) then begin
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
  let picker = search_tables.pickers.(0) in
  let number_of_legal = picker.number_of_captures + picker.number_of_quiets in
  let number_of_pv = min !multipv number_of_legal in
  let zobrist = position.state_array.(position.game_ply).zobrist in
  let tt_index = Int64.to_int (Int64.rem zobrist !slots) in
  let actual_depths = Array.make number_of_pv 0 in
  let bestline_id_tab = Array.make (depth + 1) (-1) in
  let base_ms = miliseconds_of_span !soft_bound in
  let stability_counter = ref 0 in
  while not (stop_search.(thread) || (thread = 0 && Mtime.Span.compare (Mtime_clock.count !start_time) !soft_bound > 0) || !var_depth + 1 > depth || total_counter node_counter + 1 > !node_limit || !var_mate < mate + 1) || !var_depth = 0 do
    incr var_depth;
    let alpha = ref (- 99999) in
    let beta = ref 99999 in
    for multi = 0 to (number_of_pv - 1) do
      if !var_depth < 2 || multi > 0 || thread > 0 then begin
        let _ = (search position search_tables thread multi !var_depth 0 (-99999) 99999 false) in ()
      end
      else begin
        let previous_score = !search_record.(bestline_id_tab.(!var_depth - 1)).(!var_depth - 1).score in
        let delta = ref (15 + ((previous_score * previous_score) / 16000)) in
        alpha := (previous_score - !delta);
        beta := (previous_score + !delta);
        let score = ref (search position search_tables thread multi !var_depth 0 !alpha !beta false) in
        while not (stop_search.(thread) || total_counter node_counter > !node_limit || (!score > !alpha && !score < !beta)) do
          if !score <= !alpha then begin
            beta := (!alpha + !beta) / 2;
            alpha := !alpha - !delta
          end
          else if !score >= !beta then begin
            beta := !beta + !delta
          end;
          delta := !delta + (!delta / 2);
          score := search position search_tables thread multi !var_depth 0 !alpha !beta false;
        done;
      end;
     
      if !search_record.(multi).(!var_depth).score > (-max_int) then begin
        if number_of_pv > multi + 1 then begin
          remove_move !search_record.(multi).(!var_depth).bestmove picker;
          clear_entry !tt tt_index;
        end;
        actual_depths.(multi) <- !var_depth
      end
    done;
    for multi = 0 to (number_of_pv - 2) do
      add_move !search_record.(multi).(!var_depth).bestmove picker
    done;
    if thread = 0 then begin
      let exec_time = Mtime.Span.to_float_ns (Mtime_clock.count !start_time) /. 1e9 in
      let nps = int_of_float (float_of_int (total_counter node_counter) /. exec_time) in
      let hashfull = min 1000 (int_of_float (1000. *. (float_of_int (total_counter transposition_counter) /. (Int64.to_float !slots)))) in
      let time =  (int_of_float (1000. *. exec_time)) in
      let variations = ref [] in
      for multi = 0 to (number_of_pv - 1) do
        let actual_depth = actual_depths.(multi) in
        if not (actual_depth <> !var_depth && multi = 0) then begin
          if actual_depth > 0 then begin
            let result = !search_record.(multi).(actual_depth) in
            variations := (actual_depth, result.score, multi) :: !variations
          end
        end
      done;
      variations := List.sort (fun x y -> compare y x) !variations;
      if number_of_pv > 1 then begin
        let depth, score, multi = List.hd !variations in
        store thread zobrist depth score score !search_record.(multi).(depth).bestmove (hce position) !go_counter
      end;
      begin try
        let depth, _, id = (List.hd !variations) in
        bestline.depth <- depth;
        bestline.id <- id;
        bestline_id_tab.(!var_depth) <- id
      with _ -> ()
      end;
      if !var_depth > 1 then begin
        let previous_depth = !search_record.(bestline_id_tab.(!var_depth - 1)).(!var_depth - 1) in
        let actual_depth = !search_record.(bestline.id).(!var_depth) in
        if actual_depth.bestmove = previous_depth.bestmove then begin
          incr stability_counter
        end
        else begin
          stability_counter := 0
        end;
        if !var_depth > 6 && not stop_search.(0) && base_ms < 2. *. 10e8  then begin
          let scale = ref 1. in
          if !stability_counter < 2 then begin
            scale := !scale *. 1.3
          end
          else if !stability_counter > 3 then begin
            scale := !scale *. 0.75
          end;
          if actual_depth.score + 100 < previous_depth.score then begin
            scale := !scale *. 1.5
          end;
          let total_nodes = ref 0 in
          let bestmove_index = ref (-1) in
          for i = 0 to number_of_legal - 1 do
            let move = !nodes_fraction.(bestline.id).(i) in
            if move.move = actual_depth.bestmove then begin
              bestmove_index := i
            end;
            total_nodes := !total_nodes + move.nodes;
          done;
          let bestmove_nodes_fraction = float_of_int !nodes_fraction.(bestline.id).(!bestmove_index).nodes /. float_of_int !total_nodes in
          scale := (1.75 -. bestmove_nodes_fraction) *. !scale;
          let new_soft_bound = min (miliseconds_of_span !hard_bound) (!scale *. base_ms) in
          soft_bound := span_of_milliseconds new_soft_bound
        end
      end;
      let rec printer variations already_printed = match variations with
        |[] -> ()
        |(depth, _, multi) :: other_variations ->
          let print_alpha, print_beta = if multi = 0 then !alpha, !beta else (-99999), 99999 in
          let score = formate_score !search_record.(multi).(depth).score var_mate print_alpha print_beta in
          let pv = (String.concat " " (List.map uci_of_mouvement (pv_finder position !search_record.(multi).(depth).bestmove depth))) in
          print_endline (Printf.sprintf "info depth %i seldepth %i multipv %i score %s nodes %i nps %i hashfull %i time %i pv %s" depth depth already_printed score (total_counter node_counter) nps hashfull time pv);
          printer other_variations (already_printed + 1)
      in printer !variations 1
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
      let pos_copy = copy_position !current_position in
      let tables_copy = copy_search_tables !current_search_tables in
      stop_search.(thread_id) <- false;
    Mutex.unlock domain_mutex;
    iterative_deepening pos_copy tables_copy max_depth (-1) thread_id;
    Mutex.lock domain_mutex;
      decr jobs_remaining;
      if !jobs_remaining = 0 then begin
        work_available := false;
        Condition.broadcast domain_cond
      end;
    Mutex.unlock domain_mutex;
  done

let setoption search_tables instructions =
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
    |"name" :: "Ponder" :: _ -> ()
    |"name" :: "UCI_Chess960" :: _ -> type_check instructions chess_960
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash search_tables
    |"name" :: "MultiPV" :: _ ->
      let value = value_of_instructions instructions in
      if value <> !multipv then begin
        type_spin value multipv min_multipv max_multipv;
        search_record :=  (Array.init !multipv (fun _ -> Array.init (max_depth + 1) (fun _ -> {score = -max_int; bestmove = 0})));
        nodes_fraction := (Array.init !multipv (fun _ -> Array.init 218 (fun _ -> {move = 0; nodes = 0})))
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
            Domain.spawn (fun () -> domain_loop (id + old_value))
          )
        end
      end
    |_ -> ()

(*Answer to the command "go"*)
let go instructions position search_tables =
  start_time := Mtime_clock.counter ();
  let picker = search_tables.pickers.(0) in
  let number_of_legal = picker.number_of_captures + picker.number_of_quiets in
  if number_of_legal = 0 then begin
    let result = if true then "mate" else "cp" in
    print_endline (Printf.sprintf "info depth 0 score %s 0" result);
    print_endline "bestmove (none)"
  end
  else begin
    soft_bound := Mtime.Span.max_span;
    hard_bound := Mtime.Span.max_span;
    for thread = 0 to !threads_number - 1 do
      node_counter.(thread) <- 0;
      stop_search.(thread) <- false;
    done;
    for i = 0 to (max_depth + 40) - 1 do
      search_tables.pickers.(i).killer1 <- 0;
      search_tables.pickers.(i).killer2 <- 0
    done;
    incr go_counter;
    is_pondering := false;
    wtime := (-. 1.);
    btime := (-. 1.);
    winc := 0.;
    binc := 0.;
    movestogo := 500.;
    movetime := (9. *. 10e8);
    node_limit := max_int;
    let depth = ref max_depth in
    let mate = ref (-1) in
    let rec aux instruction = match instruction with
      |h :: g ->
        begin match h with
          |"ponder" -> is_pondering := true
          |"wtime" -> wtime := (float_of_string (List.hd g))
          |"btime" -> btime := (float_of_string (List.hd g))
          |"winc" -> winc := (float_of_string (List.hd g))
          |"binc" -> binc := (float_of_string (List.hd g))
          |"movestogo" -> movestogo := (float_of_string (List.hd g))
          |"depth" -> depth := (int_of_string (List.hd g))
          |"nodes" -> node_limit := (int_of_string (List.hd g))
          |"mate" -> mate := (int_of_string (List.hd g))
          |"movetime" -> movetime := (float_of_string (List.hd g))
          |_ -> ()
        end;
        aux g
      |_ -> ()
    in aux instructions;
    if not !is_pondering then begin
      init_time position number_of_legal !wtime !btime !winc !binc !movetime !movestogo
    end;
    let base_ms = miliseconds_of_span !soft_bound in
    search_record := (Array.init !multipv (fun _ -> Array.init (max_depth + 1) (fun _ -> {score = -max_int; bestmove = 0})));
    nodes_fraction := (Array.init !multipv (fun _ -> Array.init 218 (fun _ -> {move = 0; nodes = 0})));
    if !threads_number > 1 && base_ms > 50. then begin
      current_position := copy_position position;
      current_search_tables := copy_search_tables search_tables;
      Mutex.lock domain_mutex;
        incr current_job;
        jobs_remaining := !threads_number - 1;
        work_available := true;
        Condition.broadcast domain_cond;
      Mutex.unlock domain_mutex
    end;
    iterative_deepening (copy_position position) search_tables !depth !mate 0;
    if !threads_number > 1 && base_ms > 50. then begin
      Mutex.lock domain_mutex;
      for thread = 1 to !threads_number - 1 do
        stop_search.(thread) <- true
      done;
      while !jobs_remaining > 0 do
        Condition.wait domain_cond domain_mutex
      done;
      Mutex.unlock domain_mutex;
    end;
    while !is_pondering && not stop_search.(0) do
      ()
    done;
    let print_bestmove = "bestmove " ^ try (uci_of_mouvement (!search_record.(bestline.id).(bestline.depth).bestmove)) with _ -> uci_of_mouvement picker.hash_move in
    let print_ponder = try " ponder " ^ uci_of_mouvement (List.nth (pv_finder position !search_record.(bestline.id).(bestline.depth).bestmove bestline.depth) 1) with _ -> "" in
    print_endline (print_bestmove ^ print_ponder)
  end

let checkers position =
  let checkers = ref "" in
  let white_to_move = position.white_to_move in
  let total_occupancy = (position.occupancy.(0) ||| position.occupancy.(1)) in
  let king_position = (lsb_index position.pieces.(king + 6 * white_to_move)) in
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
  position_uci ["position"; "startpos"] position search_tables;
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
      |"setoption" :: _ -> process (fun () -> setoption search_tables instructions)
      |"ucinewgame" :: _ -> process (fun () -> reset_hash search_tables)
      |"position" :: _ -> process (fun () -> position_uci instructions position search_tables)
      |"go" :: "perft" :: depth :: _ when is_integer_string depth ->
        print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft position search_tables.pickers (int_of_string depth) 0)));
      |"go" :: "searchmoves" :: instructions ->
        let index = Int64.to_int (Int64.rem position.state_array.(0).zobrist !slots) in
        clear_entry !tt index;
        let picker = search_tables.pickers.(0) in
        picker.number_of_quiets <- 0;
        picker.number_of_captures <- 0;
        let rec func move_list = match move_list with
          |uci_move :: other_moves ->
            let move = try mouvement_of_uci uci_move position with _ -> 0 in
            if move <> 0 then begin
              add_move move picker
            end;
            func other_moves
          |_ -> ()
        in func instructions;
        let _ = Thread.create
          (fun () -> process (
            fun () ->
              go instructions position search_tables;
              legal_moves position picker phase_all)) ()
        in ()
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
        (*for i = 0 to search_tables.pickers.(0).number_of_captures - 1 do
          print_endline (Printf.sprintf "%s : see %i" (uci_of_mouvement search_tables.pickers.(0).capture_moves.(i)) (see position search_tables.pickers.(0).capture_moves.(i)))
        done;
        for i = 0 to search_tables.pickers.(0).number_of_quiets - 1 do
          print_endline (Printf.sprintf "%s : see %i" (uci_of_mouvement search_tables.pickers.(0).quiet_moves.(i)) (see position search_tables.pickers.(0).quiet_moves.(i)))
        done;*)
        let eval =
          if position.white_to_move = 0 then
            (float_of_int (hce position)) /. 100.
          else
            -. (float_of_int (hce position)) /. 100.
        in print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval ^ " (white side)")
      |"ponderhit" :: _ ->
        is_pondering := false;
        start_time := Mtime_clock.counter ();
        soft_bound := Mtime.Span.max_span;
        hard_bound := Mtime.Span.max_span;
        init_time position (search_tables.pickers.(0).number_of_captures + search_tables.pickers.(0).number_of_quiets) !wtime !btime !winc !binc !movetime !movestogo
      |[] -> ()
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." (List.hd instructions))
  done