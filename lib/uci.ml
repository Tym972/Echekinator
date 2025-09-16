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

(*Fonction transformant une list de string en string*)
let rec string_of_list list = match list with
  |[] -> ""
  |h :: t -> h ^ " " ^ string_of_list t

(*Fonction renvoyant le relevé des positions actualisé*)
let new_board_record last_move board_record board white_to_move castling_rights =
  if is_irreversible last_move then begin
    [zobrist board white_to_move last_move castling_rights]
  end
  else begin
    zobrist board white_to_move last_move castling_rights :: !board_record
  end

(*Fonction permettant de jouer un move en actualisant les variables d'états de la partie*)
let make_move_2 board move white_to_move last_move castling_rights king_position in_check moves_record board_record = 
  make_move_1 board move white_to_move last_move castling_rights;
  king_position := index_array board (king !white_to_move);
  in_check := threatened board !king_position !white_to_move;
  moves_record := move :: !moves_record;
  board_record := new_board_record move board_record board !white_to_move !castling_rights

(*Fonction permettant de jouer une list de moves*)
let make_list move_list board last_move moves_record board_record castling_rights king_position in_check white_to_move =
  let rec func move_list  control = match move_list with
    |[] -> ()
    |move :: t when !control ->
      if List.mem move (legal_moves board !white_to_move !last_move !castling_rights !king_position !in_check) then begin
        make_move_2 board move white_to_move last_move castling_rights king_position in_check moves_record board_record;
        func t control
      end
      else if move = Null then begin
        func t control
      end
      else begin
        control := false
      end
    |_ -> ()
  in func move_list (ref true)

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

(*Time allocated for a search*)  
let search_time = ref 0.

(*Number of moves expected to play*)
let horizon = ref 40.

(*Variable indication if Pondering is allowed*)
let ponder = ref false

(*A implémenter*)
let multipv = ref 1
let min_multipv = 1
let max_multipv = 256

(*A implémenter*)
let mb_trasposition_size = ref 16
let min_mb_trasposition_size = 1
let max_mb_tansposition_size = 33554432

(*A implémenter*)
let threads_number = ref 1
let min_threads_number = 1
let max_threads_number = 1024

let reset_hash () =
  clear transposition_table;
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
    |"name" :: "Ponder" :: _ ->
      type_check instructions ponder;
      if !ponder then begin
        horizon := 25.
      end
      else begin
        horizon := 40.
      end
    |"name" :: "UCI_Chess960" :: _ -> type_check instructions chess_960
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash ()
    |"name" :: "MultiPV" :: _ -> type_spin instructions multipv min_multipv max_multipv
    |"name" :: "Hash" :: _ -> type_spin instructions mb_trasposition_size min_mb_trasposition_size max_mb_tansposition_size 
    |"name" :: "Threads" :: _ -> type_spin instructions threads_number min_threads_number max_threads_number
    |_ -> ()

(*Answer to the command "ucinewgame"*)
let ucinewgame board white_to_move last_move castling_right king_position in_check moves_record board_record start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record =
  reset_hash ();
  reset board white_to_move last_move castling_right king_position in_check moves_record board_record chessboard true Null (true, true, true, true) !from_white_king false [] [zobrist chessboard true Null (true, true, true, true)];
  reset start_position initial_white_to_move initial_last_move initial_castling_right  initial_king_position initial_in_check initial_moves_record initial_board_record chessboard true Null (true, true, true, true) !from_white_king false [] [zobrist chessboard true Null (true, true, true, true)]

(*Answer to the command "command"*)
let position instructions board white_to_move last_move castling_right king_position in_check moves_record board_record start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record =
  match instructions with
  |"position" :: str :: _ when List.mem str ["fen"; "startpos"] -> begin
      reset start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record chessboard true Null (true, true, true, true) !from_white_king false [] [zobrist chessboard true Null (true, true, true, true)];
      let index_moves = ref 2 in
      let rec aux_fen list  = match list with
      |h::t when h <> "moves" ->
        begin
          incr index_moves;
          h ^ " " ^ aux_fen t
        end
      |_ -> ""
      in position_of_fen (aux_fen (pop instructions 2)) start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record;
      reset board white_to_move last_move castling_right king_position in_check moves_record board_record start_position !initial_white_to_move !initial_last_move !initial_castling_right !initial_king_position !initial_in_check !initial_moves_record !initial_board_record;
      if ((List.length instructions) > !index_moves && List.nth instructions !index_moves = "moves") then begin
        let reverse_historique = move_list_of_san (string_of_list (pop instructions (!index_moves + 1))) !initial_white_to_move !initial_last_move !initial_castling_right board in
        make_list reverse_historique board last_move moves_record board_record castling_right king_position in_check white_to_move
      end
    end
  |_ -> ()

(*Fonction utilisée pour terminer la recherche après le temps alloué*)
let monitor_time time control =
  Thread.delay ((time /. 1000.) -. 0.01);
  control := true

(*Fonction mettant en forme le score retourné*)
let score score var_mate =
  if abs score < 99000 then begin
    Printf.sprintf "cp %i" (int_of_float (float_of_int (score) /. 10.))
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

let rec algoperft board white_to_move last_move castling_right depth root zobrist_position table_perft =
  if depth = 0 then begin
    1
  end
  else begin
    let old_key, number, hash_depth = table_perft.(zobrist_position mod transposition_size) in
    if depth = hash_depth && zobrist_position = old_key then begin
      number
    end
    else begin
      let king_position = index_array board (king white_to_move) in
      let in_check = threatened board king_position white_to_move in
      let legal_moves = ref (legal_moves board white_to_move last_move castling_right king_position in_check) in
      let nodes = ref 0 in
      while !legal_moves <> [] do
        let move = List.hd !legal_moves in
        let new_casling_right = castling_modification move castling_right in
        let nouveau_zobrist = if depth > 1 then (new_zobrist move last_move zobrist_position castling_right new_casling_right board) else 0 in
        make board move;
        legal_moves := List.tl !legal_moves;
        let perft = (algoperft board (not white_to_move) move new_casling_right (depth - 1) false nouveau_zobrist table_perft) in
        nodes := !nodes + perft;
        if root then begin
          print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
        end;
        unmake board move
      done;
       table_perft.(zobrist_position mod transposition_size) <- (zobrist_position, !nodes, depth);
      !nodes
    end
  end

(*Answer to the command "go"*)
let go instructions board white_to_move last_move castling_right king_position in_check board_record half_moves evaluation start_time =
  stop_calculation := false;
  node_counter := 0;
  for i = 0 to (2 * max_depth) - 1 do
    killer_moves.(i) <- Null
  done;
  incr go_counter;
  let zobrist_position = (List.hd board_record) in
  match instructions with
    |_ :: "perft" :: depth :: _ when is_integer_string depth ->
      let depth = int_of_string (List.nth instructions 2) in
      let table_perft = Array.make transposition_size (0, 0, (-1)) in
      print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft board white_to_move last_move castling_right depth true zobrist_position table_perft)));
    |_ ->
      let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
      let searchmoves = ref (legal_moves board white_to_move last_move castling_right king_position in_check) in
      let first_move = Array.make max_multipv Null in
      let _, _, _, move = probe transposition_table zobrist_position in
      first_move.(0) <- move;
      let is_pondering = ref false in
      let wtime = ref (float_of_int infinity) in
      let btime = ref (float_of_int infinity) in
      let winc = ref 0. in
      let binc = ref 0. in
      let movestogo = ref 500. in
      let depth = ref max_depth in
      let mate = ref (-1) in
      let movetime = ref (-1.) in
      let aux_searchmoves list  =
        let new_list = ref [] in
        let control = ref true in
        let rec func list = match list with
          |h::t when !control ->
            let move = tolerance board h white_to_move !searchmoves in
            if List.mem move !searchmoves then begin
              new_list := move :: !new_list;
              func t
            end
            else if List.mem h commands then begin
              control := false
            end
          |_ -> ()
        in func list;
        searchmoves := !new_list
      in let rec aux instruction = match instruction with
        |h :: g :: t ->
          begin match h with
            |"searchmoves" -> aux_searchmoves (g :: t)
            |"ponder" -> begin is_pondering := true end
            |"wtime" -> begin try wtime := (float_of_string g) with _ -> () end
            |"btime" -> begin try btime := (float_of_string g) with _ -> () end
            |"winc" -> begin try winc := (float_of_string g) with _ -> () end
            |"binc" -> begin try binc := (float_of_string g) with _ -> () end
            |"movestogo" -> begin try movestogo := (float_of_string g) with _ -> () end
            |"depth" -> begin try depth := (int_of_string g) with _ -> () end
            |"nodes" -> begin try node_limit := (int_of_string g) with _ -> () end
            |"mate" -> begin try mate := (int_of_string g) with _ -> () end
            |"movetime" -> begin
              try
                movetime := (float_of_string g)
              with _ -> () end
            |_ -> ()
        end;
        aux (g :: t)
        |_ -> ()
      in aux instructions;
      search_time :=
        if !movetime > 0. then begin
          !movetime
        end
        else begin
          if white_to_move then begin
            (if !wtime > !winc then !wtime +. !winc else !wtime) /. (min !movestogo !horizon)
          end
          else begin
            (if !btime > !binc then !btime +. !binc else !btime) /. (min !movestogo !horizon)
          end
        end;
      if not !is_pondering then begin
        let _ = Thread.create (fun () -> monitor_time !search_time stop_calculation) () in ()
      end;
      let best_score = ref "cp 0" in
      let pv = ref "" in
      let var_depth = ref 0 in 
      let var_mate = ref infinity in
      let alpha = ref (-infinity) in
      let beta = ref infinity in
      let print_bestmove = ref "0000" in
      let print_ponder = ref "0000" in
      while not !stop_calculation && !var_depth < !depth && !node_counter + 1 < !node_limit && !var_mate > !mate do
        incr var_depth;
        let searchmoves_copy = ref !searchmoves in
        for multi = 1 to !multipv do
          let new_score =
            let score = ref (root_search board white_to_move last_move castling_right board_record half_moves in_check !var_depth !alpha !beta evaluation zobrist_position first_move.(multi - 1) !searchmoves_copy pv) in
            while not (!stop_calculation || !node_counter > !node_limit || (!score > !alpha && !score < !beta)) do
              if !score <= !alpha then begin
                alpha := - infinity
              end
              else if !score >= !beta then begin
                beta := infinity
              end;
              score := root_search board white_to_move last_move castling_right board_record half_moves in_check !var_depth !alpha !beta evaluation zobrist_position first_move.(multi - 1) !searchmoves_copy pv;
            done;
            !score
          in if new_score > (-infinity) then begin
            print_bestmove := (try (List.nth (word_detection !pv) 0) with _ -> "0000");
            print_ponder := (try (List.nth (word_detection !pv) 1) with _ -> "0000");
            let bound = 
              if new_score <= !alpha then begin
                " upperbound"
              end
              else if new_score >= !beta then begin
                " lowerbound"
              end
              else begin
                ""
              end
            in 
            alpha := new_score - 25;
            beta := new_score + 25;
            best_score := score new_score var_mate ^ bound;
            let exec_time = Sys.time () -. start_time in
            print_endline (Printf.sprintf "info depth %i seldepth %i multipv %i score %s nodes %i nps %i hashfull %i time %i pv %s" !var_depth !var_depth multi !best_score !node_counter (int_of_float (float_of_int !node_counter /. exec_time)) (int_of_float (1000. *. (float_of_int !transposition_counter /. (float_of_int transposition_size)))) (int_of_float (1000. *. exec_time)) !pv);
            first_move.(multi - 1) <- pv_table.(0);
            if !multipv - multi > 0 then begin
              searchmoves_copy := List.filter (fun move -> move <> pv_table.(0)) !searchmoves_copy; print_endline (string_of_int (List.length !searchmoves_copy))
            end
          end
        done
      done;
      print_endline (Printf.sprintf "bestmove %s ponder %s" !print_bestmove !print_ponder)

let checkers board white_to_move =
  let position_roi = (index_array board (king white_to_move)) in
  let rec aux list = match list with
    |[] -> ""
    |move :: t -> if to_ move = position_roi then coord.(from move) ^ " " ^ aux t else aux t
  in let moves,_ = (pseudo_legal_moves board (not white_to_move) (index_array board (king (not white_to_move))))
  in aux moves

let display board white_to_move last_move castling_right moves_record board_record =
  print_board board;
  print_endline (Printf.sprintf "Fen: %s" (fen board white_to_move last_move castling_right moves_record board_record));
  print_endline (Printf.sprintf "Key: %i" (List.hd board_record));
  print_endline (Printf.sprintf "Checkers: %s" (checkers board white_to_move))

(*Fonction lançant le programme*)
let echekinator () =
  let board = Array.copy chessboard in
  let start_position = Array.copy chessboard in
  let last_move = ref Null in
  let initial_last_move = ref Null in
  let castling_right = ref (true, true, true, true) in
  let initial_castling_right = ref (true, true, true, true) in
  let king_position = ref !from_white_king in
  let initial_king_position = ref !from_white_king in
  let in_check = ref false in
  let initial_in_check = ref false in
  let moves_record = ref [] in
  let initial_moves_record = ref [] in
  let board_record = ref [zobrist chessboard true Null (true, true, true, true)] in
  let initial_board_record = ref [zobrist chessboard true Null (true, true, true, true)] in
  let white_to_move = ref true in
  let initial_white_to_move = ref true in
  let evaluation =
    if not (List.length !moves_record > 30 || tours_connectees board !white_to_move) then
      eval1_q
    else if not (finale board || List.length !moves_record > 90) then
      eval2_q
    else
      eval3_q
  in
  let exit = ref false in
  while not !exit do
    let command_line = word_detection (lire_entree "" true) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"setoption" -> setoption command_line
      |"ucinewgame" -> ucinewgame board white_to_move last_move castling_right king_position in_check moves_record board_record start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record
      |"position" -> position command_line board white_to_move last_move castling_right king_position in_check moves_record board_record start_position initial_white_to_move initial_last_move initial_castling_right initial_king_position initial_in_check initial_moves_record initial_board_record
      |"go" ->
        let start_time = Sys.time () in
        let _ = Thread.create (fun () -> go command_line (Array.copy board) !white_to_move !last_move !castling_right !king_position !in_check !board_record (List.length !board_record - 1) evaluation start_time) () in ()
      |"quit" -> exit := true
      |"stop" -> stop_calculation := true
      |"d" -> display board !white_to_move !last_move !castling_right !moves_record !board_record
      |"eval" ->
        let eval = (float_of_int (evaluation board !white_to_move !king_position !in_check (-infinity) infinity)) /. 1000. in
        print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval)
      |"ponderhit" -> let _ = Thread.create (fun () -> monitor_time !search_time stop_calculation) () in ()
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." command)
  done