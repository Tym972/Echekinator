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

(*Fonction renvoyant le relevé des positions actualisé*)
let new_board_record last_move board_record zobrist_position half_moves =
  if is_irreversible last_move then begin
    half_moves := 0;
    [zobrist_position]
  end
  else begin
    incr half_moves;
    zobrist_position :: !board_record
  end

(*Fonction permettant de jouer une list de moves*)
let make_list move_list board last_move white_to_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves =
  let rec func move_list  control = match move_list with
    |[] -> ()
    |move :: t when !control ->
      let legal_moves, number_of_legal_moves = (legal_moves board !white_to_move !last_move !castling_rights !king_position !in_check) in
      if move_array_mem move legal_moves !number_of_legal_moves then begin
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
    |"name" :: "Ponder" :: _ -> type_check instructions ponder
    |"name" :: "UCI_Chess960" :: _ -> type_check instructions chess_960
    |"name" :: "Clear" :: "Hash" :: _ -> reset_hash ()
    |"name" :: "MultiPV" :: _ -> type_spin instructions multipv min_multipv max_multipv
    |"name" :: "Hash" :: _ -> type_spin instructions mb_trasposition_size min_mb_trasposition_size max_mb_tansposition_size 
    |"name" :: "Threads" :: _ -> type_spin instructions threads_number min_threads_number max_threads_number
    |_ -> ()

(*Answer to the command "ucinewgame"*)
let ucinewgame board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves =
  reset_hash ();
  reset board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves chessboard true Null (true, true, true, true) !from_white_king false [] (zobrist chessboard true Null (true, true, true, true)) 0;
  reset start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves chessboard true Null (true, true, true, true) !from_white_king false [] (zobrist chessboard true Null (true, true, true, true)) 0;
  position_aspects.(0) <- (!white_to_move, !last_move, !castling_rights, !board_record, !half_moves, !zobrist_position)

(*Answer to the command "command"*)
let position instructions board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves =
  begin match instructions with
    |"position" :: str :: _ when List.mem str ["fen"; "startpos"] -> begin
        reset start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves chessboard true Null (true, true, true, true) !from_white_king false [] (zobrist chessboard true Null (true, true, true, true)) 0;
        let index_moves = ref 2 in
        let rec aux_fen list  = match list with
        |h::t when h <> "moves" ->
          begin
            incr index_moves;
            h ^ " " ^ aux_fen t
          end
        |_ -> ""
        in if str = "fen" then begin
          position_of_fen (aux_fen (pop instructions 2)) start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves;
        end;
          reset board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves start_position !initial_white_to_move !initial_last_move !initial_castling_rights !initial_king_position !initial_in_check !initial_moves_record !initial_zobrist_position !initial_half_moves;
        if ((List.length instructions) > !index_moves && List.nth instructions !index_moves = "moves") then begin
          let reverse_historique = move_list_of_algebric_list (algebric_list_of_san (String.concat " " (pop instructions (!index_moves + 1)))) !initial_white_to_move !initial_last_move !initial_castling_rights board in
          make_list reverse_historique board last_move white_to_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves
        end;
        position_aspects.(0) <- (!white_to_move, !last_move, !castling_rights, !board_record, !half_moves, !zobrist_position)
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

let rec algoperft board depth ply table_perft =
  if depth = 0 then begin
    1
  end
  else begin
    let white_to_move, last_move, castling_rights, _, _, zobrist_position = position_aspects.(ply) in
    let old_key, number, hash_depth = table_perft.(zobrist_position mod transposition_size) in
    if depth = hash_depth && zobrist_position = old_key then begin
      number
    end
    else begin
      let king_position = index_array board (king white_to_move) in
      let in_check = threatened board king_position white_to_move in
      let moves, number_of_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in
      let nodes = ref 0 in
      for i = 0 to !number_of_moves - 1 do
        let move = moves.(i) in
        let new_castling_rights = castling_modification move castling_rights in
        let new_zobrist = if depth > 1 then (new_zobrist move last_move zobrist_position castling_rights new_castling_rights board) else 0 in
        make board move;
        position_aspects.(ply + 1) <- (not white_to_move, move, new_castling_rights, [], 0, new_zobrist);
        let perft = (algoperft board (depth - 1) (ply + 1) table_perft) in
        nodes := !nodes + perft;
        if ply = 0 then begin
          print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
        end;
        unmake board move
      done;
       table_perft.(zobrist_position mod transposition_size) <- (zobrist_position, !nodes, depth);
      !nodes
    end
  end

(*Answer to the command "go"*)
let go instructions board white_to_move last_move castling_rights zobrist_position king_position in_check =
  out_of_time := false;
  node_counter := 0;
  for i = 0 to (2 * max_depth) - 1 do
    killer_moves.(i) <- Null
  done;
  incr go_counter;
  match instructions with
    |_ :: "perft" :: depth :: _ when is_integer_string depth ->
      let depth = int_of_string depth in
      let table_perft = Array.make transposition_size (0, 0, (-1)) in
      print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft board depth 0 table_perft)));
    |_ ->
      let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
      let legal_moves, number_of_legal_moves = legal_moves board white_to_move last_move castling_rights king_position in_check in
      if !number_of_legal_moves = 0 then begin
        let result = if in_check then "mate" else "cp" in
        print_endline (Printf.sprintf "info depth 0 score %s 0" result);
        print_endline "bestmove (none)"
      end
      else begin
        let is_pondering = ref false in
        let wtime = ref (float_of_int max_int) in
        let btime = ref (float_of_int max_int) in
        let winc = ref 0. in
        let binc = ref 0. in
        let movestogo = ref 500. in
        let depth = ref max_depth in
        let mate = ref (-1) in
        let movetime = ref (-1.) in
        let aux_searchmoves list =
          let index = ref 0 in
          let new_moves = Array.make !number_of_legal_moves Null in
          let control = ref true in
          let rec func list = match list with
            |h::t when !control -> print_endline h;
              let move = tolerance board h white_to_move legal_moves !number_of_legal_moves in
              if move_array_mem move legal_moves !number_of_legal_moves then begin
                new_moves.(!index) <- move;
                incr index;
              end
              else if List.mem h commands then begin
                control := false
              end;
              func t
            |_ -> ()
          in func list;
          if !index = 0 then begin
            out_of_time := true
          end
          else begin
            number_of_legal_moves := !index;
            for i = 0 to !index - 1 do
              legal_moves.(i) <- new_moves.(i)
            done
          end
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
        let soft_bound, hard_bound =
          if !movetime > 0. then begin
            !movetime /. 1000., !movetime /. 1000.
          end
          else begin
            if white_to_move then begin
              ((!wtime /. (min !movestogo 25.)) +. !winc /. 2.) /. 1000., ((!wtime /. (min !movestogo 20.)) +. !winc /. 2.) /. 1000.
            end
            else begin
              ((!btime /. (min !movestogo 25.)) +. !binc /. 2.) /. 1000., ((!btime /. (min !movestogo 20.)) +. !binc /. 2.) /. 1000.
            end
          end in
        search_time := hard_bound;
        if not !is_pondering then begin
          start_time := Sys.time ();
        end;
        pv_table.(0) <- (let _, _, _, move(*, _*) = probe transposition_table zobrist_position in move);
        let number_of_pv = min !multipv !number_of_legal_moves in
        let print_score_table = Array.make number_of_pv "cp 0" in
        let short_pv_table = Array.make number_of_pv [] in
        let alpha_table = Array.make number_of_pv (- max_int) in
        let beta_table = Array.make number_of_pv max_int in
        let reached_depth_table = Array.make number_of_pv 0 in
        let info_table = Array.make number_of_pv ((-1), 0, 0) in
        let var_depth = ref 0 in 
        let var_mate = ref max_int in
        let info = ref [] in
        while (Sys.time () -. !start_time < soft_bound) && !var_depth < !depth && !node_counter + 1 < !node_limit && !var_mate > !mate do
          incr var_depth;
          let legal_moves_copy = (Array.copy legal_moves) in
          let number_of_legal_moves_copy = (ref !number_of_legal_moves) in
          for multi = 0 to (number_of_pv - 1) do
            let first_move =
              let acc = ref Null in
              let counter = ref 0 in
              while !acc = Null && !counter < number_of_pv do
                let candidate = try List.hd short_pv_table.(!counter) with _ -> Null in
                if move_array_mem candidate legal_moves_copy !number_of_legal_moves_copy then begin
                  acc := candidate
                end;
                incr counter
              done;
              !acc
            in let new_score =
              let score = ref (root_search in_check !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy legal_moves_copy) (ref !number_of_legal_moves_copy) short_pv_table multi) in
              while not (!out_of_time || !node_counter > !node_limit || (!score > alpha_table.(multi) && !score < beta_table.(multi))) do
                if !score <= alpha_table.(multi) then begin
                  alpha_table.(multi) <- (-max_int)
                end
                else if !score >= beta_table.(multi) then begin
                  beta_table.(multi) <- max_int
                end;
                score := root_search in_check !var_depth alpha_table.(multi) beta_table.(multi) first_move (Array.copy legal_moves_copy) (ref !number_of_legal_moves_copy) short_pv_table multi;
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
              reached_depth_table.(multi) <- !var_depth;
              print_score_table.(multi) <- score new_score var_mate ^ bound;
              info_table.(multi) <- !var_depth, new_score, multi;
              if number_of_pv > multi + 1 then begin
                for index = 0 to !number_of_legal_moves_copy - 1 do
                  if pv_table.(0) = legal_moves_copy.(index) then begin
                    remove_move index legal_moves_copy number_of_legal_moves_copy
                  end
                done
              end
            end
          done;
          info := merge_sort (Array.to_list info_table);
          let exec_time = Sys.time () -. !start_time in
          let nps = int_of_float (float_of_int !node_counter /. exec_time) in
          let hashfull = int_of_float (1000. *. (float_of_int !transposition_counter /. (float_of_int transposition_size))) in
          let time =  (int_of_float (1000. *. exec_time)) in
          let rec aux_info list n = match list with
            |[] -> ()
            |(depth, _, multi) :: t ->
              if !var_depth = depth then begin
                let score = print_score_table.(multi) in
                let pv = (String.concat " " (List.map uci_of_mouvement short_pv_table.(multi))) in
                print_endline (Printf.sprintf "info depth %i seldepth %i multipv %i score %s nodes %i nps %i hashfull %i time %i pv %s" !var_depth !var_depth n score !node_counter nps hashfull time pv)
              end;
              aux_info t (n + 1)
          in aux_info !info 1;
        done;
        let _, _, best_number = try List.hd !info with _ -> (0, 0, (-1)) in
        if best_number = (-1) then begin
          print_endline ("info depth 0 score cp 0" ^ "\n" ^ "bestmove (none)");
        end
        else begin
          let print_bestmove = "bestmove " ^ try (uci_of_mouvement  (List.hd short_pv_table.(best_number))) with _ -> "(none)" in
          let print_ponder = try (" ponder " ^ uci_of_mouvement  (List.nth short_pv_table.(best_number) 1)) with _ -> "" in
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

let display board white_to_move last_move castling_rights moves_record zobrist_position half_moves =
  print_board board;
  print_endline (Printf.sprintf "Fen: %s" (fen board white_to_move last_move castling_rights moves_record half_moves));
  print_endline (Printf.sprintf "Key: %i" zobrist_position);
  print_endline (Printf.sprintf "Checkers: %s" (checkers board white_to_move))

(*Fonction lançant le programme*)
let echekinator () =
  let start_position = Array.copy chessboard in
  let white_to_move = ref true in
  let initial_white_to_move = ref true in
  let last_move = ref Null in
  let initial_last_move = ref Null in
  let castling_rights = ref (true, true, true, true) in
  let initial_castling_rights = ref (true, true, true, true) in
  let king_position = ref !from_white_king in
  let initial_king_position = ref !from_white_king in
  let in_check = ref false in
  let initial_in_check = ref false in
  let moves_record = ref [] in
  let initial_moves_record = ref [] in
  let zobrist_position = ref (zobrist chessboard true Null (true, true, true, true)) in
  let initial_zobrist_position = ref (zobrist chessboard true Null (true, true, true, true)) in
  let board_record = ref [!zobrist_position] in
  let initial_board_record = ref [!zobrist_position] in
  let half_moves = ref 0 in
  let initial_half_moves = ref 0 in
  position_aspects.(0) <- (!white_to_move, !last_move, !castling_rights, !board_record, !half_moves, !zobrist_position);
  let exit = ref false in
  while not !exit do
    let command_line = word_detection (lire_entree "" true) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"setoption" -> setoption command_line
      |"ucinewgame" -> ucinewgame board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves
      |"position" -> position command_line board white_to_move last_move castling_rights king_position in_check moves_record zobrist_position board_record half_moves start_position initial_white_to_move initial_last_move initial_castling_rights initial_king_position initial_in_check initial_moves_record initial_zobrist_position initial_board_record initial_half_moves
      |"go" ->
        start_time := max_float;
        let _ = Thread.create (fun () -> go command_line (Array.copy board) !white_to_move !last_move !castling_rights !zobrist_position  !king_position !in_check) () in ()
      |"quit" -> exit := true
      |"stop" -> out_of_time := true
      |"d" -> display board !white_to_move !last_move !castling_rights !moves_record !zobrist_position !half_moves
      |"eval" ->
        let eval = (float_of_int (hce board true)) /. 100. in
        print_endline ("HCE Evaluation : " ^ (if eval > 0. then "+" else "") ^ string_of_float eval ^ " (white side)")
      |"ponderhit" ->
        start_time := Sys.time ();
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." command)
  done