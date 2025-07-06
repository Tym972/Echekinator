(*Module implémentant la communication UCI*)

open Plateau
open Generateur
open Zobrist
open Traduction1
open Traduction2
open Traduction3
open Strategie1
open Evaluations

(*Supprime les n premiers éléments d'une liste*)
let rec pop list n =
  if n = 0 then begin
    list
  end
  else begin
    match list with
      |[] -> []
      |_ :: t -> pop t (n - 1)
  end

(*Fonction transformant une liste de string en string*)
let rec string_of_list list = match list with
  |[] -> ""
  |h :: t -> h ^ " " ^ string_of_list t

(*Answer to the command "uci"*)
let uci () =
  print_endline (
    "id name " ^ nom_du_projet ^ "\n"
    ^ "id author Timothée Fixy" ^ "\n"
    ^ "\n"
    ^ "option name Clear Hash type button" ^ "\n"
    ^ "option name Ponder type check default false" ^ "\n"
    ^ "option name UCI_Chess960 type check default false" ^ "\n"
    ^ "uciok")

let setoption instructions =
  let longueur = List.length instructions in
  if longueur > 2 then begin
    match (List.nth instructions 2) with
      |"UCI_Chess960" when longueur = 5 -> chess_960 := (bool_of_string (List.nth instructions 4))
      |_ -> ()
  end

(*Answer to the command "ucinewgame"*)
let ucinewgame plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial =
  actualise table 1000;
  compteur_transposition := 0;
  reinitialise plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau echiquier true Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)];
  reinitialise position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial  echiquier true Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)]

(*Answer to the command "command"*)
let position instructions plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial =
  let length_list = List.length instructions in
  if length_list > 1 then begin
    reinitialise position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial echiquier true Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)];
    let index_moves = ref 2 in
    if List.nth instructions 1 = "fen" then begin
      let rec aux_fen list  = match list with
      |h::t when h <> "moves" ->
        begin
          incr index_moves;
          h ^ " " ^ aux_fen t 
        end
      |_ -> ""
    in position_of_fen (aux_fen (pop instructions 2)) position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial;
    end;
    reinitialise plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial !releve_coups_initial !releve_plateau_initial;
    if (length_list > !index_moves && List.nth instructions !index_moves = "moves") then begin
      let reverse_historique = move_list_of_san (string_of_list (pop instructions (!index_moves + 1))) !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial plateau in
      joue_liste reverse_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs
    end
  end

(*Fonction utilisée pour terminer la recherche après le temps alloué*)
let monitor_time time control =
  Thread.delay (time /. 1000.);
  control := true

(*Fonction mettant en forme le score retourné*)
let score score =
  if abs score < 99000 then begin
    Printf.sprintf "cp %i" (int_of_float (float_of_int (score) /. 10.))
  end
  else begin
    if score mod 2 = 0 then begin
      Printf.sprintf "mate %i" (((99999 - score) / 2) + 1)
    end
    else begin
      Printf.sprintf "mate -%i" (((99999 + score) / 2))
    end
  end

let pv_finder depth =
  let pv = ref "" in
  for i = 0 to (min pv_length.(0) depth) - 1 do 
    pv := !pv ^ (uci_of_mouvement pv_table.(i)) ^ " ";
  done;
  !pv

(*
let g () =
  for i = 0 to (2 * max_depth) - 1 do
    killer_moves.(i) <- Aucun
  done;
  for i = 0 to 8191 do
    history_moves.(i) <- 0
  done;*)

let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur racine zobrist_position table_perft =
  if profondeur = 0 then begin
    1
  end
  else begin
    let nombre, hash_depth = try ZobristHashtbl.find table_perft zobrist_position with _ -> (-1),(-1) in
    if profondeur = hash_depth then begin
      nombre
    end
    else begin
      let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque) in
      let nodes = ref 0 in
      while !cp <> [] do
        let coup = List.hd !cp in
        let nouveau_doit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_zobrist = if profondeur > 1 then (nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_doit_au_roque plateau) else 0 in
        joue plateau coup;
        cp := List.tl !cp;
        let perft = (algoperft plateau (not trait_aux_blancs) coup nouveau_doit_au_roque (profondeur - 1) false nouveau_zobrist table_perft) in
        nodes := !nodes + perft;
        if racine then begin
          print_endline (uci_of_mouvement coup ^ ": " ^ string_of_int perft)
        end;
        dejoue plateau coup
      done;
      ZobristHashtbl.add table_perft zobrist_position (!nodes, profondeur);
      !nodes
    end
  end

(*Answer to the command "go"*)
let go instruction plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau demi_coups evaluation =
  if (List.length instruction > 1 && List.nth instruction 1 = "perft" && est_entier_string (List.nth instruction 2)) then begin
    let depth = int_of_string (List.nth instruction 2) in
    let table_perft = ZobristHashtbl.create taille_transposition in
    print_endline ("\n" ^ "Nodes searched : " ^ (string_of_int (algoperft plateau trait_aux_blancs dernier_coup droit_au_roque depth true (List.hd releve_plateau) table_perft)));
  end
  else begin
    let t = Sys.time () in
    let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
    let searchmoves = ref [] in
    let wtime = ref (float_of_int infinity) in
    let btime = ref (float_of_int infinity) in
    let winc = ref 0. in
    let binc = ref 0. in
    let movestogo = ref (-1) in
    let depth = ref max_depth in
    let nodes = ref infinity in
    let mate = ref false in
    let movetime = ref (-1.) in
    let rec aux_searchmoves list coups_valides_joueur = match list with
      |[] -> ()
      |h::t ->
        if not (List.mem h commands) then begin
          searchmoves := !searchmoves @ [tolerance plateau h trait_aux_blancs coups_valides_joueur];
          aux_searchmoves t coups_valides_joueur
        end
    in let rec aux instruction = match instruction with
      |h :: g :: t ->
        begin match h with
          |"searchmoves" -> aux_searchmoves (g :: t) (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)
          |"wtime" -> begin try wtime := (float_of_string g) with _ -> () end
          |"btime" -> begin try btime := (float_of_string g) with _ -> () end
          |"winc" -> begin try winc := (float_of_string g) with _ -> () end
          |"binc" -> begin try binc := (float_of_string g) with _ -> () end
          |"movestogo" -> begin try movestogo := (int_of_string g) with _ -> () end
          |"depth" -> begin try depth := (int_of_string g) with _ -> () end
          |"nodes" -> begin try nodes := (int_of_string g) with _ -> () end
          |"mate" -> mate := true
          |"movetime" -> begin
            try
              movetime := (float_of_string g)
            with _ -> () end
          |_ -> ()
      end;
      aux (g :: t)
      |_ -> ()
    in aux instruction;
    let time =
      if !movetime > 0. then begin
        !movetime
      end
      else begin
        if trait_aux_blancs then begin
          (if !wtime > !winc then !wtime +. !winc else !wtime) /. (if !movestogo > 0 then (float_of_int !movestogo) else 40.)
        end
        else begin
          (if !btime > !binc then !btime +. !binc else !btime) /. (if !movestogo > 0 then (float_of_int !movestogo) else 40.)
        end
      end
    in let _ = Thread.create (fun () -> monitor_time time stop_calculating) () in
    let best_score = ref "cp 0" in
    let pv = ref "" in
    let var_depth = ref 0 in
    while not !stop_calculating && !var_depth < !depth do
      incr var_depth;
      let new_score = pvs plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau demi_coups !var_depth !var_depth (-infinity) infinity evaluation (List.hd releve_plateau) true in
      if not !stop_calculating then begin
        let exec_time = Sys.time () -. t in
        best_score := score new_score;
        pv := pv_finder !var_depth;
        print_endline (Printf.sprintf "info depth %i seldepth %i multipv 1 score %s nodes %i nps %i hashfull %i time %i pv %s" !var_depth !var_depth !best_score !compteur_recherche (int_of_float (float_of_int !compteur_recherche /. exec_time)) (int_of_float (1000. *. (float_of_int !compteur_transposition /. (float_of_int taille_transposition)))) (int_of_float (1000. *. exec_time)) !pv);
      end
    done;
    print_endline (Printf.sprintf "bestmove %s ponder %s" (try (List.nth (detecte_mots !pv) 0) with _ -> "0000") (try (List.nth (detecte_mots !pv) 1) with _ -> "0000"))
  end

let checkers plateau trait_aux_blancs =
  let position_roi = (index_tableau plateau (roi trait_aux_blancs)) in
  let rec aux liste = match liste with
    |[] -> ""
    |coup :: t -> if arrivee coup = position_roi then coord.(depart coup) ^ " " ^ aux t else aux t
  in let moves,_ = (deplacements plateau (not trait_aux_blancs) (index_tableau plateau (roi (not trait_aux_blancs))))
  in aux moves

let display plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  affiche plateau;
  print_endline (Printf.sprintf "Fen: %s" (fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau));
  print_endline (Printf.sprintf "Key: %i" (List.hd releve_plateau));
  print_endline (Printf.sprintf "Checkers: %s" (checkers plateau trait_aux_blancs))

(*Fonction lançant le programme*)
let echekinator () =
  let plateau = Array.copy echiquier in
  let position_de_depart = Array.copy echiquier in
  let dernier_coup = ref Aucun in
  let dernier_coup_initial = ref Aucun in
  let droit_au_roque = ref (true, true, true, true) in
  let droit_au_roque_initial = ref (true, true, true, true) in
  let releve_coups = ref [] in
  let releve_coups_initial = ref [] in
  let releve_plateau = ref [zobrist echiquier true Aucun (true, true, true, true)] in
  let releve_plateau_initial = ref [zobrist echiquier true Aucun (true, true, true, true)] in
  let trait_aux_blancs = ref true in
  let trait_aux_blancs_initial = ref true in
  let evaluation =
    if not (List.length !releve_coups > 30 || tours_connectees plateau !trait_aux_blancs) then
      eval1_q
    else if not (finale plateau || List.length !releve_coups > 90) then
      eval2_q
    else
      eval3_q
  in
  let exit = ref false in
  while not !exit do
    let command_line = detecte_mots (lire_entree "" true) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"setoption" -> setoption command_line
      |"ucinewgame" -> ucinewgame plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      |"position" -> position command_line plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      |"go" ->
        let _ = Thread.create (fun () -> go command_line (Array.copy plateau) !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau (List.length !releve_plateau - 1) evaluation) () in
        stop_calculating := false;
        compteur_recherche := 0
      |"quit" -> exit := true
      |"stop" -> stop_calculating := true
      |"d" -> display plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau
      |"ponderhit" -> ()
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." command)
  done