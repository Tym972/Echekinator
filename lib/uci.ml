(*Module implémentan la communication UCI*)

open Plateau
open Generateur
open Zobrist
open Traduction1
open Traduction2
open Traduction3
open Config
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
  print_endline ("id name " ^ nom_du_projet ^ "\n" ^ "id author Timothée Fixy" ^ "\n \n" ^ "uciok")

(*Answer to the command "ucinewgame"*)
let ucinewgame plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial =
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
      let reverse_historique = algebric_to_type_mouvement (string_of_list (pop instructions (!index_moves + 1))) !trait_aux_blancs !dernier_coup !droit_au_roque plateau in
      joue_liste reverse_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs
    end
  end

(*Fonction utilisée pour terminer la recherche après le temps alloué*)
let monitor_time time =
  Thread.delay (time /. 1000.);
  stop_calculating := true

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

(*Answer to the command "go"*)
let go instruction plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau =
  let t = Sys.time () in
  let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
  let searchmoves = ref [] in
  let wtime = ref (float_of_int infinity) in
  let btime = ref (float_of_int infinity) in
  let winc = ref 0. in
  let binc = ref 0. in
  let movestogo = ref (-1) in
  let depth = ref 255 in
  let nodes = ref infinity in
  let mate = ref false in
  let movetime = ref 1000. in
  let best_score = ref "cp 0" in
  let best_move = ref "0000" in
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
        |"movetime" -> begin try movetime := (float_of_string g) with _ -> () end
        |_ -> ()
    end;
    aux (g :: t)
    |_ -> ()
  in aux instruction;
  let time =
    if trait_aux_blancs then begin
      (if !wtime > !winc then !wtime +. !winc else !wtime) /. (if !movestogo > 0 then (float_of_int !movestogo) else 40.)
    end
    else begin
      (if !btime > !binc then !btime +. !binc else !btime) /. (if !movestogo > 0 then (float_of_int !movestogo) else 40.)
    end
  in let _ = Thread.create (fun () -> monitor_time time) () in
  let var_depth = ref 0 in
  while not !stop_calculating && !var_depth < !depth do
    incr var_depth;
    let new_score, new_move = negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau !var_depth !var_depth (-infinity) infinity eval1_q in
    if not !stop_calculating then begin
      let exec_time = Sys.time () -. t in
      best_move := uci_of_mouvement new_move;
      best_score := score new_score;
      print_endline (Printf.sprintf "info depth %i multipv 1 score %s nodes %i nps %i hashfull 0 time %i pv %s" !var_depth !best_score !compteur_recherche (int_of_float (float_of_int !compteur_recherche /. exec_time)) (int_of_float (1000. *. exec_time)) !best_move);
    end
  done;
  print_endline (Printf.sprintf "bestmove %s ponder %s" !best_move "0000")

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
  let exit = ref false in
  while not !exit do
    let command_line = detecte_mots (lire_entree "" true) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"ucinewgame" -> ucinewgame plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      |"position" -> position command_line plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      |"go" ->
        let _ = Thread.create (fun () -> go command_line (Array.copy plateau) !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau) () in
        stop_calculating := false;
        compteur_recherche := 0
      |"quit" -> exit := true
      |"stop" -> stop_calculating := true
      |_ -> print_endline (Printf.sprintf "Unknown command: '%s'. Type help for more information." command)
  done