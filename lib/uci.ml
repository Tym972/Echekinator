(*Module implémentan la communication UCI*)

open Plateau
open Generateur
open Zobrist
open Traduction1
open Traduction3
open Config
open Strategie1
open Evaluations

let rec pop list n = 
  if n = 0 then begin
    list
  end
  else begin
    match list with
      |[] -> []
      |_ :: t -> pop t (n - 1)
  end

let rec string_of_list list = match list with
  |[] -> ""
  |h :: t -> h ^ " " ^ string_of_list t

let uci () =
  print_endline ("id name " ^ nom_du_projet ^ "\n" ^ "id author Timothée Fixy" ^ "\n \n" ^ "uciok")

let ucinewgame position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  reinitialise position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau echiquier true Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)]

let position instructions position_de_depart plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  let length_list = List.length instructions in
  if length_list > 1 then begin
    let index_moves = ref 2 in
    if List.nth instructions 1 = "fen" then begin
      position_of_fen (supprimer (List.nth instructions 2)) position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
      for i = 0 to 63 do
        plateau.(i) <- position_de_depart.(i)
      done;
      index_moves := 3
    end;
    if (length_list > !index_moves && List.nth instructions !index_moves = "moves") then begin
      let reverse_historique = algebric_to_type_mouvement (string_of_list (pop instructions (!index_moves + 1))) !trait_aux_blancs !dernier_coup !droit_au_roque position_de_depart in print_endline (string_of_int (List.length reverse_historique));
      joue_liste reverse_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs
    end else begin  end
  end

let go instruction plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau =
  let commands = ["searchmoves"; "ponder"; "wtime"; "btime"; "winc"; "binc"; "movestogo"; "depth"; "nodes"; "mate"; "movetime"; "infinite"] in
  let searchmoves = ref [] in
  let ponder = ref false in
  let wtime = ref 10000. in
  let btime = ref 10000. in
  let winc = ref 100. in
  let binc = ref 100. in
  let movestogo = ref (-1) in
  let depth = ref 255 in
  let nodes = ref (Int64.to_int (Random.int64 4611686018427387903L)) in
  let mate = ref false in
  let movetime = ref 1000. in
  let infinite = ref false in
  let best_score = ref 0 in
  let best_move = ref Aucun in
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
        |"ponder" -> ponder := true
        |"wtime" -> begin try wtime := (float_of_string g) with _ -> () end
        |"btime" -> begin try btime := (float_of_string g) with _ -> () end
        |"winc" -> begin try winc := (float_of_string g) with _ -> () end
        |"binc" -> begin try binc := (float_of_string g) with _ -> () end
        |"movestogo" -> begin try movestogo := (int_of_string g) with _ -> () end
        |"depth" -> begin try depth := (int_of_string g) with _ -> () end
        |"nodes" -> begin try nodes := (int_of_string g) with _ -> () end
        |"mate" -> mate := true
        |"movetime" -> begin try movetime := (float_of_string g) with _ -> () end
        |"infinite" -> infinite := true
        |_ -> ()
    end;
    aux (g :: t)
    |_ -> ()
  in aux instruction;
  for i = 1 to !depth do print_endline (string_of_int i);
    let new_score, new_move = negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i (-999999) 9999999 eval1_q in
    best_move := new_move;
    best_score := new_score
  done;
  affiche plateau

let echekinator () =
  let position_de_depart = Array.copy echiquier in
  let plateau = Array.copy echiquier in
  let dernier_coup = ref Aucun in
  let droit_au_roque = ref (true, true, true, true) in
  let releve_coups = ref [] in
  let releve_plateau = ref [zobrist echiquier true Aucun (true, true, true, true)] in
  let trait_aux_blancs = ref true in
  let exit = ref false in
  let calculating = ref true in
  while not !exit do
    let command_line = detecte_mots (lire_entree "" false) in
    let command = try List.hd command_line with _ -> "" in
    match command with
      |"uci" -> uci ()
      |"isready" -> print_endline "readyok"
      |"ucinewgame" -> ucinewgame position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau
      |"position" -> position command_line position_de_depart plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau
      |"go" ->
        let go_thread = Thread.create (fun () -> go command_line plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau) () in ignore go_thread
      |"quit" -> exit := true
      |"stop" -> calculating := false
      |_ -> ()
  done