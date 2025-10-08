open Config
open Libs.Board
open Libs.Zobrist
open Libs.Generator
open Libs.Transposition
open Strategie2
open Libs.Traduction
open Libs.Quiescence

let rec negamax board white_to_move last_move droit_au_roque releve_plateau depth evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if depth = 0 then begin incr compteur_noeuds_terminaux;
    best_score := quiescence_treatment_depth_0 evaluation board white_to_move last_move (-99999) 99999
  end
  else begin
    let cp = ref (legal_moves board white_to_move last_move droit_au_roque)
    in if !cp = [] then begin
      if threatened board (index_array board (king white_to_move)) white_to_move then begin
        best_score := (- (depth + 9999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      while !cp <> [] do
        let move = List.hd !cp in
        make board move;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque move droit_au_roque in
        let nouveau_releve = adapte_releve board move depth white_to_move nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negamax board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) evaluation
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := move;
        end;
        dejoue board move
      done
    end
  end;
  !best_score, !best_move

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax*)
let rec negalphabeta_simple board white_to_move last_move droit_au_roque releve_plateau depth profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if depth = 0 then begin incr compteur_noeuds_terminaux;
    best_score := quiescence_treatment_depth_0 evaluation board white_to_move last_move alpha beta
  end
  else begin
    let cp = ref (tab_tri.(depth - 1) board white_to_move last_move droit_au_roque [] evaluation negalphabeta)
    in if !cp = [] then begin
      if threatened board (index_array board (king white_to_move)) white_to_move then begin
        best_score := (profondeur_initiale - (depth + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let move = List.hd !cp in
        make board move;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque move droit_au_roque in
        let nouveau_releve = adapte_releve board move depth white_to_move nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_simple board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
          in - note 
        in if score > !best_score then begin
          best_score := score;
          best_move := move;
          if score >= beta then begin
            b := false
          end
          else begin
            alpha0 := max !alpha0 score
          end
        end;
        dejoue board move
      done
    end
  end;
  !best_score, !best_move

let rec pvs board white_to_move last_move droit_au_roque releve_plateau depth profondeur_initiale alpha beta evaluation =
  let alpha0 = ref alpha in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    alpha0 := 0
  end
  else if depth = 0 then begin
    alpha0 := quiescence_treatment_depth_0 evaluation board white_to_move last_move alpha beta
  end
  else begin
    let cp = ref (tab_tri.(depth - 1) board white_to_move last_move droit_au_roque [] evaluation negalphabeta)
    in if !cp = [] then begin
      if threatened board (index_array board (king white_to_move)) white_to_move then begin
        alpha0 := (profondeur_initiale - (depth + 9999))
      end 
      else begin
        alpha0 := 0
      end
    end
    else begin
      let score = ref 0 in
      let b = ref true in
      let first = ref true in
      while (!b && !cp <> []) do
        let move = List.hd !cp in
        make board move;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = (modification_roque move droit_au_roque) in
        let nouveau_releve = adapte_releve board move depth white_to_move nouveau_droit_au_roque releve_plateau in
        if !first then begin
          score := begin
            let note, _ = pvs board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
            in - note
          end;
          first := false
        end
        else begin
          score := begin
            let note, _ = pvs board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- (!alpha0 + 1)) (- !alpha0) evaluation
            in - note
          end;
          if alpha < !score && !score < beta then begin
            score := begin
              let note, _ = pvs board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
              in - note
            end;
          end
        end;
        if !score > !alpha0 then begin
          alpha0 := !score;
          best_move := move 
        end;
        if !alpha0 >= beta then begin
          b := false
        end;
        dejoue board move
      done
    end
  end;
  !alpha0, !best_move

let coups_joueur_idd_simple board depth profondeur_initiale white_to_move last_move droit_au_roque releve_plateau evaluation idd_candidat algo =
  if depth <> profondeur_initiale then
    tab_tri.(depth - 1) board white_to_move last_move droit_au_roque releve_plateau evaluation algo
  else
    idd_candidat :: List.filter (fun move -> move <> idd_candidat) (tab_tri.(depth - 1) board white_to_move last_move droit_au_roque releve_plateau evaluation algo)

let rec negalphabeta_idd_simple board white_to_move last_move droit_au_roque releve_plateau depth profondeur_initiale alpha beta evaluation idd_candidat = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else if depth = 0 then begin
    best_score := quiescence_treatment_depth_0 evaluation board white_to_move last_move alpha beta
  end
  else begin
    let cp = ref (coups_joueur_idd_simple board depth profondeur_initiale white_to_move last_move droit_au_roque releve_plateau evaluation idd_candidat negalphabeta)
    in if !cp = [] then begin
      if threatened board (index_array board (king white_to_move)) white_to_move then begin
        best_score := (profondeur_initiale - (depth + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let move = List.hd !cp in
        make board move;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque move droit_au_roque in
        let nouveau_releve = adapte_releve board move depth white_to_move nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_idd_simple board (not white_to_move) move nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- beta) (- !alpha0) evaluation Null
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := move;
          if score >= beta then begin
            b := false
          end
          else begin
            alpha0 := max !alpha0 score
          end
        end;
        dejoue board move
      done
    end
  end;
  !best_score, !best_move

let idd_basique_print board white_to_move last_move droit_au_roque releve_plateau depth alpha beta evaluation =
  let move = ref (List.hd (coups_valides board white_to_move last_move droit_au_roque)) in
  let score = ref 0 in
  for i = 1 to depth do
    let new_score, new_move = negalphabeta board white_to_move last_move droit_au_roque releve_plateau i i alpha beta evaluation in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move board coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_simple_print board white_to_move last_move droit_au_roque releve_plateau depth alpha beta evaluation =
  let move = ref (List.hd (coups_valides board white_to_move last_move droit_au_roque)) in
  let score = ref 0 in
  for i = 1 to depth do
    let new_score, new_move = negalphabeta_idd_simple board white_to_move last_move droit_au_roque releve_plateau i i alpha beta evaluation !move in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move board coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_trans_print board white_to_move last_move droit_au_roque releve_plateau depth alpha beta evaluation =
  let score = ref 0 in
  let move = ref Null in
  for i = 1 to depth do
    let new_score, new_move = negalphabeta_trans board white_to_move last_move droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move board coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_total_print board white_to_move last_move droit_au_roque releve_plateau depth alpha beta evaluation =
  let score = ref 0 in
  let move = ref Null in
  for i = 1 to depth do
    let new_score, new_move = negalphabeta_total board white_to_move last_move droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move board coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i)
  done;
  !score, !move

let negatime board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = negamax board white_to_move last_move droit_au_roque releve_plateau depth evaluation in
  fx, (Sys.time () -. t)

(*Fonction renvoyant un appel à la fonction alphabeta ansi que le temps nécessaire à l'éxécution*)
let negalphabetime_simple board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_simple board white_to_move last_move droit_au_roque releve_plateau depth depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let pvstime board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = pvs board white_to_move last_move droit_au_roque releve_plateau depth depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_basique_print board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = idd_basique_print board white_to_move last_move droit_au_roque releve_plateau depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_simple_print board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = idd_simple_print board white_to_move last_move droit_au_roque releve_plateau depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_trans_print board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = idd_trans_print board white_to_move last_move droit_au_roque releve_plateau depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_total_print board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  let t = Sys.time () in
  let fx = idd_total_print board white_to_move last_move droit_au_roque releve_plateau depth (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation algo =
  for _ = 1 to nombre_de_coups do
    let (a, move), c = algo board !white_to_move !last_move !droit_au_roque !releve_plateau depth evaluation in
    if affichage then begin
      print_endline ("Matériel : " ^ (string_of_int a));
      print_endline ("Temps : " ^ (string_of_float c))
    end; 
    joue_coup_2 board move white_to_move last_move droit_au_roque releve_coups releve_plateau;
    if affichage then begin
      affiche board;
      print_endline (algebric_of_move !last_move board coups_valides_joueur);
    end
  done

let runnegamax affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negatime

let runnegalphabeta_simple affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negalphabetime_simple

let runpvs affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation pvstime

let runnegalphabeta affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negalphabetime

let runnegalphabeta_trans affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negalphabetime_trans

let runnegalphabeta_quiescent affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negalphabetime_quiescent

let runnegalphabeta_fp affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation negalphabetime_total

let runidd_basique affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation idd_time_basique_print

let runidd_simple affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation idd_time_simple_print

let runidd_trans affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation idd_time_trans_print

let runidd_total affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation =
  run affichage board white_to_move last_move droit_au_roque releve_plateau depth evaluation idd_time_total_print

(*let stat tableau_1 tableau_2 tableau_3 =
  let taille_tableau = Array.length tableau_1 - 1 in
  let n1 = ref 0 in
  let n2 = ref 0 in
  let n3 = tableau_3.(0) + tableau_3.(1) in
  print_endline (Printf.sprintf "Hash = Best : %f et Hash <> Best : %f" ((float_of_int tableau_3.(0)) /. (float_of_int n3)) ((float_of_int tableau_3.(1)) /. (float_of_int n3)));
  for i = 0 to taille_tableau do
    n1:= !n1 + tableau_1.(i);
    n2 := !n2 + tableau_2.(i)
  done;
  print_endline (Printf.sprintf "Hash_best : %i et Best_move %i" !n1 !n2);
  for i = 0 to taille_tableau do
    let k1 = (float_of_int tableau_1.(i)) /. (float_of_int !n1) in
    let k2 = (float_of_int tableau_2.(i)) /. (float_of_int !n2) in
    print_endline (Printf.sprintf "Index_%i - Hash_best : %f et Best_move : %f" i k1 k2);
  done
  
  let tab_hash_1 = Array.make 100 0
let tab_hash_2 = Array.make 100 0
let tab_hash_best = Array.make 2 0

let a1 = ref 0
let a2 = ref 0*)

let main b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 board =
  print_endline ("\nProfondeur " ^ (string_of_int depth));
  affiche board;
  if b1 then begin
    print_endline "Negamax";
    runnegamax true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b2 then begin
    print_endline "Negalphabeta sans releve_plateau";
    runnegalphabeta_simple true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b3 then begin
    print_endline "Negalphabeta";
    runnegalphabeta true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b4 then begin
    print_endline "PVS";
    runpvs true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b5 then begin
    print_endline "Negalphabeta transposition";
    runnegalphabeta_trans true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b6 then begin
    print_endline "Negalphabeta quiescent";
    runnegalphabeta_quiescent true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b7 then begin
    print_endline "Negalphabeta total";
    runnegalphabeta_fp true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b8 then begin
    print_endline "IDD Basique";
    runidd_basique true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b9 then begin
    print_endline "IDD Simple";
    runidd_simple true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b10 then begin
    print_endline "IDD Trans";
    runidd_trans true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  if b11 then begin
    print_endline "IDD Total";
    runidd_total true (Array.copy board) (ref !white_to_move) (ref !last_move) (ref !droit_au_roque) (ref !releve_plateau) depth evaluation
  end;
  print_endline ("Noeuds explorés : " ^ string_of_int !compteur_recherche);
  print_endline ("Noeuds recherche quiescente : " ^ string_of_int !compteur_quiescent);
  print_endline ("Noeuds hashtable : " ^ string_of_int !compteur_transposition);
  print_endline ("EBF : " ^ string_of_float (float_of_int !compteur_recherche /. float_of_int (!compteur_recherche - !compteur_noeuds_terminaux)));
  actualise table 10 (*;stat tab_hash_1 tab_hash_2 tab_hash_best print_endline (Printf.sprintf "oui : %i et non : %i et taux : %f" !a1 !a2 ((float_of_int !a1)/.(float_of_int !a1 +. float_of_int !a2)))*)

let () = main false false false false false false false false false false true board