open Config
open Libs.Board
open Libs.Zobrist
open Libs.Generator
open Libs.Transposition
open Strategie2
open Libs.Traduction
open Libs.Quiescence

let rec negamax plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup (-99999) 99999
  end
  else begin
    let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)
    in if !cp = [] then begin
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := (- (profondeur + 9999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      while !cp <> [] do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negamax plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) evaluation
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax*)
let rec negalphabeta_simple plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque [] evaluation negalphabeta)
    in if !cp = [] then begin
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := (profondeur_initiale - (profondeur + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_simple plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
          in - note 
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
          if score >= beta then begin
            b := false
          end
          else begin
            alpha0 := max !alpha0 score
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

let rec pvs plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation =
  let alpha0 = ref alpha in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    alpha0 := 0
  end
  else if profondeur = 0 then begin
    alpha0 := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque [] evaluation negalphabeta)
    in if !cp = [] then begin
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        alpha0 := (profondeur_initiale - (profondeur + 9999))
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
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = (modification_roque coup droit_au_roque) in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau in
        if !first then begin
          score := begin
            let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
            in - note
          end;
          first := false
        end
        else begin
          score := begin
            let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- (!alpha0 + 1)) (- !alpha0) evaluation
            in - note
          end;
          if alpha < !score && !score < beta then begin
            score := begin
              let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
              in - note
            end;
          end
        end;
        if !score > !alpha0 then begin
          alpha0 := !score;
          best_move := coup 
        end;
        if !alpha0 >= beta then begin
          b := false
        end;
        dejoue plateau coup
      done
    end
  end;
  !alpha0, !best_move

let coups_joueur_idd_simple plateau profondeur profondeur_initiale trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation idd_candidat algo =
  if profondeur <> profondeur_initiale then
    tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo
  else
    idd_candidat :: List.filter (fun coup -> coup <> idd_candidat) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo)

let rec negalphabeta_idd_simple plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation idd_candidat = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Null in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else if profondeur = 0 then begin
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (coups_joueur_idd_simple plateau profondeur profondeur_initiale trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation idd_candidat negalphabeta)
    in if !cp = [] then begin
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := (profondeur_initiale - (profondeur + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_idd_simple plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation Null
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
          if score >= beta then begin
            b := false
          end
          else begin
            alpha0 := max !alpha0 score
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

let idd_basique_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let move = ref (List.hd (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)) in
  let score = ref 0 in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move plateau coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_simple_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let move = ref (List.hd (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)) in
  let score = ref 0 in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_idd_simple plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation !move in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move plateau coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_trans_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let score = ref 0 in
  let move = ref Null in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move plateau coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let idd_total_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let score = ref 0 in
  let move = ref Null in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_move !move plateau coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i)
  done;
  !score, !move

let negatime plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negamax plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation in
  fx, (Sys.time () -. t)

(*Fonction renvoyant un appel à la fonction alphabeta ansi que le temps nécessaire à l'éxécution*)
let negalphabetime_simple plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_simple plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let pvstime plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = pvs plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_basique_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = idd_basique_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_simple_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = idd_simple_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_trans_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = idd_trans_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time_total_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = idd_total_print plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation algo =
  for _ = 1 to nombre_de_coups do
    let (a, coup), c = algo plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur evaluation in
    if affichage then begin
      print_endline ("Matériel : " ^ (string_of_int a));
      print_endline ("Temps : " ^ (string_of_float c))
    end; 
    joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
    if affichage then begin
      affiche plateau;
      print_endline (algebric_of_move !dernier_coup plateau coups_valides_joueur);
    end
  done

let runnegamax affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negatime

let runnegalphabeta_simple affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabetime_simple

let runpvs affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation pvstime

let runnegalphabeta affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabetime

let runnegalphabeta_trans affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabetime_trans

let runnegalphabeta_quiescent affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabetime_quiescent

let runnegalphabeta_fp affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabetime_total

let runidd_basique affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation idd_time_basique_print

let runidd_simple affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation idd_time_simple_print

let runidd_trans affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation idd_time_trans_print

let runidd_total affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  run affichage plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation idd_time_total_print

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

let main b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 plateau =
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau;
  if b1 then begin
    print_endline "Negamax";
    runnegamax true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b2 then begin
    print_endline "Negalphabeta sans releve_plateau";
    runnegalphabeta_simple true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b3 then begin
    print_endline "Negalphabeta";
    runnegalphabeta true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b4 then begin
    print_endline "PVS";
    runpvs true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b5 then begin
    print_endline "Negalphabeta transposition";
    runnegalphabeta_trans true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b6 then begin
    print_endline "Negalphabeta quiescent";
    runnegalphabeta_quiescent true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b7 then begin
    print_endline "Negalphabeta total";
    runnegalphabeta_fp true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b8 then begin
    print_endline "IDD Basique";
    runidd_basique true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b9 then begin
    print_endline "IDD Simple";
    runidd_simple true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b10 then begin
    print_endline "IDD Trans";
    runidd_trans true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  if b11 then begin
    print_endline "IDD Total";
    runidd_total true (Array.copy plateau) (ref !trait_aux_blancs) (ref !dernier_coup) (ref !droit_au_roque) (ref !releve_plateau) profondeur evaluation
  end;
  print_endline ("Noeuds explorés : " ^ string_of_int !compteur_recherche);
  print_endline ("Noeuds recherche quiescente : " ^ string_of_int !compteur_quiescent);
  print_endline ("Noeuds hashtable : " ^ string_of_int !compteur_transposition);
  print_endline ("EBF : " ^ string_of_float (float_of_int !compteur_recherche /. float_of_int (!compteur_recherche - !compteur_noeuds_terminaux)));
  actualise table 10 (*;stat tab_hash_1 tab_hash_2 tab_hash_best print_endline (Printf.sprintf "oui : %i et non : %i et taux : %f" !a1 !a2 ((float_of_int !a1)/.(float_of_int !a1 +. float_of_int !a2)))*)

let () = main false false false false false false false false false false true plateau