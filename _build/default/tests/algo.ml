open Config
open Libs.Plateau
open Libs.Generateur
open Libs.Strategie1
open Libs.Strategie2
open Libs.Traduction2
open Libs.Zobrist
open Libs.Quiescence
open Libs.Transposition
open Libs.Total

let rec negamax plateau trait_aux_blancs dernier_coup droit_au_roque profondeur evaluation =
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if profondeur = 0 then begin
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup (-99999) 99999
  end
  else begin
    let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)
    in if !cp = [] then begin
      if (menacee plateau (index plateau (roi trait_aux_blancs))) then begin
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
        let score =
          let note, _ = negamax plateau (not trait_aux_blancs) coup (modification_roque coup droit_au_roque) (profondeur - 1) evaluation
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
let rec negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (coups_joueur plateau profondeur trait_aux_blancs dernier_coup droit_au_roque [] evaluation negalphabeta_valide)
    in if !cp = [] then begin
      if (menacee plateau (index plateau (roi trait_aux_blancs))) then begin
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
        let score =
          let note, _ = negalphabeta plateau (not trait_aux_blancs) coup (modification_roque coup droit_au_roque) (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
          in - note 
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

let rec pvs plateau trait_aux_blancs dernier_coup droit_au_roque profondeur profondeur_initiale alpha beta evaluation =
  let alpha0 = ref alpha in
  let best_move = ref Aucun in
  if profondeur = 0 then begin
    alpha0 := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (coups_joueur plateau profondeur trait_aux_blancs dernier_coup droit_au_roque [] evaluation negalphabeta_valide)
    in if !cp = [] then begin
      if (menacee plateau (index plateau (roi trait_aux_blancs))) then begin
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
        if !first then begin
          score := begin
            let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
            in - note
          end;
          first := false
        end
        else begin
          score := begin
            let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (profondeur - 1) profondeur_initiale (- (!alpha0 + 1)) (- !alpha0) evaluation
            in - note
          end;
          if alpha < !score && !score < beta then begin
            score := begin
              let note, _ = pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
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

let rec supprime element liste = match liste with
  |[] -> []
  |h::t -> if h = element then t else h :: supprime element t

let coups_joueur_idd plateau profondeur profondeur_initiale trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation idd_candidat algo =
  if profondeur <> profondeur_initiale then
    tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo
  else
    idd_candidat :: supprime idd_candidat (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo)

let rec negalphabeta_idd plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation idd_candidat =
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else if profondeur = 0 then begin
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (coups_joueur_idd plateau profondeur profondeur_initiale trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation idd_candidat negalphabeta_valide)
    in if !cp = [] then begin
      if (menacee plateau (index plateau (roi trait_aux_blancs))) then begin
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
        let nouveau_releve =
          if est_irremediable coup then begin
            if profondeur < 8 then begin
              []
            end
            else begin
              [zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque]
            end
          end
          else if List.length releve_plateau + profondeur < 8 then begin
            []
          end
          else begin 
            ((zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque) :: releve_plateau)
          end
        in let score =
          let note, _ = negalphabeta_idd plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation Aucun
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

let idd plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let move = ref (List.hd (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)) in
  let score = ref 0 in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_idd plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation !move in
    move := new_move;
    score := new_score;
    print_endline (algebric_of_mouvement !move plateau coups_valides_joueur ^ " " ^ string_of_int !score ^ " " ^  string_of_int i) 
  done;
  !score, !move

let negatime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur evaluation  =
  let t = Sys.time () in
  let fx = negamax plateau trait_aux_blancs dernier_coup droit_au_roque profondeur evaluation in
  fx, (Sys.time () -. t)

(*Fonction renvoyant un appel à la fonction alphabeta ansi que le temps nécessaire à l'éxécution*)
let negalphabetime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque profondeur profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let pvstime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = pvs plateau trait_aux_blancs dernier_coup droit_au_roque profondeur profondeur_initiale (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let idd_time plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = idd plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let runnegamax b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negatime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runnegalphabeta b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negalphabetime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runpvs b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = pvstime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runnegalphabeta_valide b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negalphabetime_valide plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau;
  end

let runnegalphabeta_trans b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negalphabetime_trans plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a)); 
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runnegalphabeta_quiescent b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negalphabetime_quiescent plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end;
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runidd b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = idd_time plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end;
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau
  end

let runnegalphabeta_fp b1 b2 plateau trait_aux_blancs =
  if b1 then begin
    affiche plateau
  end;
  let (a, coup), c = negalphabetime_total plateau! trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_int a));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if b2 then begin
    affiche plateau;
  end

let main b1 b2 b3 b4 b5 b6 b7 b8 plateau =
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau;
  if b1 then begin
    print_endline "Negamax";
    runnegamax false true (Array.copy plateau) trait_aux_blancs
  end;
  if b2 then begin
    print_endline "Negalphabeta";
    runnegalphabeta false true (Array.copy plateau) trait_aux_blancs
  end;
  if b3 then begin
    print_endline "Negalphabeta valide";
    runnegalphabeta_valide false true (Array.copy plateau) trait_aux_blancs
  end;
  if b4 then begin
    print_endline "PVS";
    runpvs false true (Array.copy plateau) trait_aux_blancs
  end;
  if b5 then begin
    print_endline "Negalphabeta transposition";
    runnegalphabeta_trans false true (Array.copy plateau) trait_aux_blancs
  end;
  if b6 then begin
    print_endline "Negalphabeta quiescent";
    runnegalphabeta_quiescent false true (Array.copy plateau) trait_aux_blancs
  end;
  if b7 then begin
    print_endline "IDD";
    runidd false true (Array.copy plateau) trait_aux_blancs
  end;
  if b8 then begin
    print_endline "Negalphabeta futility pruning";
    runnegalphabeta_fp false true (Array.copy plateau) trait_aux_blancs
  end;
  print_endline (algebric_of_mouvement !dernier_coup plateau coups_valides_joueur);
  print_endline ("Noeuds explorés : " ^ string_of_int !compteur_recherche);
  print_endline ("Noeuds recherche quiescente : " ^ string_of_int !compteur_quiescent);
  print_endline ("EBF : " ^ string_of_float (float_of_int !compteur_recherche /. float_of_int (!compteur_recherche - !compteur_noeuds_terminaux)))

let () = main false true false false false false false false plateau