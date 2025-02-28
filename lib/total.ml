open Plateau
open Generateur
open Strategie1
open Quiescence
open Transposition
open Zobrist

let traitement_mat_profondeur_0 plateau trait_aux_blancs dernier_coup =
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  if (menacee plateau position_roi trait_aux_blancs) then begin
    let cp = coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)
    in if cp = [] then begin
      (- 99950)
    end
    else begin
      0
    end
  end
  else begin
    0
  end

let rec negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position = incr compteur_recherche;
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    0, Aucun
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in 
    let best_score = ref (-99999) in
    let best_move = ref Aucun in
    let presence = ref true in
    let hash_node_type, hash_depth, hash_value, hash_move, _ = try ZobristHashtbl.find table zobrist_position with _ -> begin
      presence := false;
      (All, (-1), 0, Aucun, 0)
    end
    in begin
      let continuation = ref true in
      if !presence then begin
        traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation
      end;
      if !continuation then begin
        if profondeur = 0 then begin incr compteur_noeuds_terminaux;
          best_score := traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
        end
        else begin
          let b = ref true in
          if (hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun then begin
            joue plateau hash_move;
            let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
            let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque in
            let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau
            in let score =
              let note, _ = negalphabeta_total plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
              in - note
            in if score > !best_score then begin
              best_score := score;
              best_move := hash_move;
              alpha0 := max !alpha0 score;
              if score >= !beta0 then begin
                b := false
              end
            end;
            dejoue plateau hash_move;
          end;
          if !b then begin
            let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
            in if !cp = [] then begin incr compteur_noeuds_terminaux;
              if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
                best_score := (profondeur_initiale - (profondeur + 99999))
              end 
              else begin
                best_score := 0
              end
            end
            else begin
              while (!b && !cp <> []) do
                let coup = List.hd !cp in
                joue plateau coup;
                cp := List.tl !cp;
                let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
                let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque in
                let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau
                in let score =
                  let note, _ = negalphabeta_total plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
                  in - note 
                in if score > !best_score then begin
                  best_score := score;
                  best_move := coup;
                  alpha0 := max !alpha0 score;
                  if score >= !beta0 then begin
                    b := false
                  end
                end;
                dejoue plateau coup
              done
            end
          end
        end
      end
    end;
    let node_type =
      if !best_score <= alpha then begin
        All
      end
      else if !best_score >= beta then begin
        Cut
      end
      else begin
        Pv
      end in
    if !presence then begin
      if profondeur > hash_depth then begin
        ZobristHashtbl.replace table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
      end
    end
    else begin
      ZobristHashtbl.add table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
    end;
    !best_score, !best_move
  end

let negalphabetime_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation (List.hd releve_plateau) in
  fx, (Sys.time () -. t)

let iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let score = ref 0 in
  let move = ref Aucun in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
  done;
  !score, !move

let iid_time_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)