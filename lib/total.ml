open Plateau
open Generateur
open Strategie1
open Quiescence
open Transposition
open Zobrist

let compteur_transposition = ref 0

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

let rec negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position =
  incr compteur_recherche;
  if !stop_calculating || repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    0, Aucun
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let best_score = ref (-infinity) in
    let best_move = ref Aucun in
    let ply = profondeur_initiale - profondeur in
    let presence = ref true in
    let hash_node_type, hash_depth, hash_value, hash_move, _ =
      try ZobristHashtbl.find table zobrist_position with _ ->
        begin
          presence := false;
          (All, (-1), 0, Aucun, 0)
        end
    in let continuation = ref true in
    if !presence (*&& not (List.length releve_plateau > 6 && (repetition (nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque (modification_roque hash_move droit_au_roque) :: releve_plateau) 3)*) then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation ply
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
      end
      else begin
        let b = ref true in
        let hash_ordering =
          (*not (hash_node_type = All || hash_move = Aucun) && hash_value > beta*)
          ((hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun)
        in if hash_ordering then begin
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau in
          joue plateau hash_move;
          let score =
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
          let cp =
            if hash_ordering then
              ref (List.filter (fun c -> c <> hash_move) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta))
            else
              ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
          in if !cp = [] && not hash_ordering then begin incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := ply - 99999
            end 
            else begin
              best_score := 0
            end
          end
          else begin
            while (!b && !cp <> []) do
              let coup = List.hd !cp in
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau in
              joue plateau coup;
              cp := List.tl !cp;
              let score =
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
    end;
    if not !stop_calculating then begin
      let node_type =
        if !best_score <= alpha then begin
          All
        end
        else if !best_score >= beta then begin
          Cut
        end
        else begin
          Pv
        end
      in let stored_value =
        if abs !best_score < 99000 then begin
          !best_score
        end
        else begin
          if !best_score >= 0 then begin
            !best_score + ply
          end
          else begin
            !best_score - ply
          end
        end
      in if !presence then begin
        if profondeur > hash_depth then begin
          ZobristHashtbl.replace table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
        end
      end
      else begin
        incr compteur_transposition;
        ZobristHashtbl.add table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
      end
    end;
    !best_score, !best_move
  end

let adapte_releve3 zobrist_position coup profondeur releve_plateau demi_coups =
  if est_irremediable coup then begin
    if profondeur < 8 then begin
      [], 0
    end
    else begin
      [zobrist_position], 0
    end
  end
  else if demi_coups + profondeur < 7 then begin
    [], demi_coups + 1
  end
  else begin 
    zobrist_position :: releve_plateau, demi_coups + 1
  end
  
let rec pvs plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau demi_coups profondeur profondeur_initiale alpha beta evaluation zobrist_position ispv =
  incr compteur_recherche;
  if !stop_calculating || repetition releve_plateau 3 || demi_coups = 100 then begin
    incr compteur_noeuds_terminaux;
    0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let best_score = ref (-infinity) in
    let best_move = ref Aucun in
    let ply = profondeur_initiale - profondeur in
    let presence = ref true in
    let hash_node_type, hash_depth, hash_value, hash_move, _ =
      if ispv then begin
        presence := false; (All, (-1), 0, Aucun, 0)
      end
      else begin
        try ZobristHashtbl.find table zobrist_position with _ ->
          begin
            presence := false;
            (All, (-1), 0, Aucun, 0)
          end
        end
    in let continuation = ref true in
    if !presence then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation ply
    end;
    if !continuation then begin
      if profondeur = 0 then begin
        incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
      end
      else begin
        let b = ref true in
        let hash_ordering = (hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun in
        if hash_ordering then begin
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve, nouveau_demi_coups = adapte_releve3 nouveau_zobrist hash_move profondeur releve_plateau demi_coups in
          joue plateau hash_move;
          let score = - pvs plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
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
          let cp =
            if hash_ordering then
              ref (List.filter (fun c -> c <> hash_move) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta))
            else
              ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
          in if !cp = [] && not hash_ordering then begin incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := ply - 99999
            end 
            else begin
              best_score := 0
            end
          end
          else begin
            let first_move = ref true in
            while (!b && !cp <> []) do
              let coup = List.hd !cp in
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve, nouveau_demi_coups = adapte_releve3 nouveau_zobrist coup profondeur releve_plateau demi_coups in
              joue plateau coup;
              cp := List.tl !cp;
              let score =
                if !first_move then begin
                  first_move := false;
                  - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
                end
                else begin
                  let note0 = - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !alpha0 - 1) (- !alpha0) evaluation nouveau_zobrist false
                  in if (note0 > !alpha0 && ispv) then begin 
                    - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
                  end
                  else begin
                    note0
                  end
                end 
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
    end;
    if not !stop_calculating then begin
      let node_type =
        if !best_score <= alpha then begin
          (*best_move := Aucun;*)
          All
        end
        else if !best_score >= beta then begin
          Cut
        end
        else begin
          Pv
        end
      in let stored_value =
        if abs !best_score < 99000 then begin
          !best_score
        end
        else begin
          if !best_score >= 0 then begin
            !best_score + ply
          end
          else begin
            !best_score - ply
          end
        end
      in if !presence then begin
        if profondeur > hash_depth then begin
          ZobristHashtbl.replace table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
        end
      end
      else begin
        incr compteur_transposition;
        ZobristHashtbl.add table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
      end
    end;
    !best_score
  end

let negalphabetime_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation (List.hd releve_plateau) in
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
  let fx = iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-infinity) infinity evaluation in
  fx, (Sys.time () -. t)