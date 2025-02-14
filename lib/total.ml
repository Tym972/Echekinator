open Plateau
open Generateur
open Strategie1
open Quiescence
open Evaluations

let rec lemaire liste = match liste with
  |[] -> 0
  |h::t -> max tabvalue.(abs (prise h)) (lemaire t)

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax, utilisé après l'ouverture. Les pat par répétitions sont pris en comptes*)
let rec negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else if profondeur < 1 then begin
    best_score := traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let continuation = ref true in
    let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta) in
    (*if (not (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs)) && profondeur >= 3 then begin
      let score,_ = negalphabeta_total plateau (not trait_aux_blancs) Aucun droit_au_roque releve_plateau (profondeur - 3) profondeur_initiale (- beta) (- beta + 1) evaluation false in
      if score >= beta then begin
        continuation := false;
        best_score := beta
      end
    end;*)
    (*if profondeur = 1 then begin
      let position_roi = index_tableau plateau (roi trait_aux_blancs) in
      let roi_en_echec = menacee plateau position_roi trait_aux_blancs in
      if not (roi_en_echec || alpha < (-70000) || beta > 70000) then begin
        let delta = evaluation plateau trait_aux_blancs position_roi roi_en_echec alpha beta in
        if delta >= beta then begin
          cp := detecte_captures !cp;
          (*continuation := false;*)
          best_score := beta
        end
        else if delta + 10000 < alpha then begin
          cp := detecte_captures !cp;
          (*continuation := false;*)
          best_score := delta
        end
      end
    end;*)
    if !continuation then begin
      if !cp = [] then begin
        if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
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
            let note, _ = negalphabeta_total plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
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
      end end
  end;
  !best_score, !best_move

  let negalphabetime_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
    let t = Sys.time () in
    let fx = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation in
    fx, (Sys.time () -. t)