(*Module implémentant les fonctions permettant la recherche quiescente*)

open Plateau
open Generateur
open Strategie1
open Evaluations

(*Fonction construisant une liste des déplacements possible d'une tour*)
let captures_tour plateau case liste =
  let co = plateau.(case) in
  let t = tab64.(case) in
  for i = 0 to 3 do
    let dir = vect_tour.(i) in
    let k = ref 1 in
    let s = ref true in
    while (!s && tab120.(t + (!k * dir)) <> (-1)) do
      let candidat = tab120.(t + (!k * dir)) in
      let dest = plateau.(candidat) in
      if dest = 0 then begin
        incr k
      end
      else if co * dest > 0 then begin
        s :=  false
      end
      else begin 
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
        s :=  false
      end
    done
  done

(*Fonction construisant une liste des déplacements possible d'un fou*)
let captures_fou plateau case liste =
  let co = plateau.(case) in
  let f = tab64.(case) in
  for i = 0 to 3 do
    let dir = vect_fou.(i) in
    let k = ref 1 in
    let s = ref true in
    while (!s && tab120.(f + (!k * dir)) <> (-1)) do
      let candidat = tab120.(f + (!k * dir)) in
      let dest = plateau.(candidat) in
      if dest = 0 then begin
        incr k
      end
      else if co * dest > 0 then begin
        s :=  false
      end
      else begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
        s :=  false
      end
    done
  done

(*Fonction construisant une liste des déplacements possible d'un cavalier*)
let captures_cavalier plateau case liste =
  let co = plateau.(case) in
  let c = tab64.(case) in
  for i = 0 to 7 do
    let dir = vect_cavalier.(i) in
    if tab120.(c + dir) <> (-1) then begin
      let candidat = tab120.(c + dir) in
      let dest = plateau.(candidat) in
      if co * dest < 0 then begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste
      end
    end
  done

(*Fonction construisant une liste des déplacements possible d'une dame*)
let captures_dame plateau case liste =
  let co = plateau.(case) in
  let f = tab64.(case) in
  for i = 0 to 7 do
    let dir = vect_roi.(i) in
    let k = ref 1 in
    let s = ref true in
    while (!s && tab120.(f + (!k * dir)) <> (-1)) do
      let candidat = tab120.(f + (!k * dir)) in
      let dest = plateau.(candidat) in
      if dest = 0 then begin
        incr k
      end
      else if co * dest > 0 then begin
        s :=  false
      end
      else begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
        s :=  false
      end
    done
  done

(*Fonction construisant une liste des déplacements possible d'un roi*)
let captures_roi plateau case =
  let liste = ref [] in
  let co = plateau.(case) in
  let r = tab64.(case) in
  for i = 0 to 7 do
    let dir = vect_roi.(i) in
    if tab120.(r + dir) <> (-1) then begin
      let candidat = tab120.(r + dir) in
      let dest = plateau.(candidat) in
      if co * dest < 0 then begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste
      end
    end
  done;
  !liste

(*Fonction construisant une liste des captures/ promotions possible d'un pion*)
let captures_pion plateau case liste =
  let co = plateau.(case) in
  let p = tab64.(case) in
  if co > 0 then begin
    let candidat1 = tab120.(p - 10) in
    if plateau.(candidat1) = 0 && case < 16 then begin
      liste :=
      Promotion {depart = case; arrivee = candidat1; promotion = 5; prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = 4; prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = 3; prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = 2; prise = 0} ::
      !liste
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat3 = tab120.(p - 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 < 0 then begin
        if case > 15 then begin
          liste := Classique {piece = 1; depart = case; arrivee = candidat3; prise = dest3} :: !liste
        end
        else begin
          liste :=
          Promotion {depart = case; arrivee = candidat3; promotion = 5; prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = 4; prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = 3; prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = 2; prise = dest3} ::
          !liste
        end
      end
    end;
    if (case mod 8 <> 0) then begin
      let candidat4 = tab120.(p - 11) in
      let dest4 = plateau.(candidat4) in
      if dest4 < 0 then begin
        if case > 15 then begin
          liste := Classique {piece = 1; depart = case; arrivee = candidat4; prise = dest4} :: !liste
        end
        else begin
          liste :=
          Promotion {depart = case; arrivee = candidat4; promotion = 5; prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = 4; prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = 3; prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = 2; prise = dest4} ::
          !liste
        end
      end
    end
  end
  else begin
    let candidat1 = tab120.(p + 10) in
    if plateau.(candidat1) = 0 && case > 47 then begin
      liste :=
      Promotion {depart = case; arrivee = candidat1; promotion = (-5); prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = (-4); prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = (-3); prise = 0} ::
      Promotion {depart = case; arrivee = candidat1; promotion = (-2); prise = 0} :: !liste
    end;
    if (case mod 8 <> 0) then begin
      let candidat3 = tab120.(p + 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 > 0 then begin
        if case < 48 then begin
          liste := Classique {piece = (-1); depart = case; arrivee = candidat3; prise = dest3} :: !liste
        end
        else begin
          liste :=
          Promotion {depart = case; arrivee = candidat3; promotion = (-5); prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = (-4); prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = (-3); prise = dest3} ::
          Promotion {depart = case; arrivee = candidat3; promotion = (-2); prise = dest3} ::
          !liste
        end
      end
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat4 = tab120.(p + 11) in
      let dest4 = plateau.(candidat4) in
      if dest4 > 0 then begin
        if case < 48 then begin
          liste := Classique {piece = (-1); depart = case; arrivee = candidat4; prise = dest4} :: !liste
        end
        else begin
          liste :=
          Promotion {depart = case; arrivee = candidat4; promotion = (-5); prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = (-4); prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = (-3); prise = dest4} ::
          Promotion {depart = case; arrivee = candidat4; promotion = (-2); prise = dest4} :: !liste
        end
      end
    end
  end

let tabfun_captures = [|captures_pion; captures_cavalier; captures_fou; captures_tour; captures_dame|]

(*Fonction construisant une liste des déplacements classique possibles d'un joueur*)
let deplacements_captures plateau trait_aux_blancs position_roi =
  let liste_coups = ref [] in
  let liste_coups_roi = captures_roi plateau position_roi in
  if trait_aux_blancs then begin
    let aux depart arrive =
      for i = depart downto arrive do
        let piece = plateau.(i) in
        if piece > 0 then begin
          (tabfun_captures.(piece - 1) plateau i liste_coups)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [63, position_roi + 1; position_roi - 1, 0]
  end
  else begin
    let aux depart arrivee =
      for i = depart to arrivee do
        let piece = plateau.(i) in
        if piece < 0 then begin
          (tabfun_captures.(- piece - 1) plateau i liste_coups)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, position_roi - 1; position_roi + 1, 63]
  end;
  !liste_coups, liste_coups_roi

(*Fonction construisant une liste des déplacements classique possibles d'un joueur*)
let deplacements_captures_clouage plateau trait_aux_blancs position_roi piece_clouees =
  let liste_coups = ref [] in
  let liste_coups_roi = captures_roi plateau position_roi in
  let liste_coups_clouees = ref [] in
  if trait_aux_blancs then begin
    let aux depart arrivee = 
    for i = depart downto arrivee do
      if not (List.mem i piece_clouees) then begin
        let piece = plateau.(i) in
        if piece > 0 then begin
          (tabfun_captures.(piece - 1) plateau i liste_coups)
        end
      end
      else begin
        let piece = plateau.(i) in (tabfun_captures.(piece - 1) plateau i liste_coups_clouees)
      end
    done in List.iter (fun (a, b) -> aux a b) [63, position_roi + 1; position_roi - 1, 0]
  end
  else begin
    let aux depart arrivee =
      for i = depart to arrivee do
        if not (List.mem i piece_clouees) then begin
          let piece = plateau.(i) in
          if piece < 0 then begin
            (tabfun_captures.(- piece - 1) plateau i liste_coups)
          end
        end
        else begin
          let piece = plateau.(i) in (tabfun_captures.(- piece - 1) plateau i liste_coups_clouees)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, position_roi - 1; position_roi + 1, 63]
  end;
  !liste_coups, liste_coups_roi, !liste_coups_clouees

let captures plateau trait_aux_blancs dernier_coup =
  let liste_coups = ref [] in
  let roi_joueur = roi trait_aux_blancs in
  let position_roi = index_tableau plateau roi_joueur in
  let aux coup position_roi =
    joue plateau coup;
    if not (menacee plateau position_roi trait_aux_blancs) then begin
      liste_coups := coup :: !liste_coups
    end;
    dejoue plateau coup
  in List.iter (fun prise_en_passant -> aux prise_en_passant position_roi) (enpassant plateau trait_aux_blancs dernier_coup);
  if menacee plateau position_roi trait_aux_blancs then begin
    let coups, coups_roi = deplacements_captures plateau trait_aux_blancs position_roi in
    List.iter (fun coup_roi -> aux coup_roi (arrivee coup_roi)) coups_roi;
    List.iter (fun coup_autre -> aux coup_autre position_roi) coups;
    !liste_coups
  end
  else begin
    let piece_clouees = clouees plateau position_roi trait_aux_blancs in
    if piece_clouees = [] then begin
      let coups, coups_roi = deplacements_captures plateau trait_aux_blancs position_roi in
      List.iter (fun coup_roi -> aux coup_roi (arrivee coup_roi)) coups_roi;
      !liste_coups @ coups
    end
    else begin
      let coups, coups_roi, coups_clouees = deplacements_captures_clouage plateau trait_aux_blancs position_roi piece_clouees in
      List.iter (fun coup_roi -> aux coup_roi (arrivee coup_roi)) coups_roi;
      List.iter (fun coup_clouees -> aux coup_clouees position_roi) coups_clouees;
     !liste_coups @ coups
    end
  end

let smaller_attaquer plateau case trait_aux_blancs =
  let coup = ref Aucun in
  let b = ref false in
  let m = tab64.(case) in
  let piece = plateau.(case) in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  let vect_pion = [|(-9) * signe_joueur; (-11) * signe_joueur|] in
  let i = ref 0 in
  while (not !b && !i < 2) do
    let dir = vect_pion.(!i) in
    if tab120.(m + dir) <> (-1) then begin
      let candidat = tab120.(m + dir) in
      if plateau.(candidat) = (- signe_joueur) then begin
        let coup_potentiel = Classique {piece = (- signe_joueur); depart = candidat; arrivee = case; prise = piece} in
        if est_valide plateau coup_potentiel false then begin
          b := true;
          coup := coup_potentiel
        end
      end
    end;
    incr i
  done;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 8) do
      let dir = vect_cavalier.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = (-2) * signe_joueur then begin
          let coup_potentiel = Classique {piece = (-2) * signe_joueur; depart = candidat; arrivee = case; prise = piece} in
          if est_valide plateau coup_potentiel false then begin
            b := true;
            coup := coup_potentiel
          end
        end
      end;
      incr i
    done
  end;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 4) do
      let dir = vect_fou.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(m + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(m + (!k * dir)) in
        let attaquant = plateau.(candidat) in
        let neg_attaquant = attaquant * signe_joueur in
        if attaquant = 0 then begin
          incr k
        end
        else if neg_attaquant > 0 then begin
          s :=  false
        end
        else begin
          if neg_attaquant = (-3) || neg_attaquant = (-5) || (neg_attaquant = (-6) && !k = 1) then begin
            let coup_potentiel = Classique {piece = attaquant; depart = candidat; arrivee = case; prise = piece} in
            if est_valide plateau coup_potentiel false then begin
              if attaquant = -3 then begin
                b := true
              end;
              coup := coup_potentiel
            end
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 4) do
      let dir = vect_tour.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(m + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(m + (!k * dir)) in
        let attaquant = plateau.(candidat) in
        let neg_attaquant = attaquant * signe_joueur in
        if attaquant = 0 then begin
          incr k
        end
        else if neg_attaquant  > 0 then begin
          s :=  false
        end
        else begin
          if neg_attaquant = (-4) || neg_attaquant = (-5) || (neg_attaquant = (-6) && !k = 1) then begin
            let coup_potentiel = Classique {piece = attaquant; depart = candidat; arrivee = case; prise = piece} in
            if est_valide plateau coup_potentiel false then begin
              b := true;
              coup := coup_potentiel
            end
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  !coup

(*Static Exchange Evaluation, examine une série d'échange sur une case donnée*)
let rec see plateau case trait_aux_blancs =
  let value = ref 0 in
  let coup = smaller_attaquer plateau case trait_aux_blancs in
  if coup <> Aucun then begin
    joue plateau coup;
    value := max 0 (tabvalue.(abs (prise coup)) - see plateau case (not trait_aux_blancs));
    dejoue plateau coup
  end;
  !value

(*Tri les coups selon leur potentiel SEE en supprimant ceux dont cette évaluation est négative*)
let tri_see liste plateau trait_aux_blancs =
  begin
    let rec association liste_coups =
      match liste_coups with
      |[] -> []
      |Promotion {depart = _; arrivee; promotion; prise} as coup :: t ->
        joue plateau coup;
        let note = tabvalue.(abs promotion) + tabvalue.(abs prise) - see plateau arrivee trait_aux_blancs in
        dejoue plateau coup;
        if note >= 0 then
          (note, coup) :: association t else association t
      |coup :: t ->
        joue plateau coup;
        let note = tabvalue.(abs (prise coup)) - see plateau (arrivee coup) trait_aux_blancs in
        dejoue plateau coup;
        if note >= 0 then
          (note, coup) :: association t else association t
    in List.map snd (tri_fusion (association liste))
  end

let rec detecte_extension liste_coups = match liste_coups with
  |[] -> []
  |Classique {piece; depart; arrivee; prise} :: t when prise <> 0 -> Classique {piece; depart; arrivee; prise} :: detecte_extension t
  |Promotion x :: t -> Promotion x :: detecte_extension t
  |Enpassant x :: t -> Enpassant x :: detecte_extension t
  |_ :: t -> detecte_extension t

let rec adapte_delta liste_coups = match liste_coups with
  |[] -> 0
  |Promotion _ :: _  -> 8000
  |_ :: t -> adapte_delta t

let compteur_quiescent = ref 0

(*Fonction implémentant la recherche quiescente*)
let rec recherche_quiescente plateau trait_aux_blancs alpha beta evaluation cap profondeur position_roi roi_en_echec = incr compteur_quiescent;
  let delta = evaluation plateau trait_aux_blancs position_roi roi_en_echec alpha beta in
  let best_score = ref delta in
  if profondeur = 0 then begin
    best_score := delta
  end
  else if delta >= beta then begin
    best_score := beta
  end
  else if delta + 9100 + adapte_delta cap < alpha then begin
    best_score := alpha
  end
  else begin
    let cps = ref (tri_see cap plateau trait_aux_blancs) in
    let b = ref true in
    let alpha0 = ref (max delta alpha) in
    while (!b && !cps <> []) do
      let coup = List.hd !cps in
      joue plateau coup;
      cps := List.tl !cps;
      let nouveau_trait = not trait_aux_blancs in
      let position_roi_adverse = index_tableau plateau (roi nouveau_trait) in
      let score = - recherche_quiescente plateau (not trait_aux_blancs) (- beta) (- !alpha0) evaluation (captures plateau nouveau_trait coup) (profondeur - 1) position_roi_adverse (menacee plateau position_roi_adverse nouveau_trait) 
      in if score > !best_score then begin
        best_score := score;
        if score >= beta then begin
          b := false
        end
        else begin
          alpha0 := max !alpha0 score
        end
      end;
      dejoue plateau coup
    done
  end;
  !best_score

(*Fonction permettant d'évaluer un plateau à la profondeur 0*)
let traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier alpha beta =
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  let cp = coups_valides plateau trait_aux_blancs dernier (false, false, false, false)
  in if cp = [] then begin
    if (menacee plateau position_roi trait_aux_blancs) then begin
      (profondeur_initiale - 99999)
    end
    else begin
      0
    end
  end
  else begin
    let cap = detecte_extension cp in
    let roi_en_echec = menacee plateau position_roi trait_aux_blancs in
    if cap = [] then begin
      recherche_quiescente plateau trait_aux_blancs alpha beta evaluation cap 0 position_roi roi_en_echec
    end
    else begin
      recherche_quiescente plateau trait_aux_blancs alpha beta evaluation cap (-1) position_roi roi_en_echec
    end
  end

let rec negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-infinity) in
  let best_move = ref Aucun in
  if !stop_calculating || repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
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
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_quiescent plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
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

let negalphabetime_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation = (*if dernier_coup = Aucun then begin affiche plateau end;*)
  let t = Sys.time () in
  let fx = negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation in
  fx, (Sys.time () -. t)