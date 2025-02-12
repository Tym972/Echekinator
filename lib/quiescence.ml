(*Module implémentant les fonctions permettant la recherche quiescente*)

open Plateau
open Generateur
open Strategie1
open Evaluations

(*Fonction construisant une liste des captures possible d'une tour*)
let captures_tour plateau case liste =
  let co = plateau.(case) in
  let t = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 3 do
      let dir = vect_tour.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin 
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
          s :=  false
        end
      done
    done
  end
  else begin
    for i = 0 to 3 do
      let dir = vect_tour.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          incr k
        end
        else if dest < 0 then begin
          s :=  false
        end
        else begin 
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
          s :=  false
        end
      done
    done
  end

(*Fonction construisant une liste des captures possible d'un fou*)
let captures_fou plateau case liste =
  let co = plateau.(case) in
  let f = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(f + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(f + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
          s :=  false
        end
      done
    done
  end
  else begin
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(f + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(f + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          incr k
        end
        else if dest < 0 then begin
          s :=  false
        end
        else begin
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste;
          s :=  false
        end
      done
    done
  end

(*Fonction construisant une liste des captures possible d'un cavalier*)
let captures_cavalier plateau case liste =
  let co = plateau.(case) in
  let c = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_cavalier.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest < 0 then begin
          liste := Classique {piece = 2; depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = vect_cavalier.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest > 0 then begin
          liste := Classique {piece = (-2); depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end

(*Fonction construisant une liste des captures possible d'une dame*)
let captures_dame plateau case liste =
  (captures_tour plateau case liste);
  (captures_fou plateau case liste)

(*Fonction construisant une liste des captures possible d'un roi*)
let captures_roi plateau case liste =
  let co = plateau.(case) in
  let r = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(r + dir) <> (-1) then begin
        let candidat = tab120.(r + dir) in
        let dest = plateau.(candidat) in
        if dest < 0 then begin
          liste := Classique {piece = 6; depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(r + dir) <> (-1) then begin
        let candidat = tab120.(r + dir) in
        let dest = plateau.(candidat) in
        if dest > 0 then begin
          liste := Classique {piece = (-6); depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end

(*Fonction construisant une liste des captures/ promotions possible d'un pion*)
let captures_pion plateau case liste =
  let co = plateau.(case) in
  let p = tab64.(case) in
  if co > 0 then begin
    let candidat1 = tab120.(p - 10) in
    if plateau.(candidat1) = 0 && case < 16 then begin
      liste := Promotion {depart = case; arrivee = candidat1; promotion = 5; prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = 4; prise = 0} :: 
      Promotion {depart = case; arrivee = candidat1; promotion = 3; prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = 2; prise = 0} :: !liste
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat3 = tab120.(p - 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 < 0 then begin
        if case > 15 then begin
          liste := Classique {piece = 1; depart = case; arrivee = candidat3; prise = dest3} :: !liste
        end
        else begin
          liste := Promotion {depart = case; arrivee = candidat3; promotion = 5; prise = dest3} :: Promotion {depart = case; arrivee = candidat3; promotion = 4; prise = dest3} :: 
          Promotion {depart = case; arrivee = candidat3; promotion = 3; prise = dest3} :: Promotion {depart = case; arrivee = candidat3; promotion = 2; prise = dest3} :: !liste
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
          liste := Promotion {depart = case; arrivee = candidat4; promotion = 5; prise = dest4} :: Promotion {depart = case; arrivee = candidat4; promotion = 4; prise = dest4} :: 
          Promotion {depart = case; arrivee = candidat4; promotion = 3; prise = dest4} :: Promotion {depart = case; arrivee = candidat4; promotion = 2; prise = dest4} :: !liste
        end
      end
    end
  end
  else begin
    let candidat1 = tab120.(p + 10) in
    if plateau.(candidat1) = 0 && case > 47 then begin
      liste := Promotion {depart = case; arrivee = candidat1; promotion = (-5); prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = (-4); prise = 0} :: 
      Promotion {depart = case; arrivee = candidat1; promotion = (-3); prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = (-2); prise = 0} :: !liste
    end;
    if (case mod 8 <> 0) then begin
      let candidat3 = tab120.(p + 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 > 0 then begin
        if case < 48 then begin
          liste := Classique {piece = (-1); depart = case; arrivee = candidat3; prise = dest3} :: !liste
        end
        else begin
          liste := Promotion {depart = case; arrivee = candidat3; promotion = (-5); prise = dest3} :: Promotion {depart = case; arrivee = candidat3; promotion = (-4); prise = dest3} :: 
          Promotion {depart = case; arrivee = candidat3; promotion = (-3); prise = dest3} :: Promotion {depart = case; arrivee = candidat3; promotion = (-2); prise = dest3} :: !liste
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
          liste := Promotion {depart = case; arrivee = candidat4; promotion = (-5); prise = dest4} :: Promotion {depart = case; arrivee = candidat4; promotion = (-4); prise = dest4} :: 
          Promotion {depart = case; arrivee = candidat4; promotion = (-3); prise = dest4} :: Promotion {depart = case; arrivee = candidat4; promotion = (-2); prise = dest4} :: !liste
        end
      end
    end
  end

let tabfun_captures = [|captures_pion; captures_cavalier; captures_fou; captures_tour; captures_dame; captures_roi|]

(*Fonction construisant une liste des captures et promotions pseudo légales d'un joueur*)
let pseudo_captures plateau trait_aux_blancs =
  let l = ref [] in
  if trait_aux_blancs then begin
    for i = 63 downto 0 do
      let pl = plateau.(i) in
      if pl > 0 then begin
        (tabfun_captures.(pl - 1) plateau i l)
      end
    done
  end
  else begin
    for i = 0 to 63 do
      let pl = plateau.(i) in
      if pl < 0 then begin
        (tabfun_captures.(- pl - 1) plateau i l)
      end
    done
  end;
  !l

(*Fonction construisant une liste des captures et promotions légales d'un joueur*)
let captures plateau trait_aux_blancs dernier =
  let l = ref [] in
  let cp = ref ((enpassant plateau trait_aux_blancs dernier) @ pseudo_captures plateau trait_aux_blancs) in
  let roi_joueur = roi trait_aux_blancs in
  let position_roi = index_tableau plateau roi_joueur in
  if menacee plateau position_roi trait_aux_blancs then begin
    while !cp <> [] do
      let coup = List.hd !cp in
      joue plateau coup;
      if piece coup = roi_joueur then begin
        if not (menacee plateau (arrivee coup) trait_aux_blancs) then begin
          l := coup :: !l
        end
      end
      else begin
        if not (menacee plateau position_roi trait_aux_blancs) then begin
          l := coup :: !l
        end
      end;
      cp := List.tl !cp;
      dejoue plateau coup
    done;
    !l
  end
  else begin
    let piece_clouees = clouees plateau position_roi trait_aux_blancs in
    while !cp <> [] do
      let coup = List.hd !cp in
      if piece coup = roi_joueur then begin
        joue plateau coup;
        if not (menacee plateau (arrivee coup) trait_aux_blancs) then begin
          l := coup :: !l
        end;
        dejoue plateau coup
      end
      else begin
        if List.mem (depart coup) piece_clouees then begin
          joue plateau coup;
          if not (menacee plateau position_roi trait_aux_blancs) then begin
            l := coup :: !l
          end;
          dejoue plateau coup
        end
        else begin
          l := coup :: !l
        end
      end;
      cp := List.tl !cp
    done;
    !l
  end

let smaller_attaquer plateau case =
  let coup = ref Aucun in
  let b = ref false in
  let m = tab64.(case) in
  let piece = plateau.(case) in
  if piece > 0 then begin
    let vect_pion = [|(-9); (-11)|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = (-1) then begin
          let coup_potentiel = Classique {piece = -1; depart = candidat; arrivee = case; prise = piece} in
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
          if plateau.(candidat) = (-2) then begin
            let coup_potentiel = Classique {piece = -2; depart = candidat; arrivee = case; prise = piece} in
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
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            s :=  false
          end
          else begin
            if dest = (-3) || dest = (-5) || (dest = (-6) && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel false then begin
                if dest = -3 then begin
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
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            s :=  false
          end
          else begin
            if dest = (-4) || dest = (-5) || (dest = (-6) && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
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
    end
  end
  else if piece < 0 then begin
    let vect_pion = [|9; 11|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = 1 then begin
          let coup_potentiel = Classique {piece = 1; depart = candidat; arrivee = case; prise = piece} in
          if est_valide plateau coup_potentiel true then begin
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
          if plateau.(candidat) = 2 then begin
            let coup_potentiel = Classique {piece = 2; depart = candidat; arrivee = case; prise = piece} in
            if est_valide plateau coup_potentiel true then begin
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
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest < 0 then begin
            s :=  false
          end
          else begin
            if dest = 3 || dest = 5 || (dest = 6 && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel true then begin
                if dest = 3 then begin
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
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest < 0 then begin
            s :=  false
          end
          else begin
            if dest = 4 || dest = 5 || (dest = 6 && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel true then begin
                b := true;
                coup := coup_potentiel
              end
            end;
            s :=  false
          end
        done;
        incr i
      done
    end
  end;
  !coup

let rec see plateau case =
  let value = ref 0 in
  let coup = (smaller_attaquer plateau case) in
  if coup <> Aucun then begin
    joue plateau coup;
    value := max 0 (tabvalue.(abs (prise coup) - 1) - see plateau case);
    dejoue plateau coup
  end;
  !value

let tri_see liste plateau =
  if List.length liste < 2 then begin 
    liste
  end
  else begin
    let rec association liste_coups =
      match liste_coups with
      |[] -> []
      |Classique {piece; depart; arrivee; prise} :: t ->
        joue plateau (Classique {piece; depart; arrivee; prise});
        let note = see plateau (arrivee) in
        dejoue plateau (Classique {piece; depart; arrivee; prise});
        ((tabvalue.(abs prise) - note), Classique {piece; depart; arrivee; prise}) :: association t
      |Enpassant {depart; arrivee} :: t ->
        joue plateau (Enpassant {depart; arrivee});
        let note = see plateau (arrivee) in
        dejoue plateau (Enpassant {depart; arrivee});
        ((tabvalue.(1) - note), Enpassant {depart; arrivee}) :: association t
      |Promotion {depart; arrivee; promotion; prise} :: t ->
        joue plateau (Promotion {depart; arrivee; promotion; prise});
        let note = see plateau (arrivee) in
        dejoue plateau (Promotion {depart; arrivee; promotion; prise});
        ((tabvalue.(abs promotion) + tabvalue.(abs prise) - note), Promotion {depart; arrivee; promotion; prise}) :: association t
      |h :: t -> (0, h) :: association t
    in List.map snd (tri_fusion (association liste))
  end

let rec detecte_captures listes_coups = match listes_coups with
  |[] -> []
  |Classique {piece; depart; arrivee; prise} :: t when prise <> 0 -> Classique {piece; depart; arrivee; prise} :: detecte_captures t
  |Promotion x :: t -> Promotion x :: detecte_captures t
  |Enpassant x :: t -> Enpassant x :: detecte_captures t
  |_ :: t -> detecte_captures t

let compteur_quiescent = ref 0

(*Fonction implémentant la recherche quiescente*)
let rec recherche_quiescente plateau trait_aux_blancs alpha beta evaluation cap profondeur = incr compteur_quiescent;
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  let roi_en_echec = (menacee plateau position_roi trait_aux_blancs) in
  let best_score = ref (-99999) in
  let delta = evaluation plateau trait_aux_blancs position_roi roi_en_echec alpha beta in
  if profondeur = 0 then begin
    best_score := delta
  end
  else if delta >= beta then begin
    best_score := beta
  end
  else if delta + 9000 < alpha then begin
    best_score := delta
  end
  else begin
    let cps = ref (mvvlva cap) in
    if !cps = [] then begin
      best_score := delta
    end
    else begin
      let b = ref true in
      let alpha0 = ref (max delta alpha) in
      best_score := !alpha0;
      while (!b && !cps <> []) do
        let coup = List.hd !cps in
        joue plateau coup;
        cps := List.tl !cps;
        let score = - recherche_quiescente plateau (not trait_aux_blancs) (- beta) (- !alpha0) evaluation (captures plateau (not trait_aux_blancs) coup) (profondeur - 1)
        in if score > !best_score then begin
          best_score := score;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score

(*Fonction permettant d'évaluer un plateau à la profondeur 0*)
let traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier alpha beta =
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  let cp = coups_valides plateau trait_aux_blancs dernier (false, false, false, false)
  in if cp = [] then begin
    if (menacee plateau position_roi trait_aux_blancs) then begin
      (- 99950)
    end
    else begin
      0
    end
  end
  else begin
    let cap = detecte_captures cp in
    if cap = [] then begin
      evaluation plateau trait_aux_blancs position_roi (menacee plateau position_roi trait_aux_blancs) alpha beta
    end
    else begin
      recherche_quiescente plateau trait_aux_blancs alpha beta evaluation cap 1000
    end
  end

let rec negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
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

let negalphabetime_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation = (*if dernier_coup = Aucun then begin affiche plateau end;*)
  let t = Sys.time () in
  let fx = negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)