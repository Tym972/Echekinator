(*Module implémentant les fonctions qui permettent de générer les coups possibles à partir d'une position*)

open Plateau

(*Fonction indiquant si une case occupée par une pièce amie est attaquée par une pièce ennemie*)
let menacee plateau case =
  let b = ref false in
  let m = tab64.(case) in
  if plateau.(case) > 0 then begin
    let vect_pion = [|(-9); (-11)|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = (-1) then begin
          b := true
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
            b := true
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
              b := true
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
              b := true
            end;
            s :=  false
          end
        done;
        incr i
      done
    end
  end
  else if plateau.(case) < 0 then begin
    let vect_pion = [|9; 11|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = 1 then begin
          b := true
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
            b := true
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
              b := true
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
              b := true
            end;
            s :=  false
          end
        done;
        incr i
      done
    end
  end;
  !b

(*Fonction construisant une liste des déplacements possible d'une tour*)
let deplacements_tour plateau case liste =
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
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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

(*Fonction construisant une liste des déplacements possible d'un fou*)
let deplacements_fou plateau case liste =
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
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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
          liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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

(*Fonction construisant une liste des déplacements possible d'un cavalier*)
let deplacements_cavalier plateau case liste =
  let co = plateau.(case) in
  let c = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_cavalier.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
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
        if dest >= 0 then begin
          liste := Classique {piece = (-2); depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end

(*Fonction construisant une liste des déplacements possible d'une dame*)
let deplacements_dame plateau case liste =
  (deplacements_tour plateau case liste);
  (deplacements_fou plateau case liste)

(*Fonction construisant une liste des déplacements possible d'un roi*)
let deplacements_roi plateau case liste =
  let co = plateau.(case) in
  let r = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(r + dir) <> (-1) then begin
        let candidat = tab120.(r + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
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
        if dest >= 0 then begin
          liste := Classique {piece = (-6); depart = case; arrivee = candidat; prise = dest} :: !liste
        end
      end
    done
  end

(*Fonction construisant une liste des déplacements possible d'un pion*)
let deplacements_pion plateau case liste =
  let co = plateau.(case) in
  let p = tab64.(case) in
  if co > 0 then begin
    let candidat1 = tab120.(p - 10) in
    if plateau.(candidat1) = 0 then begin
      if case > 15 then begin
        liste := Classique {piece = 1; depart = case; arrivee = candidat1; prise = 0} :: !liste;
        if (case > 47 && case < 56) then begin
          let candidat2 = tab120.(p - 20) in
          if plateau.(candidat2) = 0 then begin
            liste := Classique {piece = 1; depart = case; arrivee = candidat2; prise = 0} :: !liste
          end
        end
      end
      else begin
        liste := Promotion {depart = case; arrivee = candidat1; promotion = 5; prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = 4; prise = 0} :: 
        Promotion {depart = case; arrivee = candidat1; promotion = 3; prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = 2; prise = 0} :: !liste
      end
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
    if plateau.(candidat1) = 0 then begin
      if case < 48 then begin
        liste := Classique {piece = (-1); depart = case; arrivee = candidat1; prise = 0} :: !liste;
        if (case > 7 && case < 16) then begin
          let candidat2 = tab120.(p + 20) in
          if (plateau.(candidat2) = 0) then begin
            liste := Classique {piece = (-1); depart = case; arrivee = candidat2; prise = 0} :: !liste
          end
        end
      end
      else begin
        liste := Promotion {depart = case; arrivee = candidat1; promotion = (-5); prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = (-4); prise = 0} :: 
        Promotion {depart = case; arrivee = candidat1; promotion = (-3); prise = 0} :: Promotion {depart = case; arrivee = candidat1; promotion = (-2); prise = 0} :: !liste
      end
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

let tabfun = [|deplacements_pion; deplacements_cavalier; deplacements_fou; deplacements_tour; deplacements_dame; deplacements_roi|]

(*Fonction construisant une liste des déplacements classique possibles d'un joueur*)
let deplacements_all plateau trait_aux_blancs =
  let liste = ref [] in
  if trait_aux_blancs then begin
    for i = 63 downto 0 do
      let pl = plateau.(i) in
      if pl > 0 then begin
        (tabfun.(pl - 1) plateau i liste)
      end
    done
  end
  else begin
    for i = 0 to 63 do
      let pl = plateau.(i) in
      if pl < 0 then begin
        (tabfun.(- pl - 1) plateau i liste)
      end
    done
  end;
  !liste

(*Fonction construisant une liste des roques possible d'un joueur*)
let roque plateau trait_aux_blancs droit_au_roque =
  let prb, grb, prn, grn = droit_au_roque in
  let l = ref [] in 
  if trait_aux_blancs then begin
    if (prb || grb) && (plateau.(52) <> (-1)) then begin
      let prp = (prb && plateau.(61) = 0 && plateau.(62) = 0 && plateau.(54) <> (-1) && plateau.(55) <> (-1)) in
      let grp = (grb && plateau.(57) = 0 && plateau.(58) = 0 && plateau.(59) = 0 && plateau.(49) <> (-1) && plateau.(50) <> (-1)) in
      if (prp || grp) then begin
        let dep = deplacements_all plateau false in
        let destinee = Hashtbl.create 64 in
        let rec fonc2 liste = match liste with
          |[] -> ()
          |h::t -> begin match h with
            |Classique {piece = _; depart = _; arrivee; prise = _} |Promotion {depart = _; arrivee; promotion = _; prise = _} -> Hashtbl.add destinee arrivee (); fonc2 t
            |_ -> fonc2 t
          end
        in fonc2 dep;
        if (prp && not (Hashtbl.mem destinee 61 || Hashtbl.mem destinee 62))
          then l := Roque {sorte = 1} :: !l;
        if (grp && not (Hashtbl.mem destinee 58 || Hashtbl.mem destinee 59))
          then l := Roque {sorte = 2} :: !l
      end
    end
  end
  else begin
    if (prn || grn) && (plateau.(12) <> 1) then begin
      let prp = (prn && plateau.(5) = 0 && plateau.(6) = 0 && plateau.(14) <> 1 && plateau.(15) <> 1) in
      let grp = (grn && plateau.(1) = 0 && plateau.(2) = 0 && plateau.(3) = 0 && plateau.(9) <> 1 && plateau.(10) <> 1) in
      if (prp || grp)then begin
        let dep = deplacements_all plateau true in
        let destinee = Hashtbl.create 64 in
        let rec fonc2 liste = match liste with
          |[] -> ()
          |h::t -> begin match h with
            |Classique {piece = _; depart = _; arrivee; prise = _} |Promotion {depart = _; arrivee; promotion = _; prise = _} -> Hashtbl.add destinee arrivee (); fonc2 t
            |_ -> fonc2 t
          end
        in fonc2 dep;
        if (prp && not (Hashtbl.mem destinee 5 || Hashtbl.mem destinee 6))
          then l := Roque {sorte = 3} :: !l;
        if (grp && not (Hashtbl.mem destinee 2 || Hashtbl.mem destinee 3))
          then l := Roque {sorte = 4} :: !l
      end
    end
  end;
  !l

(*Fonction adaptant les droits aux roques en fonction du coup*)
let modification_roque coup (prb, grb, prn, grn) =
  if (prb || grb || prn || grn) then  begin match coup with
    |Classique {depart; piece; arrivee; prise = _} ->
      if piece = 6 then begin
        false, false, prn && arrivee <> 7, grn && arrivee <> 0
      end
      else if piece = (-6) then begin
        prb && arrivee <> 63, grb && arrivee <> 56, false, false
      end
      else begin
        prb && depart <> 63 && arrivee <> 63, grb && depart <> 56 && arrivee <> 56, prn && depart <> 7 && arrivee <> 7, grn && depart <> 0 && arrivee <> 0
      end
    |Roque {sorte} ->
      if sorte = 1 || sorte = 2 then begin
        false, false, prn, grn
      end
      else begin
        prb, grb, false, false
      end
    |Promotion {depart = _; arrivee; promotion = _; prise = _} ->
      prb && arrivee <> 63, grb && arrivee <> 56, prn && arrivee <> 7, grn && arrivee <> 0
    |_ -> prb, grb, prn, grn
  end
  else begin
   (false, false, false, false)
  end

(*Fonction analysant l'historique pour détecter les roques impossibles, en renvoyant un quadruplet de booléens : petit roque blanc, grands roque blanc, petit roque noir, grands roque noir. Non utilisée*)
let rec roques_possibles listes_coups = match listes_coups with
  |[] -> true, true, true, true
  |h :: t ->
    let prb1, grb1, prn1, grn1 = roques_possibles t in
    let prb2, grb2, prn2, grn2 = modification_roque h (prb1, grb1, prn1, grn1) in
    prb1 && prb2, grb1 && grb2, prn1 && prn2, grn1 && grn2

(*Fonction construisant une liste des prises en passant possible d'un joueur*)
let enpassant plateau trait_aux_blancs dernier_coup =
  let l = ref [] in
  if trait_aux_blancs then begin
    let aux coup = match coup with
      |Classique {piece; depart; arrivee; prise = _} when (piece = (-1) && (depart < 16) && (arrivee > 23)) -> arrivee
      |_-> (-1)
    in let arrivee = aux dernier_coup
    in if arrivee <> (-1) then begin
      let droite = arrivee + 1
      in if (droite <> 32 && plateau.(droite) = 1) then begin
        l := Enpassant {depart = droite; arrivee = droite - 9} :: !l
      end;
      let gauche = arrivee - 1
      in if (gauche <> 23 && plateau.(gauche) = 1) then begin
        l := Enpassant {depart = gauche; arrivee = gauche - 7} :: !l
      end;
    end
  end
  else begin
    let aux coup = match coup with
      |Classique {piece; depart; arrivee; prise = _} when (piece = 1 && (depart > 47) && (arrivee < 40)) -> arrivee
      |_-> (-1)
    in let arrivee = aux dernier_coup
    in if arrivee <> (-1) then begin
      let droite = arrivee + 1
      in if (droite <> 40 && plateau.(droite) = (-1)) then begin
        l := Enpassant {depart = droite; arrivee = droite + 7} :: !l
      end;
      let gauche = arrivee - 1
      in if (gauche <> 31 && plateau.(gauche) = (-1)) then begin
        l := Enpassant {depart = gauche; arrivee = gauche + 9} :: !l
      end;
    end
  end;
  !l

(*Fonction permettant de jouer un coup sur l'échiquier*)
let joue plateau coup = match coup with
  |Classique {piece; depart; arrivee; prise = _} -> begin
    plateau.(depart) <- 0;
    plateau.(arrivee) <- piece
  end
  |Roque {sorte} -> begin
    match sorte with
    |1 -> plateau.(60) <- 0; plateau.(62) <- 6; plateau.(63) <- 0; plateau.(61) <- 4
    |2 -> plateau.(60) <- 0; plateau.(58) <- 6; plateau.(56) <- 0; plateau.(59) <- 4
    |3 -> plateau.(4) <- 0; plateau.(6) <- (-6); plateau.(7) <- 0; plateau.(5) <- (-4)
    |_ -> plateau.(4) <- 0; plateau.(2) <- (-6); plateau.(0) <- 0; plateau.(3) <- (-4)
  end
  |Enpassant {depart; arrivee} -> begin
    if depart < 32 then begin
      plateau.(depart) <- 0;
      plateau.(arrivee) <- 1;
      plateau.(arrivee + 8) <- 0
    end
    else begin
      plateau.(depart) <- 0;
      plateau.(arrivee) <- (-1);
      plateau.(arrivee - 8) <- 0
    end
  end
  |Promotion {depart; arrivee; promotion; prise = _} -> begin
    plateau.(depart) <- 0;
    plateau.(arrivee) <- promotion
  end
  |Aucun -> ()

(*Fonction permettant l'annulation d'un coup*)
let dejoue plateau coup = match coup with
  |Classique {piece; depart; arrivee; prise} -> begin
    plateau.(depart) <- piece;
    plateau.(arrivee) <- prise
  end
  |Roque {sorte} -> begin
    match sorte with
    |1 -> plateau.(60) <- 6; plateau.(62) <- 0; plateau.(63) <- 4; plateau.(61) <- 0
    |2 -> plateau.(60) <- 6; plateau.(58) <- 0; plateau.(56) <- 4; plateau.(59) <- 0
    |3 -> plateau.(4) <- (-6); plateau.(6) <- 0; plateau.(7) <- (-4); plateau.(5) <- 0
    |_ -> plateau.(4) <- (-6); plateau.(2) <- 0; plateau.(0) <- (-4); plateau.(3) <- 0
  end
  |Enpassant {depart; arrivee} -> begin
    if depart < 32 then begin
      plateau.(depart) <- 1;
      plateau.(arrivee) <- 0;
      plateau.(arrivee + 8) <- (-1)
    end
    else begin
      plateau.(depart) <- (-1);
      plateau.(arrivee) <- 0;
      plateau.(arrivee - 8) <- 1
    end
  end
  |Promotion {depart; arrivee; promotion; prise} -> begin
    plateau.(depart) <- if promotion > 0 then 1 else (-1);
    plateau.(arrivee) <- prise
  end
  |Aucun -> ()

(*Fonction permettant de jouer un coup en actualisant les variables d'états de la partie*)
let joue_coup_1 plateau coup trait_aux_blancs dernier_coup droit_au_roque = 
  joue plateau coup;
  droit_au_roque := modification_roque coup !droit_au_roque;
  dernier_coup := coup;
  trait_aux_blancs := not !trait_aux_blancs

(*Fonction donnant la case de départ d'un coup classique et d'une promotion*)
let depart coup = match coup with
  |Classique {piece = _; depart; arrivee = _; prise = _} | Promotion {depart; arrivee = _; prise = _; promotion = _} -> depart
  |_ -> (-1)

(*Fonction donnant la prise d'un coup*)
let prise coup = match coup with
  |Classique {piece = _; depart = _; arrivee = _; prise} | Promotion {depart = _; arrivee = _; prise; promotion = _} -> prise
  |_ -> (-1)

(*Fonction donnant la pièce d'un coup classique*)
let piece coup = match coup with
  |Classique {piece; depart = _; arrivee = _; prise = _} -> piece
  |_ -> 0

(*Fonction donnant la case d'arrivée d'un coup*)
let arrivee coup = match coup with
  |Classique {piece = _; depart = _; arrivee; prise = _} | Promotion {depart = _; arrivee; prise = _; promotion = _} | Enpassant {depart = _; arrivee} -> arrivee
  |_ -> (-1)

(*Fonction indiquant si un coup est une prise en passant*)
let est_en_passant coup = match coup with
  |Enpassant _ -> true
  |_ -> false

(*Fonction indiquant si une pièce cloue une pièce clouable*)
let cloue plateau chessman joueur cord64 vect distance =
  let b = ref false in
  let k = ref distance in
  let s = ref true in
  if joueur = 1 then begin
    while (tab120.(cord64 + (!k * vect)) <> (-1) && !s) do
      let candidat = tab120.(cord64 + (!k * vect)) in
      let dest = plateau.(candidat) in
      if dest > 0 then begin
        s :=  false
      end
      else if dest < 0 then begin 
        if dest = chessman || dest = (-5) then begin
          b := true;
        end;
        s :=  false
      end;
      incr k
    done
  end
  else begin 
    while (tab120.(cord64 + (!k * vect)) <> (-1) && !s) do
      let candidat = tab120.(cord64 + (!k * vect)) in
      let dest = plateau.(candidat) in
      if dest < 0 then begin
        s :=  false
      end
      else if dest > 0 then begin 
        if dest = chessman || dest = 5 then begin
          b := true;
        end;
        s :=  false
      end;
      incr k
    done
  end;
  !b

(*Fonction donnant les cases des pièces clouées (indépendanmment de leur possibilité de mouvement) en considérant que le roi est dans la case en argument*)
let clouees plateau case =
  let ensemble = ref [] in
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
        if dest > 0 then begin
          if cloue plateau (-4) 1 t dir (!k + 1) then begin
            ensemble := candidat :: !ensemble
          end;
          s :=  false
        end
        else if dest < 0 then begin 
          s :=  false
        end;
        incr k
      done
    done;
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest > 0 then begin
          if cloue plateau (-3) 1 t dir (!k + 1) then begin
            ensemble := candidat :: !ensemble
          end;
          s :=  false
        end
        else if dest < 0 then begin 
          s :=  false
        end;
        incr k
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
        if dest < 0 then begin
          if cloue plateau 4 2 t dir (!k + 1) then begin
            ensemble := candidat :: !ensemble
          end;
          s :=  false
        end
        else if dest > 0 then begin
          s :=  false
        end;
        incr k
      done
    done;
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest < 0 then begin
          if cloue plateau 3 2 t dir (!k + 1) then begin
            ensemble := candidat :: !ensemble
          end;
          s :=  false
        end
        else if dest > 0 then begin
          s :=  false
        end;
        incr k
      done
    done
  end;
  !ensemble

(*Fonction construisant une liste des coups légaux du joueur*)  
let coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque =
  let l = ref [] in
  let cp = ref ((enpassant plateau trait_aux_blancs dernier_coup) @ (deplacements_all plateau trait_aux_blancs)) in
  let roi_joueur = roi trait_aux_blancs in
  let position_roi = index plateau roi_joueur in
  if menacee plateau position_roi then begin
    while !cp <> [] do
      let coup = List.hd !cp in
      joue plateau coup;
      if piece coup = roi_joueur then begin
        if not (menacee plateau (arrivee coup)) then begin
          l := coup :: !l
        end
      end
      else begin
        if not (menacee plateau position_roi) then begin
          l := coup :: !l
        end
      end;
      cp := List.tl !cp;
      dejoue plateau coup
    done;
    !l
  end
  else begin
    let piece_clouees = clouees plateau position_roi in
    while !cp <> [] do
      let coup = List.hd !cp in
      if piece coup = roi_joueur then begin
        joue plateau coup;
        if not (menacee plateau (arrivee coup)) then begin
          l := coup :: !l
        end;
        dejoue plateau coup
      end
      else begin
        if List.mem (depart coup) piece_clouees || est_en_passant coup then begin
          joue plateau coup;
          if not (menacee plateau position_roi) then begin
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
    (roque plateau trait_aux_blancs droit_au_roque) @ !l
  end

(*Fonction indiquant si un coup est valide*)
let est_valide plateau coup joueur =
  let b = ref true in
  joue plateau coup;
  if piece coup = (roi joueur) then begin
    if (menacee plateau (arrivee coup)) then begin
      b := false
    end
  end
  else if (menacee plateau (index plateau (roi joueur))) then begin
    b := false
  end;
  dejoue plateau coup;
  !b

(*Fonction indiquant efficacement si un coup est valide, utilisée uniquement pour des coups classiques, hors roi*)
let est_valide_efficace plateau coup position_roi roi_en_echec piece_clouees =
  let b = ref true in
  if roi_en_echec then begin
    joue plateau coup;
    if (menacee plateau position_roi) then begin
      b := false
    end;
    dejoue plateau coup
  end
  else if List.mem (depart coup) piece_clouees then begin
    joue plateau coup;
    if menacee plateau position_roi then begin
      b := false
    end;
    dejoue plateau coup
  end;
  !b

(*Fonction renvoyant le statut de la partie (2 si elle est en cours, 0 si il y a pat, 1 si les blancs l'emportent, -1 si les noirs l'emportent)*)
let gagne plateau trait_aux_blancs dernier_coup =
  let vainqueur = ref 2 in
  if trait_aux_blancs then begin
    if (coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)) = [] then begin
      if menacee plateau (index plateau 6) then
        vainqueur := -1
      else
        vainqueur := 0
    end
  end
  else begin
    if (coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)) = [] then begin
      if menacee plateau (index plateau (-6)) then
        vainqueur := 1
      else
        vainqueur := 0
    end
  end;
  !vainqueur

(*Tableau dont les élément indiquent si la présence de la pièce d'indice correspondant est imcompatible avec la nulle par manque de matériel*)
let tab_manque_de_materiel = [|false; true; false; false; true; true; false|]

(*Fonction indiquant si une case est blanche*)
let est_blanche case =
  ((case / 8) mod 2 = 0 && (case mod 2) = 0) || ((case / 8) mod 2 = 1 && (case mod 2) = 1)

(*Fonction servant à cloturer la partie si un mat est strictement impossible. 4 cas sont considérés : roi contre roi, roi et fou contre roi, roi et cavalier contre roi et roi et fou contre roi et fou avec fous de même couleur*)
let manque_de_materiel plateau =
  let b = ref true in
  let compteur = ref 0 in
  let cavaliers_blancs = ref 0 in
  let cavaliers_noirs = ref 0 in
  let fous_blancs_cb = ref 0 in
  let fous_noirs_cb = ref 0 in
  let i = ref 0 in
  while (!b && !i < 64) do
    let case = plateau.(!i) in
    if case <> 0 then begin
      if tab_manque_de_materiel.(abs case) then begin
        b := false
      end
      else if abs case <> 6 then begin
        compteur := !compteur + 1;
        if !compteur > 2 then begin
          b := false
        end
        else begin
          match case with
            |2 -> cavaliers_blancs := !cavaliers_blancs + 1
            |(-2) -> cavaliers_noirs := !cavaliers_noirs + 1
            |3 -> if est_blanche !i then fous_blancs_cb := !fous_blancs_cb + 1
            |(-3) -> if est_blanche !i then fous_noirs_cb := !fous_noirs_cb + 1
            |_ -> ()
        end
      end
    end;
    incr i
  done;
  if !b then begin
    if !compteur = 2 then begin
      if (!cavaliers_blancs + !cavaliers_noirs > 0) then begin
        b := false
      end
      else if (!fous_blancs_cb <> !fous_noirs_cb) then begin
        b := false
      end
    end
  end;
  !b