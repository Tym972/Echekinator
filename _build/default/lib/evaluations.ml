(*Module implémentant des fonctions d'évaluation*)

open Plateau
open Generateur

let mobilite_tour plateau case compteur =
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
          incr compteur;
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin 
          incr compteur;
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
          incr compteur;
          incr k
        end
        else if dest < 0 then begin
          s :=  false
        end
        else begin 
          incr compteur;
          s :=  false
        end
      done
    done
  end

(*Fonction construisant une liste des déplacements possible d'un fou*)
let mobilite_fou plateau case compteur =
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
          incr compteur;
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin
          incr compteur;
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
          incr compteur;
          incr k
        end
        else if dest < 0 then begin
          s :=  false
        end
        else begin
          incr compteur;
          s :=  false
        end
      done
    done
  end

(*Fonction construisant une liste des déplacements possible d'un cavalier*)
let mobilite_cavalier plateau case compteur =
  let co = plateau.(case) in
  let c = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_cavalier.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
          incr compteur
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
          incr compteur
        end
      end
    done
  end

(*Fonction construisant une liste des déplacements possible d'une dame*)
let mobilite_dame plateau case compteur =
  (mobilite_tour plateau case compteur);
  (mobilite_fou plateau case compteur)

(*Fonction construisant une liste des déplacements possible d'un roi*)
let mobilite_roi plateau case compteur =
  let co = plateau.(case) in
  let r = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(r + dir) <> (-1) then begin
        let candidat = tab120.(r + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
          incr compteur
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
          incr compteur
        end
      end
    done
  end

(*Fonction construisant une liste des déplacements possible d'un pion*)
let mobilite_pion plateau case compteur =
  let co = plateau.(case) in
  let p = tab64.(case) in
  if co > 0 then begin
    let candidat1 = tab120.(p - 10) in
    if plateau.(candidat1) = 0 then begin
      if case > 15 then begin
        incr compteur;
        if (case > 47 && case < 56) then begin
          let candidat2 = tab120.(p - 20) in
          if plateau.(candidat2) = 0 then begin
            incr compteur
          end
        end
      end
      else begin
        compteur := !compteur + 4
      end
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat3 = tab120.(p - 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 < 0 then begin
        if case > 15 then begin
          incr compteur
        end
        else begin
          compteur := !compteur + 4
        end
      end
    end;
    if (case mod 8 <> 0) then begin
      let candidat4 = tab120.(p - 11) in
      let dest4 = plateau.(candidat4) in
      if dest4 < 0 then begin
        if case > 15 then begin
          incr compteur
        end
        else begin
          compteur := !compteur + 4
        end
      end
    end
  end
  else begin
    let candidat1 = tab120.(p + 10) in
    if plateau.(candidat1) = 0 then begin
      if case < 48 then begin
        incr compteur;
        if (case > 7 && case < 16) then begin
          let candidat2 = tab120.(p + 20) in
          if (plateau.(candidat2) = 0) then begin
            incr compteur
          end
        end
      end
      else begin
        compteur := !compteur + 4
      end
    end;
    if (case mod 8 <> 0) then begin
      let candidat3 = tab120.(p + 9) in
      let dest3 = plateau.(candidat3) in
      if dest3 > 0 then begin
        if case < 48 then begin
          incr compteur
        end
        else begin
          compteur := !compteur + 4
        end
      end
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat4 = tab120.(p + 11) in
      let dest4 = plateau.(candidat4) in
      if dest4 > 0 then begin
        if case < 48 then begin
          incr compteur
        end
        else begin
          compteur := !compteur + 4
        end
      end
    end
  end

let tab_mobilite = [|mobilite_pion; mobilite_cavalier; mobilite_fou; mobilite_tour; mobilite_dame; mobilite_roi|]

let mobilite plateau trait_aux_blancs =
  let compteur = ref 0 in
  if trait_aux_blancs then begin
    for i = 63 downto 0 do
      let pl = plateau.(i) in
      if pl > 0 then begin
        (tab_mobilite.(pl - 1) plateau i compteur)
      end
    done
  end
  else begin
    for i = 0 to 63 do
      let pl = plateau.(i) in
      if pl < 0 then begin
        (tab_mobilite.(- pl - 1) plateau i compteur)
      end
    done
  end;
  !compteur

(*Fonction indiquant le nombre de case séparant un pion passé de la promotion (elle renvoie 0 si le pion n'est pas passé)*)
let pion_passe plateau case =
  let compteur = ref 0 in
  let b = ref true in
  let co = plateau.(case) in
  let t = tab64.(case) in
  let droite = ref false in
  let gauche = ref false in
  if co > 0 then begin
    if tab120.(t - 9) <> (-1) then begin
      droite := true
    end;
    if tab120.(t - 11) <> (-1) then begin
      gauche := true
    end;
    let k = ref 1 in
    while (tab120.(t + (!k * (-10))) <> (-1) && !b) do
      compteur := !compteur + 1;
      let candidat1 = tab120.(t + (!k * (-10))) in
      let dest1 = plateau.(candidat1) in
      if dest1 = (-1) then begin
        b := false
      end;
      if (!b && !gauche) then begin
        let candidat2 = tab120.((t - 1) + (!k * (-10))) in
        let dest2 = plateau.(candidat2) in
        if dest2 = (-1) then begin
          b := false
        end
      end;
      if (!b && !droite) then begin
        let candidat3 = tab120.((t + 1) + (!k * (-10))) in
        let dest3 = plateau.(candidat3) in
        if dest3 = (-1) then begin
          b := false
        end
      end;
      if !b then begin
        incr k
      end;
    done
  end
  else begin
    if tab120.(t - 9) <> (-1) then begin
      gauche := true
    end;
    if tab120.(t - 11) <> (-1) then begin
      droite := true
    end;
    let k = ref 1 in
    while (tab120.(t + (!k * 10)) <> (-1) && !b) do
      compteur := !compteur + 1;
      let candidat1 = tab120.(t + (!k * 10)) in
      let dest1 = plateau.(candidat1) in
      if dest1 = 1 then begin
        b := false
      end;
      if (!b && !droite) then begin
        let candidat2 = tab120.((t - 1) + (!k * 10)) in
        let dest2 = plateau.(candidat2) in
        if dest2 = 1 then begin
          b := false
        end
      end;
      if (!b && !gauche) then begin
        let candidat3 = tab120.((t + 1) + (!k * 10)) in
        let dest3 = plateau.(candidat3) in
        if dest3 = 1 then begin
          b := false
        end
      end;
      if !b then begin
        incr k
      end;
    done
  end;
  if !b then !compteur else 0

(*Fonction détectant les positions nulles par manque de matériel, si les joueurs jouent correctement*)
let manque_de_materiel_approximatif plateau =
  let b = ref true in
  let compteur = ref 0 in
  let cavaliers_blancs = ref 0 in
  let cavaliers_noirs = ref 0 in
  let fous_blancs= ref 0 in
  let fous_noirs = ref 0 in
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
            |3 -> fous_blancs := !fous_blancs + 1
            |(-3) -> fous_noirs := !fous_noirs + 1
            |_ -> ()
        end
      end
    end;
    incr i
  done;
  if !b && !compteur = 2 && (!cavaliers_blancs <> 2 && !cavaliers_noirs <> 2) && ((!fous_blancs + !cavaliers_blancs) <> (!fous_noirs + !cavaliers_noirs)) then begin
    b := false
  end;
  !b

(*Valeur des pièces pour l'évaluation*)
let tabvalue = [|0; 10; 32; 33; 51; 88; 950|]

(*Fonction évaluant le positionnement d'une tour ou d'une dame*)
let evalue_tour plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees =
  let compteur = ref 0 in
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
          compteur := !compteur + 1;
          k := !k + 1
        end
        else if dest > 0 then begin
          compteur := !compteur + 2;
          if not trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end;
          s := false
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
          compteur := !compteur + 1;
          k := !k + 1
        end
        else if dest < 0 then begin
          compteur := !compteur + 2;
          if trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if not trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end;
          s := false
        end
      done
    done
  end;
  !compteur

(*Fonction évaluant le positionnement d'un fou ou d'une dame*)
let evalue_fou plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees =
  let compteur = ref 0 in
  let co = plateau.(case) in
  let t = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in 
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          compteur := !compteur + 1;
          k := !k + 1
        end
        else if dest > 0 then begin
          compteur := !compteur + 2;
          if not trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end;
          s := false
        end
      done
    done
  end
  else begin
    for i = 0 to 3 do
      let dir = vect_fou.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = 0 then begin
          compteur := !compteur + 1;
          k := !k + 1
        end
        else if dest < 0 then begin
          compteur := !compteur + 2;
          if trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if not trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end;
          s := false
        end
      done
    done
  end;
  !compteur

(*Fonction évaluant le positionnement d'un cavalier*)
let evalue_cavalier plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees = 
  let compteur = ref 0 in
  let co = plateau.(case) in
  let c = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_cavalier.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
          if dest = 0 then begin
            compteur := !compteur + 1
          end
          else if trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end
        end
        else begin
          compteur := !compteur + 2;
          if not trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end
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
          if dest = 0 then begin
            compteur := !compteur + 1
          end
          else if not trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat; prise = dest}) position_roi roi_en_echec piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end
        end
        else begin
          compteur := !compteur + 2;
          if trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end
        end
      end
    done
  end;
  !compteur

(*Fonction donnant le nombre de cases controllées par une dame*)
let evalue_dame plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees =
  let s1 = evalue_tour plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees in 
  let s2 = evalue_fou plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees in
  s1 + s2

(*Fonction évaluant le positionnement d'un roi*) 
let evalue_roi plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees = let _ = position_roi, roi_en_echec, piece_clouees in
  let compteur = ref 0 in
  let co = plateau.(case) in
  let c = tab64.(case) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest <= 0 then begin
          if dest = 0 then begin
            compteur := !compteur + 1
          end
          else begin
            if trait_aux_blancs && not (List.mem candidat !defendues) then begin
              attaquee := max !attaquee tabvalue.(- dest)
            end;
            compteur := !compteur + 1;
          end
        end
        else begin
          compteur := !compteur + 2;
          if not trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
        end
      end
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = vect_roi.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = plateau.(candidat) in
        if dest >= 0 then begin
          if dest = 0 then begin
            compteur := !compteur + 1
          end
          else begin
            if not (trait_aux_blancs || List.mem candidat !defendues) then begin
              attaquee := max !attaquee tabvalue.(dest)
            end;
            compteur := !compteur + 1;
          end
        end
        else begin
          compteur := !compteur + 2;
          if trait_aux_blancs then begin
            defendues := candidat :: !defendues;
          end;
        end
      end
    done
  end;
  !compteur

(*Fonction évaluant le positionnement d'un pion*)
let evalue_pion plateau case trait_aux_blancs defendues attaquee position_roi roi_en_echec piece_clouees =
  let compteur = ref 0 in
  let co = plateau.(case) in
  let p = tab64.(case) in
  if co > 0 then begin
    if ((case + 1) mod 8 <> 0) then begin
      let candidat1 = tab120.(p - 9) in
      let dest1 = plateau.(candidat1) in
      if dest1 <= 0 then begin
        if dest1 = 0 then begin
          compteur := !compteur + 1
        end
        else if trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat1; prise = dest1}) position_roi roi_en_echec piece_clouees) then begin
          if List.mem candidat1 !defendues then begin
            let difference = tabvalue.(co) - tabvalue.(- dest1) in
            if difference < 0 then begin
              attaquee := max !attaquee (- difference)
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(- dest1)
          end
        end
      end
      else begin
        compteur := !compteur + 2;
        if not trait_aux_blancs then begin
          defendues := candidat1 :: !defendues;
        end
      end
    end;
    if (case mod 8 <> 0) then begin
      let candidat2 = tab120.(p - 11) in
      let dest2 = plateau.(candidat2) in
      if dest2 <= 0 then begin
        if dest2 = 0 then begin
          compteur := !compteur + 1
        end
        else if trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat2; prise = dest2}) position_roi roi_en_echec piece_clouees) then begin
          if List.mem candidat2 !defendues then begin
            let difference = tabvalue.(co) - tabvalue.(- dest2) in
            if difference < 0 then begin
              attaquee := max !attaquee (- difference)
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(- dest2)
          end
        end
      end
      else begin
        compteur := !compteur + 2;
        if not trait_aux_blancs then begin
          defendues := candidat2 :: !defendues;
        end
      end
    end
  end
  else begin
    if (case mod 8 <> 0) then begin
      let candidat1 = tab120.(p + 9) in
      let dest1 = plateau.(candidat1) in
      if dest1 >= 0 then begin
        if dest1 = 0 then begin
          compteur := !compteur + 1
        end
        else if not trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat1; prise = dest1}) position_roi roi_en_echec piece_clouees) then begin
          if List.mem candidat1 !defendues then begin
            let difference = - tabvalue.(- co) + tabvalue.(dest1) in
            if difference > 0 then begin
              attaquee := max !attaquee difference
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(dest1)
          end
        end
      end
      else begin
        compteur := !compteur + 2;
        if trait_aux_blancs then begin
          defendues := candidat1 :: !defendues;
        end
      end
    end;
    if ((case + 1) mod 8 <> 0) then begin
      let candidat2 = tab120.(p + 11) in
      let dest2 = plateau.(candidat2) in
      if dest2 >= 0 then begin
        if dest2 = 0 then begin
          compteur := !compteur + 1
        end
        else if not trait_aux_blancs && (est_valide_efficace plateau (Classique {piece = co; depart = case; arrivee = candidat2; prise = dest2}) position_roi roi_en_echec piece_clouees) then begin
          if List.mem candidat2 !defendues then begin
            let difference = - tabvalue.(- co) + tabvalue.(dest2) in
            if difference > 0 then begin
              attaquee := max !attaquee difference
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(dest2)
          end
        end
      end
      else begin
        compteur := !compteur + 2;
        if trait_aux_blancs then begin
          defendues := candidat2 :: !defendues;
        end;
      end
    end
  end;
  !compteur

let tabfun2 = [|evalue_pion; evalue_cavalier; evalue_fou; evalue_tour; evalue_dame; evalue_roi|]

(*Fonction indique la différence du nombre de structure de pions doublées entre les deux joueurs*)
let doublees plateau =
  let difference_doubles_pions = ref 0 in
  for i = 8 to 55 do
    if plateau.(i) = (-1) then begin
      if plateau.(i + 8) = (-1) then begin
        difference_doubles_pions := !difference_doubles_pions + 1
      end
    end
    else if plateau.(i) = 1 then begin
      if plateau.(i - 8) = 1 then begin
        difference_doubles_pions := !difference_doubles_pions - 1
      end
    end
  done;
  !difference_doubles_pions

let placement_ouverture plateau materiel position =
  if plateau.(59) = 5 then begin
    position := !position + 3
  end;
  if plateau.(3) = (-5) then begin
    position := !position - 3
  end;
  if plateau.(57) = 2 then begin
    position := !position - 3
  end;
  if plateau.(62) = 2 then begin
    position := !position - 3
  end;
  if plateau.(1) = (-2) then begin
    position := !position + 3
  end;
  if plateau.(6) = (-2) then begin
    position := !position + 3
  end;
  if plateau.(58) = 3 then begin
    position := !position - 3
  end;
  if plateau.(61) = 3 then begin
    position := !position - 3
  end;
  if plateau.(2) = (-3) then begin
    position := !position + 3
  end;
  if plateau.(5) = (-3) then begin
    position := !position + 3
  end;
  if plateau.(27) = 1 then begin
    position := !position + 8
  end
  else if plateau.(27) = (-1) then begin
    position := !position - 8
  end;
  if plateau.(28) = 1 then begin
    position := !position + 8
  end
  else if plateau.(28) = (-1) then begin
    position := !position - 8
  end;
  if plateau.(35) = 1 then begin
    position := !position + 8
  end
  else if plateau.(35) = (-1) then begin
    position := !position - 8
  end;
  if plateau.(36) = 1 then begin
    position := !position + 8
  end
  else if plateau.(36) = (-1) then begin
    position := !position - 8
  end;
  if (plateau.(62) = 6 && plateau.(63) <> 4) then begin
    position := !position + 15;
    if (plateau.(53) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(54) = 1) then begin
      position := !position + 10
    end
    else if (plateau.(46) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(55) = 1) then begin
      position := !position + 2
    end
  end
  else if (plateau.(58) = 6 && plateau.(56) <> 4) then begin
    position := !position + 15;
    if (plateau.(48) = 1) then begin
      position := !position + 2
    end; 
    if (plateau.(49) = 1) then begin
      position := !position + 10
    end
    else if (plateau.(41) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(50) = 1) then begin 
      position := !position + 5
    end
  end
  else if plateau.(60) <> 6 then begin
    materiel := !materiel - 4;
    if plateau.(59) = 6 || plateau.(61) = 6 then begin
      materiel := !materiel + 1
    end
  end;
  if (plateau.(6) = (-6) && plateau.(7) <> (-4))then begin
    position := !position - 15;
    if (plateau.(13) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(14) = (-1)) then begin
      position := !position - 10
    end
    else if (plateau.(22) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(15) = (-1)) then begin
      position := !position - 2
    end
  end
  else if (plateau.(2) = (-6) && plateau.(0) <> (-4)) then begin
    position := !position - 15;
    if (plateau.(8) = (-1)) then begin
      position := !position - 2
    end;
    if (plateau.(9) = (-1)) then begin
      position := !position - 10
    end
    else if (plateau.(17) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(10) = (-1)) then begin
      position := !position - 5
    end
  end
  else if plateau.(4) <> (-6) then begin
    materiel := !materiel + 4;
    if plateau.(3) = (-6) || plateau.(5) = (-6) then begin
      materiel := !materiel - 1
    end
  end

let placement_mdj plateau position =
  if (plateau.(62) = 6 && plateau.(63) <> 4) then begin
    position := !position + 15;
    if (plateau.(53) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(54) = 1) then begin
      position := !position + 10
    end
    else if (plateau.(46) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(55) = 1) then begin
      position := !position + 2
    end
  end
  else if ((plateau.(58) = 6 || plateau.(57) = 6) && plateau.(56) <> 4) then begin
    position := !position + 15;
    if (plateau.(48) = 1) then begin
      position := !position + 2
    end; 
    if (plateau.(49) = 1) then begin
      position := !position + 10
    end
    else if (plateau.(41) = 1) then begin
      position := !position + 5
    end;
    if (plateau.(50) = 1) then begin 
      position := !position + 5
    end
  end;
  if (plateau.(6) = (-6) && plateau.(7) <> (-4))then begin
    position := !position - 15;
    if (plateau.(13) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(14) = (-1)) then begin
      position := !position - 10
    end
    else if (plateau.(22) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(15) = (-1)) then begin
      position := !position - 2
    end
  end
  else if ((plateau.(2) = (-6) || plateau.(1) = (-6)) && plateau.(0) <> (-4)) then begin
    position := !position - 15;
    if (plateau.(8) = (-1)) then begin
      position := !position - 2
    end;
    if (plateau.(9) = (-1)) then begin
      position := !position - 10
    end
    else if (plateau.(17) = (-1)) then begin
      position := !position - 5
    end;
    if (plateau.(10) = (-1)) then begin
      position := !position - 5
    end
  end

(**)
let eval_materiel plateau =
  let materiel = ref 0 in
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case > 0 then begin
      materiel := !materiel + tabvalue.(case)
    end
    else if case < 0 then begin
      materiel := !materiel - tabvalue.(- case)
    end
  done;
  !materiel

let eval_noirs_sl plateau trait_aux_blancs piece_clouees defendues pieces_joueur attaque_noirs position_roi roi_en_echec materiel position =
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case < 0 then begin
      let eval_piece = (tabfun2.(- case - 1)) plateau i trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees in
      materiel := !materiel - tabvalue.(- case);
      position := !position - eval_piece
    end
    else if case > 0 then begin
      pieces_joueur := i :: !pieces_joueur
    end
  done
  
let rec eval_blancs liste_cases plateau trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees materiel position =
  match liste_cases with
    |[] -> ()
    |h::t -> 
      let case = plateau.(h) in
      let eval_piece = (tabfun2.(case - 1)) plateau h trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees in
      materiel := !materiel + tabvalue.(case);
      position := !position + eval_piece;
      eval_blancs t plateau trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees materiel position

let eval_blancs_sl plateau trait_aux_blancs piece_clouees defendues pieces_joueur attaque_blancs position_roi roi_en_echec materiel position =
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case > 0 then begin
      let eval_piece = (tabfun2.(case - 1)) plateau i trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees in
      materiel := !materiel + tabvalue.(case);
      position := !position + eval_piece;
    end
    else if case < 0 then begin
      pieces_joueur := i :: !pieces_joueur
    end
  done

let rec eval_noirs liste_cases plateau trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees materiel position =
  match liste_cases with
    |[] -> ()
    |h::t -> let case = plateau.(h) in
      let eval_piece = (tabfun2.(- case - 1)) plateau h trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees in
      materiel := !materiel - tabvalue.(- case);
      position := !position - eval_piece;
      eval_noirs t plateau trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees materiel position

let evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position =
  let piece_clouees = clouees plateau position_roi in
  let defendues = ref [] in
  let pieces_joueur = ref [] in
  let attaque_blancs = ref 0 in
  let attaque_noirs = ref 0 in
  if trait_aux_blancs then begin
    eval_noirs_sl plateau trait_aux_blancs piece_clouees defendues pieces_joueur attaque_noirs position_roi roi_en_echec materiel position;
    eval_blancs !pieces_joueur plateau trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees materiel position
  end
  else begin
    eval_blancs_sl plateau trait_aux_blancs piece_clouees defendues pieces_joueur attaque_blancs position_roi roi_en_echec materiel position;
    eval_noirs !pieces_joueur plateau trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees materiel position
  end;
  if trait_aux_blancs then begin
    materiel := !materiel + !attaque_blancs
  end
  else begin
    materiel := !materiel - !attaque_noirs
  end

let evaluation_double_finale plateau trait_aux_blancs position_roi roi_en_echec materiel position =
  let piece_clouees = clouees plateau position_roi in
  let defendues = ref [] in
  let pieces_joueur = ref [] in
  let attaque_blancs = ref 0 in
  let attaque_noirs = ref 0 in
  if trait_aux_blancs then begin
    for i = 0 to 63 do
      let case = plateau.(i) in
      if case < 0 then begin
        if case = (-1) then begin
          materiel := !materiel - (2 * (7 - pion_passe plateau i));
        end;
        let eval_piece = (tabfun2.(- case - 1)) plateau i trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees in
        materiel := !materiel - tabvalue.(- case);
        position:= !position - eval_piece
      end
      else if case > 0 then begin
        if case = 1 then begin
          materiel := !materiel + (2 * (7 - pion_passe plateau i))
        end;
        pieces_joueur := i :: !pieces_joueur
      end
    done;
    let rec eval_blancs liste_cases = match liste_cases with
      |[] -> ()
      |h::t -> let case = plateau.(h) in
                let eval_piece = (tabfun2.(case - 1))plateau h trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees in
                materiel := !materiel + tabvalue.(case);
                position:= !position + eval_piece;
                eval_blancs t
    in eval_blancs !pieces_joueur
  end
  else begin
    for i = 0 to 63 do
      let case = plateau.(i) in
      if case > 0 then begin
        if case = 1 then begin
          materiel := !materiel + (2 * (7 - pion_passe plateau i))
        end;
        let eval_piece = (tabfun2.(case - 1)) plateau i trait_aux_blancs defendues attaque_blancs position_roi roi_en_echec piece_clouees in
        materiel := !materiel + tabvalue.(case);
        position:= !position + eval_piece;
      end
      else if case < 0 then begin
        if case = (-1) then begin
          materiel := !materiel - (2 * (7 - pion_passe plateau i))
        end;
        pieces_joueur := i :: !pieces_joueur
      end
    done;
    let rec eval_noirs liste_cases = match liste_cases with
      |[] -> ()
      |h::t -> let case = plateau.(h) in
                let eval_piece = (tabfun2.(- case - 1)) plateau h trait_aux_blancs defendues attaque_noirs position_roi roi_en_echec piece_clouees in
                materiel := !materiel - tabvalue.(- case);
                position:= !position - eval_piece;
                eval_noirs t
    in eval_noirs !pieces_joueur
  end;
  if trait_aux_blancs then begin
    materiel := !materiel + !attaque_blancs
  end
  else begin
    materiel := !materiel - !attaque_noirs
  end

let traitement trait_aux_blancs materiel position =
  if trait_aux_blancs then begin
    100 * materiel + position
  end
  else begin
    -(100 * materiel + position)
  end

(*Fonction évaluant la position d'un joueur en utilisée en ouverture si aucun coup théorique n'existe*)
let evalue_ouverture plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let materiel = ref (2 * (doublees plateau)) in
  let position = ref 0 in
  placement_ouverture plateau materiel position;
  evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position;
  traitement trait_aux_blancs !materiel !position

(*Fonction évaluant la position d'un joueur utilisée en milieu de jeu*)
let evalue_mdj plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let materiel = ref (2 * (doublees plateau)) in
  let position = ref 0 in
  placement_mdj plateau position;
  evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position;
  traitement trait_aux_blancs !materiel !position

(*Fonction évaluant la position d'un joueur utilisée en finale*)
let evalue_finale plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  if manque_de_materiel_approximatif plateau then begin
    0
  end
  else begin
    let materiel = ref (2 * (doublees plateau)) in
    let position = ref 0 in
    evaluation_double_finale plateau trait_aux_blancs position_roi roi_en_echec materiel position;
    traitement trait_aux_blancs !materiel !position
  end

(*Fonction d'évaluation à n'appliquer que sur les positions stables*)
let evalue_simple plateau trait_aux_blancs (position_roi : int) (roi_en_echec : bool) alpha beta =
  let _ = trait_aux_blancs, position_roi, roi_en_echec in
  let position = ref 0 in
  let note_provisoire = traitement trait_aux_blancs (eval_materiel plateau) 0 in
  if note_provisoire - 75 < beta && note_provisoire + 75 > alpha then begin
    position := mobilite plateau trait_aux_blancs;
    if note_provisoire + !position - 75 < beta && note_provisoire + !position > alpha then begin
      position := !position - mobilite plateau (not trait_aux_blancs)
    end
  end;
  note_provisoire + !position

let tab_pion_blanc = 
  [| 0;  0;  0;  0;  0;  0;  0;  0;
    50; 50; 50; 50; 50; 50; 50; 50;
    10; 10; 20; 30; 30; 20; 10; 10;
     5;  5; 10; 25; 25; 10;  5;  5;
     0;  0;  0; 20; 20;  0;  0;  0;
     5; -5;-10;  0;  0;-10; -5;  5;
     5; 10; 10;-20;-20; 10; 10;  5;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_pion_noir =
  [| 0;  0;  0;  0;  0;  0;  0;  0;
     5; 10; 10;-20;-20; 10; 10;  5;
     5; -5;-10;  0;  0;-10; -5;  5;
     0;  0;  0; 20; 20;  0;  0;  0;
     5;  5; 10; 25; 25; 10;  5;  5;
    10; 10; 20; 30; 30; 20; 10; 10;
    50; 50; 50; 50; 50; 50; 50; 50;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_chevalier_blanc = 
  [|-50;-40;-30;-30;-30;-30;-40;-50;
    -40;-20;  0;  0;  0;  0;-20;-40;
    -30;  0; 10; 15; 15; 10;  0;-30;
    -30;  5; 15; 20; 20; 15;  5;-30;
    -30;  0; 15; 20; 20; 15;  0;-30;
    -30;  5; 10; 15; 15; 10;  5;-30;
    -40;-20;  0;  5;  5;  0;-20;-40;
    -50;-40;-30;-30;-30;-30;-40;-50;
  |]

let tab_chevalier_noir =
  [|-50;-40;-30;-30;-30;-30;-40;-50;
    -40;-20;  0;  5;  5;  0;-20;-40;
    -30;  5; 10; 15; 15; 10;  5;-30;
    -30;  0; 15; 20; 20; 15;  0;-30;
    -30;  5; 15; 20; 20; 15;  5;-30;
    -30;  0; 10; 15; 15; 10;  0;-30;
    -40;-20;  0;  0;  0;  0;-20;-40;
    -50;-40;-30;-30;-30;-30;-40;-50;
  |]

let tab_fou_blanc =
  [|-20;-10;-10;-10;-10;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5; 10; 10;  5;  0;-10;
    -10;  5;  5; 10; 10;  5;  5;-10;
    -10;  0; 10; 10; 10; 10;  0;-10;
    -10; 10; 10; 10; 10; 10; 10;-10;
    -10;  5;  0;  0;  0;  0;  5;-10;
    -20;-10;-10;-10;-10;-10;-10;-20;
  |]

let tab_fou_noir =
  [|-20;-10;-10;-10;-10;-10;-10;-20;
    -10;  5;  0;  0;  0;  0;  5;-10;
    -10; 10; 10; 10; 10; 10; 10;-10;
    -10;  0; 10; 10; 10; 10;  0;-10;
    -10;  5;  5; 10; 10;  5;  5;-10;
    -10;  0;  5; 10; 10;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10;-10;-10;-10;-10;-20
  |]

let tab_tour_blanche_ouverture =
  [| 10; 0;  0; 10; 10;  0;  0; 10;
     5; 10; 10; 10; 10; 10; 10;  5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     0;  0;  0;  5;  5;  0;  0;  0;
  |]

let tab_tour_noire_ouverture =
  [| 0;  0;  0;  5;  5;  0;  0;  0;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     5; 10; 10; 10; 10; 10; 10;  5;
     10; 0; 0;  10; 10;  0;  0;  10
  |]

let tab_tour_blanche_mdg =
  [| 0;  0;  0;  0;  0;  0;  0;  0;
     5; 10; 10; 10; 10; 10; 10;  5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     0;  0;  0;  5;  5;  0;  0;  0;
  |]

let tab_tour_noire_mdg =
  [| 0;  0;  0;  5;  5;  0;  0;  0;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     5; 10; 10; 10; 10; 10; 10;  5;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_dame_blanche_ouverture =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5;  5;  5;  5;  0;-10;
     -5;  0;  5;  5;  5;  5;  0; -5;
      0;  0;  5;  5;  5;  5;  0; -5;
    -10;  5;  5;  5;  5;  5;  0;-10;
    -10;  0;  5;  0;  0;  0;  0;-10;
    -20;-10;-10; 20; -5;-10;-10;-20;
  |]

let tab_dame_noire_ouverture =
  [|-20;-10;-10; -5; 20;-10;-10;-20;
    -10;  0;  0;  0;  0;  5;  0;-10;
    -10;  0;  5;  5;  5;  5;  5;-10;
     -5;  0;  5;  5;  5;  5;  0;  0;
     -5;  0;  5;  5;  5;  5;  0; -5;
    -10;  0;  5;  5;  5;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20
  |]

let tab_dame_blanche_mdg =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5;  5;  5;  5;  0;-10;
     -5;  0;  5;  5;  5;  5;  0; -5;
      0;  0;  5;  5;  5;  5;  0; -5;
    -10;  5;  5;  5;  5;  5;  0;-10;
    -10;  0;  5;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20;
  |]

let tab_dame_noire_mdg =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  5;  0;-10;
    -10;  0;  5;  5;  5;  5;  5;-10;
     -5;  0;  5;  5;  5;  5;  0;  0;
     -5;  0;  5;  5;  5;  5;  0; -5;
    -10;  0;  5;  5;  5;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20
  |]

let tab_roi_blanc_mdg =
  [|-30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -20;-30;-30;-40;-40;-30;-30;-20;
    -10;-20;-20;-20;-20;-20;-20;-10;
     20; 20;  0;  0;  0;  0; 20; 20;
     20; 30; 10;  0;  0; 10; 30; 20
  |]

let tab_roi_noir_mdg =
  [| 20; 30; 10;  0;  0; 10; 30; 20;
     20; 20;  0;  0;  0;  0; 20; 20;
    -10;-20;-20;-20;-20;-20;-20;-10;
    -20;-30;-30;-40;-40;-30;-30;-20;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30
  |]

let tab_roi_blanc_finale =
  [|-50;-40;-30;-20;-20;-30;-40;-50;
    -30;-20;-10;  0;  0;-10;-20;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-30;  0;  0;  0;  0;-30;-30;
    -50;-30;-30;-30;-30;-30;-30;-50
  |]

let tab_roi_noir_finale =
  [|-50;-30;-30;-30;-30;-30;-30;-50;
    -30;-30;  0;  0;  0;  0;-30;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-20;-10;  0;  0;-10;-20;-30;
    -50;-40;-30;-20;-20;-30;-40;-50|]

let tab_pieces_blanches_ouverture = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_mdg|]

let tab_pieces_blanches_mdg = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_mdg|]

let tab_pieces_blanches_finale = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_finale|]

let tab_pieces_noires_ouverture = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_mdg|]

let tab_pieces_noires_mdg = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_mdg|]

let tab_pieces_noires_finale = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_finale|]

let tab_ouverture = tab_pieces_blanches_ouverture, tab_pieces_noires_ouverture

let tab_mdg = tab_pieces_blanches_mdg, tab_pieces_noires_mdg

let tab_finale = tab_pieces_blanches_finale, tab_pieces_noires_finale

let fp plateau position table =
  let note = ref 0 in
  let tb, tn = table in
  for i = 0 to 63 do 
    let case = plateau.(i) in
    if case > 0 then begin
      note := !note + tb.(case - 1).(i)
    end
    else if case < 0 then begin
      note := !note - tn.(abs case - 1).(i)
    end
  done;
  position := !position + (!note / 5)

let eval1 plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let materiel = ref (2 * (doublees plateau)) in
  let position = ref 0 in
  fp plateau position tab_ouverture;
  evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position;
  traitement trait_aux_blancs !materiel !position

let eval2 plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let materiel = ref (2 * (doublees plateau)) in
  let position = ref 0 in
  fp plateau position tab_mdg;
  evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position;
  traitement trait_aux_blancs !materiel !position

let eval3 plateau trait_aux_blancs position_roi roi_en_echec (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let materiel = ref (2 * (doublees plateau)) in
  let position = ref 0 in
  fp plateau position tab_finale;
  evaluation_double plateau trait_aux_blancs position_roi roi_en_echec materiel position;
  traitement trait_aux_blancs !materiel !position