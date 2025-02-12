(*Module implémentant les fonctions qui permettent de générer les coups possibles à partir d'une position*)

open Plateau

(*Fonction indiquant si une case occupée par une pièce amie est attaquée par une pièce ennemie*)
let menacee plateau case trait_aux_blancs =
  let b = ref false in
  let m = tab64.(case) in
  if trait_aux_blancs then begin
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
  else begin
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
  for i = 0 to 3 do
    let dir = vect_tour.(i) in
    let k = ref 1 in
    let s = ref true in
    while (!s && tab120.(t + (!k * dir)) <> (-1)) do
      let candidat = tab120.(t + (!k * dir)) in
      let dest = plateau.(candidat) in
      if dest = 0 then begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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
let deplacements_fou plateau case liste =
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
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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
let deplacements_cavalier plateau case liste =
  let co = plateau.(case) in
  let c = tab64.(case) in
  for i = 0 to 7 do
    let dir = vect_cavalier.(i) in
    if tab120.(c + dir) <> (-1) then begin
      let candidat = tab120.(c + dir) in
      let dest = plateau.(candidat) in
      if co * dest <= 0 then begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste
      end
    end
  done

(*Fonction construisant une liste des déplacements possible d'une dame*)
let deplacements_dame plateau case liste =
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
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = 0} :: !liste;
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
let deplacements_roi plateau case liste =
  let co = plateau.(case) in
  let r = tab64.(case) in
  for i = 0 to 7 do
    let dir = vect_roi.(i) in
    if tab120.(r + dir) <> (-1) then begin
      let candidat = tab120.(r + dir) in
      let dest = plateau.(candidat) in
      if co * dest <= 0 then begin
        liste := Classique {piece = co; depart = case; arrivee = candidat; prise = dest} :: !liste
      end
    end
  done

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

(*Variables indiquant la positions initiales des pièces impliquées dans un roque*)
let depart_roi_blanc = ref 60
let depart_roi_noir = ref 4
let depart_tour_blanche_pr = ref 63
let depart_tour_blanche_gr = ref 56
let depart_tour_noire_pr = ref 7
let depart_tour_noire_gr = ref 0

(*Cases de passage du roi pour le roque*)
let chemin_blanc_pr = [|61; 62; 0; 0; 0; 0|]
let chemin_blanc_gr = [|59; 58; 0; 0; 0|]
let chemin_noir_pr = [|5; 6; 0; 0; 0; 0|]
let chemin_noir_gr = [|3; 2; 0; 0; 0|]

(*Nombre de case traversée par le roi*)
let longueur_chemin_roi_blanc_pr = ref 2
let longueur_chemin_roi_blanc_gr = ref 2
let longueur_chemin_roi_noir_pr = ref 2
let longueur_chemin_roi_noir_gr = ref 2

(*Cases devant êtres vides pour le roque*)
let vides_blanc_pr = ref [61; 62]
let vides_blanc_gr = ref [59; 58; 57]
let vides_noir_pr = ref [5; 6]
let vides_noir_gr = ref [3; 2; 1]

(*Variable indiquant si le roi doit se déplacer pour atteindre sa case de roque (échecs 960)*)
let roi_blanc_clouable = ref true
let roi_noir_clouable = ref true

(*Variables indiquants si la colonne de départ de la tour du grand roque est a*)
let tour_blanche_gr_en_a = ref true
let tour_noire_gr_en_a = ref true

(*Variables indiquant si la colonne de départ de la tour du petit roque est h*)
let tour_blanche_pr_en_h = ref true
let tour_noire_pr_en_h = ref true

(*Case critique pour le droit au roque*)
let clouage_roi_blanc_1 = ref 52
let clouage_roi_blanc_2 = ref 44
let clouage_roi_noir_1 = ref 12
let clouage_roi_noir_2 = ref 20

(*Variable indiquant la direction du grand roque, valant 1 si le déplacement du roi se fait vers la droite, (-1) sinon*)
let direction_gr_blanc = ref (-1)
let direction_gr_noir = ref (-1)

(*Variable indiquant si la case de départ de la tour du grand roque est b1 (respectivement b8)*)
let tour_blanche_gr_en_b = ref false
let tour_noire_gr_en_b = ref false

(*Vecteurs de déplacement des potentielles menaces au roque*)
let vect_fou_roque_blanc_pr = [|(-9); (-11)|]
let vect_fou_roque_blanc_gr = [|(-11); (-9)|]
let vect_fou_roque_noir_pr = [|11; 9|]
let vect_fou_roque_noir_gr = [|9; 11|]
let vect_cavalier_roque_blanc = [|(-8); (-12); (-19); (-21)|]
let vect_cavalier_roque_noir = [|8; 12; 19; 21|]

(*Fonction actualisant les variables relatives au roque en fonction de la position de départ*)
let actualisation_roque position_de_depart =
  List.iter (fun tab -> Array.fill tab 0 (Array.length tab) 0) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr];
  List.iter (fun mut -> mut := 0) [longueur_chemin_roi_blanc_pr; longueur_chemin_roi_blanc_gr; longueur_chemin_roi_noir_pr; longueur_chemin_roi_noir_gr];
  List.iter (fun mut -> mut := []) [vides_blanc_pr; vides_blanc_gr; vides_noir_pr; vides_noir_gr];
  List.iter (fun (v, b) -> v := b) [(roi_blanc_clouable, true); (roi_noir_clouable, true); (tour_blanche_gr_en_a, true); (tour_noire_gr_en_a, true); (tour_blanche_gr_en_b, false); (tour_noire_gr_en_b, false); (tour_blanche_pr_en_h, true); (tour_blanche_pr_en_h, true)];
  List.iter (fun direction_gr -> direction_gr := (-1)) [direction_gr_blanc; direction_gr_noir];
  List.iter (fun (vect_fou_roque_joueur_gr, vect_fou_roque_adversaire_pr) ->
    for i = 0 to 1 do
      vect_fou_roque_joueur_gr.(i) <- (- vect_fou_roque_adversaire_pr.(i))
    done)
  [(vect_fou_roque_blanc_gr, vect_fou_roque_noir_pr); (vect_fou_roque_noir_gr, vect_fou_roque_blanc_pr)];
  let aux signe_joueur depart_roi depart_tour_pr depart_tour_gr clouage_roi_1 clouage_roi_2 roi_clouable tour_gr_en_a tour_gr_en_b tour_pr_en_h direction_gr vect_fou_roque_pr vect_fou_roque_gr =
    let tour_grand_roque = ref true in
    let increment = if signe_joueur > 0 then 56 else 0 in
    for case = 0 + increment to 7 + increment do
      let piece = signe_joueur * position_de_depart.(case) in
      if piece = 4 then begin
        if !tour_grand_roque then begin
          depart_tour_gr := case;
          tour_gr_en_a := case = 0 + increment;
          tour_gr_en_b := case = 1 + increment;
          tour_grand_roque := false
        end
        else begin
          depart_tour_pr := case;
          tour_pr_en_h := case = 7 + increment
        end
      end
      else if piece = 6 then begin
        depart_roi := case;
        clouage_roi_1 := case - (signe_joueur * 8);
        clouage_roi_2 := case - (signe_joueur * 16);
        if List.mem case [2 + increment; 6 + increment] then begin
          roi_clouable := false
        end
        else if case = 1 + increment then begin
          List.iter (fun i -> vect_fou_roque_gr.(i) <- vect_fou_roque_pr.(i)) [0; 1];
          direction_gr := 1
        end
      end
    done
  in List.iter
  (fun (signe_joueur, depart_roi, depart_tour_pr, depart_tour_gr, clouage_roi_1, clouage_roi_2, roi_clouable, tour_gr_en_a, tour_gr_en_b, tour_pr_en_h, direction_gr, vect_fou_roque_pr, vect_fou_roque_gr) ->
  aux signe_joueur depart_roi depart_tour_pr depart_tour_gr clouage_roi_1 clouage_roi_2 roi_clouable  tour_gr_en_a tour_gr_en_b tour_pr_en_h direction_gr vect_fou_roque_pr vect_fou_roque_gr)
  [((-1), depart_roi_noir, depart_tour_noire_pr, depart_tour_noire_gr, clouage_roi_noir_1, clouage_roi_noir_2, roi_noir_clouable, tour_noire_gr_en_a, tour_noire_gr_en_b, tour_noire_pr_en_h, direction_gr_noir, vect_fou_roque_noir_pr, vect_fou_roque_noir_gr);
  (1, depart_roi_blanc, depart_tour_blanche_pr, depart_tour_blanche_gr, clouage_roi_blanc_1, clouage_roi_blanc_2, roi_blanc_clouable, tour_blanche_gr_en_a, tour_blanche_gr_en_b, tour_blanche_pr_en_h, direction_gr_blanc, vect_fou_roque_blanc_pr, vect_fou_roque_blanc_gr)];
  let j = ref 0 in
  let aux chemin longueur_chemin_roi vides j i =
    chemin.(!j) <- i;
    incr longueur_chemin_roi;
    vides := i :: !vides;
    incr j
  in
  for i = !depart_roi_blanc + 1 to 62 do
    aux chemin_blanc_pr longueur_chemin_roi_blanc_pr vides_blanc_pr j i
  done;
  j := 0;
  if !depart_roi_blanc = 57 then begin
    aux chemin_blanc_gr longueur_chemin_roi_blanc_gr vides_blanc_gr j 58
  end
  else begin
    for i = !depart_roi_blanc - 1 downto 58 do
      aux chemin_blanc_gr longueur_chemin_roi_blanc_gr vides_blanc_gr j i
    done
  end;
  j := 0;
  for i = !depart_roi_noir + 1 to 6 do
    aux chemin_noir_pr longueur_chemin_roi_noir_pr vides_noir_pr j i
  done;
  j := 0;
  if !depart_roi_noir = 1 then begin
    aux chemin_noir_gr longueur_chemin_roi_noir_gr vides_noir_gr j 2
  end
  else begin 
    for i = !depart_roi_noir - 1 downto 2 do
      aux chemin_noir_gr longueur_chemin_roi_noir_gr vides_noir_gr j i
    done
  end;
  j := 0;
  for i = !depart_tour_blanche_gr + 1 to 59 do
    vides_blanc_gr := i :: !vides_blanc_gr
  done;
  for i = !depart_tour_blanche_pr - 1 downto 61 do
    vides_blanc_pr := i :: !vides_blanc_pr
  done;
  j := 0;
  for i = !depart_tour_noire_gr + 1 to 3 do
    vides_noir_gr := i :: !vides_noir_gr
  done;
  for i = !depart_tour_noire_pr - 1 downto 5 do
    vides_noir_pr := i :: !vides_noir_pr
  done;
  let rec supprime_doublon_triee liste = match liste with
    |[] -> []
    |[h] -> [h]
    |h :: g :: t -> if h = g then h :: supprime_doublon_triee t else h :: (supprime_doublon_triee (g :: t))
  in let rec aux_liste liste depart_tour depart_roi = match liste with
    |[] -> []
    |h::t when not (List.mem h [depart_tour; depart_roi]) -> h :: aux_liste t depart_tour depart_roi
    |_::t -> aux_liste t depart_tour depart_roi
  in vides_blanc_pr := List.rev (supprime_doublon_triee (tri_fusion (aux_liste !vides_blanc_pr !depart_tour_blanche_pr !depart_roi_blanc)));
  vides_noir_pr := List.rev (supprime_doublon_triee (tri_fusion (aux_liste !vides_noir_pr !depart_tour_noire_pr !depart_roi_noir)));
  vides_blanc_gr := supprime_doublon_triee (tri_fusion (aux_liste !vides_blanc_gr !depart_tour_blanche_gr !depart_roi_blanc));
  vides_noir_gr := supprime_doublon_triee (tri_fusion (aux_liste !vides_noir_gr !depart_tour_noire_gr !depart_roi_noir))

(*Fonction permettant de vérifier la validité des roques*)
let menaces_roques plateau signe_joueur clouage_roi pseudo_e2 chemin_roi longueur_chemin_roi vect_fou vect_cavalier signe_roque =
  let i = ref 0 in
  let b = ref false in
  let diagonale = ref pseudo_e2 in
  let dir_fou = ref 2 in
    while !i < longueur_chemin_roi do
      let m = tab64.(chemin_roi.(!i)) in
      if !diagonale > 0 then begin
        dir_fou := 1
      end;
      let j = ref 0 in
      while (not !b && !j < !dir_fou) do
        let dir = vect_fou.(!j) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(m + (!k * dir)) in
          let dest = signe_joueur * plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            s :=  false
          end
          else begin
            if dest = (-3) || dest = (-5) || ((dest = (-6) || dest = (-1)) && !k = 1)  then begin
              b := true
            end;
            s :=  false
          end
        done;
        incr j
      done;
      if not !b then begin
        let k = ref 0 in
        while (not !b && !k < 4) do
          let dir = vect_cavalier.(!k) in
          if tab120.(m + dir) <> (-1) then begin
            let candidat = tab120.(m + dir) in
            if signe_joueur * plateau.(candidat) = (-2) then begin
              b := true
            end
          end;
          incr k
        done
      end;
      if not !b then begin
        if !i < longueur_chemin_roi then begin
          diagonale := signe_joueur * plateau.(clouage_roi + signe_roque * (1 + !i))
        end;
        if List.mem !diagonale [(-4); (-5); (-6)] then begin
          b := true
        end
        else if !diagonale = 0 then begin
          let dir = signe_joueur * (-10) in
          let k = ref 2 in
          let s = ref true in
          while (tab120.(m + (!k * dir)) <> (-1) && !s) do
            let candidat = tab120.(m + (!k * dir)) in
            let dest = signe_joueur * plateau.(candidat) in
            if dest = 0 then begin
              incr k
            end
            else if dest > 0 then begin
              s :=  false
            end
            else begin
              if dest = (-4) || dest = (-5) then begin
                b := true
              end;
              s :=  false
            end
          done
        end
      end;
      incr i;
      dir_fou := 2
    done;
  !b

(*Fonction indiquant l'absence de menace en a ou b empêchant un grand roque*)
let gr_possible plateau trait_aux_blancs =
  if trait_aux_blancs then begin
    let b1 = plateau.(57) in
    not ((b1 < (-3)) || ((b1 = 0 || !tour_blanche_gr_en_b) && List.mem plateau.(56) [(-4); (-5)]))
  end
  else begin
    let b8 = plateau.(1) in
    not (b8 > 3 || ((b8 = 0 || !tour_noire_gr_en_b) && List.mem plateau.(0) [4; 5]))
  end

(*Fonction construisant une liste des roques possible d'un joueur*)
let roque plateau trait_aux_blancs (prb, grb, prn, grn) =
  let l = ref [] in 
  if trait_aux_blancs then begin
    let pseudo_e2 = plateau.(!clouage_roi_blanc_1) in
    if not (!roi_blanc_clouable && (pseudo_e2 = (-1) || plateau.(!clouage_roi_blanc_2) = (-2))) then begin
      if prb && (!tour_blanche_pr_en_h || plateau.(63) > (-4)) && List.for_all (fun case -> plateau.(case) = 0) !vides_blanc_pr && not (menaces_roques plateau 1 !clouage_roi_blanc_1 pseudo_e2 chemin_blanc_pr !longueur_chemin_roi_blanc_pr vect_fou_roque_blanc_pr vect_cavalier_roque_blanc 1)
          then l := Roque {sorte = 1} :: !l
      end;
      if grb && (!tour_blanche_gr_en_a || gr_possible plateau true) && List.for_all (fun case -> plateau.(case) = 0) !vides_blanc_gr && not (menaces_roques plateau 1 !clouage_roi_blanc_1 pseudo_e2 chemin_blanc_gr !longueur_chemin_roi_blanc_gr vect_fou_roque_blanc_gr vect_cavalier_roque_blanc !direction_gr_blanc)
        then l := Roque {sorte = 2} :: !l
  end
  else begin
    let pseudo_e7 = - plateau.(!clouage_roi_noir_1) in
    if not (!roi_noir_clouable && (pseudo_e7 = (-1) || plateau.(!clouage_roi_noir_2) = 2)) then begin
      if prn && (!tour_noire_pr_en_h || plateau.(7) < 4) && List.for_all (fun case -> plateau.(case) = 0) !vides_noir_pr && not (menaces_roques plateau (-1) !clouage_roi_noir_1 pseudo_e7 chemin_noir_pr !longueur_chemin_roi_noir_pr vect_fou_roque_noir_pr vect_cavalier_roque_noir 1)
          then l := Roque {sorte = 3} :: !l
      end;
      if grn && (!tour_noire_gr_en_a || gr_possible plateau false) && List.for_all (fun case -> plateau.(case) = 0) !vides_noir_gr && not (menaces_roques plateau (-1) !clouage_roi_noir_1 pseudo_e7 chemin_noir_gr !longueur_chemin_roi_noir_gr vect_fou_roque_noir_gr vect_cavalier_roque_noir !direction_gr_noir)
        then l := Roque {sorte = 4} :: !l
    end;
  !l

(*Fonction adaptant les droits aux roques en fonction du coup*)
let modification_roque coup (prb, grb, prn, grn) =
  if (prb || grb || prn || grn) then  begin match coup with
    |Classique {depart; piece; arrivee; prise = _} ->
      if piece = 6 then begin
        false, false, prn && arrivee <> !depart_tour_noire_pr, grn && arrivee <> !depart_tour_noire_gr
      end
      else if piece = (-6) then begin
        prb && arrivee <> !depart_tour_blanche_pr, grb && arrivee <> !depart_tour_blanche_gr, false, false
      end
      else begin
        prb && depart <> !depart_tour_blanche_pr && arrivee <> !depart_tour_blanche_pr, grb && depart <> !depart_tour_blanche_gr && arrivee <> !depart_tour_blanche_gr, prn && depart <> !depart_tour_noire_pr && arrivee <> !depart_tour_noire_pr, grn && depart <> !depart_tour_noire_gr && arrivee <> !depart_tour_noire_gr
      end
    |Roque {sorte} ->
      if sorte = 1 || sorte = 2 then begin
        false, false, prn, grn
      end
      else begin
        prb, grb, false, false
      end
    |Promotion {depart = _; arrivee; promotion = _; prise = _} ->
      prb && arrivee <> !depart_tour_blanche_pr, grb && arrivee <> !depart_tour_blanche_gr, prn && arrivee <> !depart_tour_noire_pr, grn && arrivee <> !depart_tour_noire_gr
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
    |1 -> plateau.(!depart_roi_blanc) <- 0; plateau.(!depart_tour_blanche_pr) <- 0; plateau.(62) <- 6; plateau.(61) <- 4
    |2 -> plateau.(!depart_roi_blanc) <- 0; plateau.(!depart_tour_blanche_gr) <- 0; plateau.(58) <- 6; plateau.(59) <- 4
    |3 -> plateau.(!depart_roi_noir) <- 0; plateau.(!depart_tour_noire_pr) <- 0; plateau.(6) <- (-6); plateau.(5) <- (-4)
    |_ -> plateau.(!depart_roi_noir) <- 0; plateau.(!depart_tour_noire_gr) <- 0; plateau.(2) <- (-6); plateau.(3) <- (-4)
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
    |1 -> plateau.(62) <- 0;  plateau.(61) <- 0; plateau.(!depart_tour_blanche_pr) <- 4;  plateau.(!depart_roi_blanc) <- 6
    |2 -> plateau.(58) <- 0; plateau.(59) <- 0; plateau.(!depart_tour_blanche_gr) <- 4;  plateau.(!depart_roi_blanc) <- 6
    |3 -> plateau.(6) <- 0; plateau.(5) <- 0; plateau.(!depart_tour_noire_pr) <- (-4);  plateau.(!depart_roi_noir) <- (-6)
    |_ -> plateau.(2) <- 0; plateau.(3) <- 0; plateau.(!depart_tour_noire_gr) <- (-4);  plateau.(!depart_roi_noir) <- (-6)
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
  |Classique {piece = _; depart; arrivee = _; prise = _} | Enpassant {depart; arrivee = _} | Promotion {depart; arrivee = _; prise = _; promotion = _} -> depart
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

(*Fonction indiquant si une pièce cloue une pièce clouable*)
let cloue plateau chessman trait_aux_blancs cord64 vect distance =
  let b = ref false in
  let k = ref distance in
  let s = ref true in
  let dame_adverse = dame (not trait_aux_blancs) in
  while (!s && tab120.(cord64 + (!k * vect)) <> (-1)) do
    let candidat = tab120.(cord64 + (!k * vect)) in
    let dest = plateau.(candidat) in
    if dame_adverse * dest < 0 then begin
      s :=  false
    end
    else if dame_adverse * dest > 0 then begin 
      if dest = chessman || dest = dame_adverse then begin
        b := true;
      end;
      s :=  false
    end;
    incr k
  done;
  !b

(*Fonction donnant les cases des pièces clouées (indépendanmment de leur possibilité de mouvement) en considérant que le roi est dans la case en argument*)
let clouees plateau case_roi trait_aux_blancs =
  let ensemble = ref [] in
  let t = tab64.(case_roi) in
  if trait_aux_blancs then begin
    for i = 0 to 3 do
      let dir = vect_tour.(i) in
      let k = ref 1 in
      let s = ref true in
      while (!s && tab120.(t + (!k * dir)) <> (-1)) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest > 0 then begin
          if cloue plateau (-4) trait_aux_blancs t dir (!k + 1) then begin
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
      while (!s && tab120.(t + (!k * dir)) <> (-1)) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest > 0 then begin
          if cloue plateau (-3) trait_aux_blancs t dir (!k + 1) then begin
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
      while (!s && tab120.(t + (!k * dir)) <> (-1)) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest < 0 then begin
          if cloue plateau 4 trait_aux_blancs t dir (!k + 1) then begin
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
      while (!s && tab120.(t + (!k * dir)) <> (-1)) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest < 0 then begin
          if cloue plateau 3 trait_aux_blancs t dir (!k + 1) then begin
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
let coups_valides plateau trait_aux_blancs dernier_coup (prb, grb, prn, grn) =
  let l = ref [] in
  let roi_joueur = roi trait_aux_blancs in
  let position_roi = index_tableau plateau roi_joueur in
  if menacee plateau position_roi trait_aux_blancs then begin
    let cp = ref ((enpassant plateau trait_aux_blancs dernier_coup) @ deplacements_all plateau trait_aux_blancs) in
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
    let cp = ref (deplacements_all plateau trait_aux_blancs) in
    let droit_au_roque = if trait_aux_blancs then prb || grb else prn || grn in
    let piece_clouees = clouees plateau position_roi trait_aux_blancs in
    let rec aux_en_passant liste = match liste with
      |[] -> ()
      |coup :: liste_coup -> begin
        joue plateau coup;
        if not (menacee plateau position_roi trait_aux_blancs) then begin
          l := coup :: !l
        end;
        dejoue plateau coup;
        aux_en_passant liste_coup
    end
    in aux_en_passant (enpassant plateau trait_aux_blancs dernier_coup);
    if piece_clouees = [] then begin
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
          l := coup :: !l
        end;
        cp := List.tl !cp
      done;
      if droit_au_roque then (roque plateau trait_aux_blancs (prb, grb, prn, grn)) @ !l else !l
    end
    else begin
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
            if not (List.mem (depart coup) piece_clouees) then begin
              l := coup :: !l
            end
            else begin
              joue plateau coup;
              if not (menacee plateau position_roi trait_aux_blancs) then begin
                l := coup :: !l
              end;
              dejoue plateau coup
            end
          end;
        cp := List.tl !cp
      done;
      if droit_au_roque then (roque plateau trait_aux_blancs (prb, grb, prn, grn)) @ !l else !l
    end
  end

(*Fonction indiquant si un coup est valide*)
let est_valide plateau coup joueur =
  let b = ref true in
  joue plateau coup;
  if piece coup = (roi joueur) then begin
    if (menacee plateau (arrivee coup) (plateau.(arrivee coup) > 0)) then begin
      b := false
    end
  end
  else if (menacee plateau (index_tableau plateau (roi joueur)) (plateau.(arrivee coup) > 0)) then begin
    b := false
  end;
  dejoue plateau coup;
  !b

(*Fonction indiquant efficacement si un coup est valide, utilisée uniquement pour des coups classiques, hors roi*)
let est_valide_efficace plateau coup position_roi roi_en_echec piece_clouees =
  let b = ref true in
  if roi_en_echec then begin
    joue plateau coup;
    if (menacee plateau position_roi (plateau.(position_roi) > 0)) then begin
      b := false
    end;
    dejoue plateau coup
  end
  else if List.mem (depart coup) piece_clouees then begin
    joue plateau coup;
    if menacee plateau position_roi (plateau.(position_roi) > 0) then begin
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
      if menacee plateau (index_tableau plateau 6) true then
        vainqueur := -1
      else
        vainqueur := 0
    end
  end
  else begin
    if (coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)) = [] then begin
      if menacee plateau (index_tableau plateau (-6)) false then
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