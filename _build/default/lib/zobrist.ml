(*Module implémentant une fonction de hachage de*)
open Plateau
open Generateur

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de prise en passant*)
let tab_zobrist = Array.make 781 0

let rec f x y = if y = 0 then 1 else x * (f x (y - 1))

let () =
  for i = 0 to 780 do
    tab_zobrist.(i) <- Int64.to_int (Random.int64 4611686018427387903L)
  done

(*Indique la colonne d'une prise en passant potentielle*)
let colonne_ep_1 coup plateau = match coup with
  |Classique {piece; depart; arrivee; prise} when (piece = 1 && prise = 0 && (depart > 47) && (arrivee < 40)) ->
    let droite = arrivee + 1 in
    if  droite <> 40 && plateau.(droite) = (-1) then begin
      (depart - 8) mod 8
    end
    else begin
      let gauche = arrivee - 1 in
      if (gauche <> 31 && plateau.(gauche) = (-1)) then begin
        (depart - 8) mod 8
      end
      else begin
        -1
      end
    end
  |Classique {piece; depart; arrivee; prise} when (piece = (-1) && prise = 0 && (depart < 16) && (arrivee > 23)) -> 
    let droite = arrivee + 1 in
    if  droite <> 32 && plateau.(droite) = 1 then begin
      (depart + 8) mod 8
    end
    else begin
      let gauche = arrivee - 1 in
      if (gauche <> 23 && plateau.(gauche) = 1) then begin
        (depart + 8) mod 8
      end
      else begin
        -1
      end
    end
  |_ -> -1

let colonne_ep_2 coup = match coup with
    |Enpassant {depart = _; arrivee} -> arrivee mod 8
    |_ -> (-1)

(*Fonction de hachage*)
let zobrist plateau trait_aux_blancs dernier_coup droit_au_roque =
  let h = ref 0 in
  for i = 0 to 63 do
    let piece = plateau.(i) in
    if piece > 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (piece - 1))
    end
    else if piece < 0 then begin
      h := !h lxor tab_zobrist.(12 * i + (5 - piece))
    end
  done;
  if trait_aux_blancs then begin
    h := !h lxor tab_zobrist.(768)
  end;
  let prb, grb, prn, grn = droit_au_roque in
  if prb then begin
    h := !h lxor tab_zobrist.(769)
  end;
  if grb then begin
    h := !h lxor tab_zobrist.(770)
  end;
  if prn then begin
    h := !h lxor tab_zobrist.(771)
  end;
  if grn then begin
    h := !h lxor tab_zobrist.(772)
  end;
  let pep = colonne_ep_1 dernier_coup plateau in
  if pep <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep)
  end;
  !h

(*Fonction caulculant la valeur de la fonction de zobrist en fonction de la précédente et du coup joué. Non utilisée.*)
let nouveau_zobrist plateau coup ancien_zobrist droit_au_roque =
  let h = ref (ancien_zobrist lxor tab_zobrist.(768)) in
  let pep_joueur = colonne_ep_1 coup plateau in
  if pep_joueur <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep_joueur)
  end;
  let prb, grb, prn, grn = droit_au_roque in
  let nprb, ngrb, nprn, ngrn = modification_roque coup droit_au_roque in
  if prb <> nprb then begin
    h:= !h lxor tab_zobrist.(769)
  end;
  if grb <> ngrb then begin
    h:= !h lxor tab_zobrist.(770)
  end;
  if prn <> nprn then begin
    h:= !h lxor tab_zobrist.(771)
  end;
  if grn <> ngrn then begin
    h:= !h lxor tab_zobrist.(772)
  end;
  let aux coup = match coup with
    |Classique {piece; depart; arrivee; prise} -> begin
      if piece > 0 then begin
        if prise = 0 then begin
          tab_zobrist.(12 * depart + (piece - 1)) lxor tab_zobrist.(12 * arrivee + (piece - 1))
        end
        else begin
          tab_zobrist.(12 * depart + (piece - 1)) lxor tab_zobrist.(12 * arrivee + (piece - 1)) lxor tab_zobrist.(12 * arrivee + (5 - prise))
        end
      end
      else begin
        if prise = 0 then begin
          tab_zobrist.(12 * depart + (5 - piece)) lxor tab_zobrist.(12 * arrivee + (5 - piece))
        end
        else begin
          tab_zobrist.(12 * depart + (5 - piece)) lxor tab_zobrist.(12 * arrivee + (5 - piece)) lxor tab_zobrist.(12 * arrivee + (prise - 1))
        end
      end
    end
    |Roque {sorte} -> begin
      match sorte with
      |1 -> tab_zobrist.(725) lxor tab_zobrist.(749) lxor tab_zobrist.(759) lxor tab_zobrist.(735)
      |2 -> tab_zobrist.(725) lxor tab_zobrist.(701) lxor tab_zobrist.(675) lxor tab_zobrist.(711)
      |3 -> tab_zobrist.(59) lxor tab_zobrist.(83) lxor tab_zobrist.(93) lxor tab_zobrist.(69)
      |_ -> tab_zobrist.(59) lxor tab_zobrist.(35) lxor tab_zobrist.(9) lxor tab_zobrist.(45)
    end
    |Enpassant {depart; arrivee} -> begin
      if depart < 32 then begin
        tab_zobrist.(12 * depart) lxor tab_zobrist.(12 * arrivee) lxor tab_zobrist.(12 * (arrivee + 8) + 6) lxor tab_zobrist.(773 + (colonne_ep_2 coup))
      end
      else begin
        tab_zobrist.(12 * depart + 6) lxor tab_zobrist.(12 * arrivee + 6) lxor tab_zobrist.(12 * (arrivee - 8)) lxor tab_zobrist.(773 + (colonne_ep_2 coup))
      end
    end
    |Promotion {depart; arrivee; promotion; prise} -> begin
      if arrivee < 8 then begin
        if prise = 0 then begin
          tab_zobrist.(12 * depart) lxor tab_zobrist.(12 * arrivee + (promotion - 1))
        end
        else begin
          tab_zobrist.(12 * depart) lxor tab_zobrist.(12 * arrivee + (promotion - 1)) lxor tab_zobrist.(12 * arrivee + (5 - prise))
        end
      end
      else begin
        if prise = 0 then begin
          tab_zobrist.(12 * depart + 6) lxor tab_zobrist.(12 * arrivee + (5 - promotion))
        end
        else begin
          tab_zobrist.(12 * depart + 6) lxor tab_zobrist.(12 * arrivee + (5 - promotion)) lxor tab_zobrist.(12 * arrivee + (prise - 1))
        end
      end
    end
    |Aucun -> 0
  in
  !h lxor aux coup