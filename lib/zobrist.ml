(*Module implémentant une fonction de hachage de*)
open Plateau

(*Création d'un tableau de nombres pseudo aléatoires. 12 * 64 cases
  pour chaque pièce de chaque case, + 1 case pour indiquer le trait + 4 cases
  pour les droits roques + 8 cases pour les colonnes de prise en passant*)
let tab_zobrist = Array.make 781 0

let () =
  for i = 0 to 780 do
    tab_zobrist.(i) <- Int64.to_int (Random.int64 4611686018427387903L)
  done

(*Indique la colonne d'une prise en passant potentielle*)
let colonne_ep coup = match coup with
  |Classique {piece; depart; arrivee; prise = _} when (abs piece = 1 && abs (depart - arrivee) = 16) -> depart mod 8
  |_ -> -1

(*Fonction de hachage*)
let zobrist plateau trait_aux_blancs dernier_coup (prb, grb, prn, grn) =
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
  let pep = colonne_ep dernier_coup in
  if pep <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep)
  end;
  !h

(*Fonction caulculant la valeur de la fonction de zobrist en fonction de la précédente et du coup joué. Non utilisée.*)
let nouveau_zobrist coup avant_dernier_coup ancien_zobrist (aprb, agrb, aprn, agrn) (nprb, ngrb, nprn, ngrn) =
  let h = ref (ancien_zobrist lxor tab_zobrist.(768)) in
  let pep_adversaire = colonne_ep avant_dernier_coup in
  if pep_adversaire <> (-1) then begin
    h := !h lxor tab_zobrist.(773 + pep_adversaire)
  end;
  if aprb <> nprb then begin
    h:= !h lxor tab_zobrist.(769)
  end;
  if agrb <> ngrb then begin
    h:= !h lxor tab_zobrist.(770)
  end;
  if aprn <> nprn then begin
    h:= !h lxor tab_zobrist.(771)
  end;
  if agrn <> ngrn then begin
    h:= !h lxor tab_zobrist.(772)
  end;
  let aux coup = match coup with
    |Classique {piece; depart; arrivee; prise} -> begin
      if (abs piece = 1 && abs (depart - arrivee) = 16) then begin
        h := !h lxor tab_zobrist.(773 + (depart mod 8))
      end;
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
        tab_zobrist.(12 * depart) lxor tab_zobrist.(12 * arrivee) lxor tab_zobrist.(12 * (arrivee + 8) + 6)
      end
      else begin
        tab_zobrist.(12 * depart + 6) lxor tab_zobrist.(12 * arrivee + 6) lxor tab_zobrist.(12 * (arrivee - 8))
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