(*Module implémentant les fonctions qui permettent de traduire les coups de la notation algébrique vers la notation avec le type Mouvement*)

open Plateau
open Generateur

(*Dictionnaire associant une pièce en notation algébrique anglaise à la valeur des pièces pour le moteur*)
let dicosimple =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', 4); ('N', 2); ('B', 3); ('Q', 5); ('K', 6)];
  ht

(*Fonction traduisant une prise en passant de la notation algébrique vers la notation avec le type Mouvement*)
let origine_en_passant coup trait_aux_blancs =
  let arrivee = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ (String.make 1 coup.[2])) in
  let depart =
    if trait_aux_blancs then 
      Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) - 1)) 
    else
      Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) + 1))
  in Enpassant {depart = depart ; arrivee = arrivee}

(*Fonction traduisant une notation algébrique exhaustive vers la notation avec le type Mouvement*)
let origine_simple coup trait_aux_blancs plateau =
  let piece =
    if trait_aux_blancs then begin
      Hashtbl.find dicosimple coup.[0]
    end
    else begin
      - (Hashtbl.find dicosimple coup.[0])
    end in
  let depart = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ (String.make 1 coup.[2])) in
  let arrivee = Hashtbl.find dicocoord ((String.make 1 coup.[3]) ^ (String.make 1 coup.[4])) in
  Classique {piece = piece; depart = depart; arrivee = arrivee; prise = plateau.(arrivee)}

(*Fonction traduisant le coup d'un pion de la notation algébrique vers la notation avec le type Mouvement*)
let origine_pion plateau coup trait_aux_blancs =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    if String.length coup = 2 then begin
      if plateau.(case + 8) = 1 then begin
        depart := case + 8
      end
      else begin
        depart := case + 16
      end
    end
    else begin
      depart := Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) - 1))
    end
  end
  else begin
    if String.length coup = 2 then begin
      if plateau.(case - 8) = (-1) then begin
        depart := case - 8
      end
      else begin
        depart := case - 16
      end
    end
    else begin
      depart := Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) + 1))
    end
  end;
  Classique {piece = (pion trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'une tour de la notation algébrique vers la notation avec le type Mouvement*)
let origine_tour plateau coup trait_aux_blancs coups_valides_joueur =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    if String.length coup = 3 then begin
      let t = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 4) do
        let dir = vect_tour.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(t + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(t + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 4 && (List.mem (Classique {piece = 4; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = 4 && (List.mem (Classique {piece = 4; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = 4 && (List.mem (Classique {piece = 4; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end
  else begin
    if String.length coup = 3 then begin
      let t = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 4) do
        let dir = vect_tour.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(t + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(t + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = (-4) && (List.mem (Classique {piece = (-4); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = (-4) && (List.mem (Classique {piece = (-4); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = (-4) && (List.mem (Classique {piece = (-4); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end;
  Classique {piece = (tour trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'un fou de la notation algébrique vers la notation avec le type Mouvement*)
let origine_fou plateau coup trait_aux_blancs coups_valides_joueur =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    if String.length coup = 3 then begin
      let f = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 4) do
        let dir = vect_fou.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(f + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(f + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 3 && (List.mem (Classique {piece = 3; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = 3 && (List.mem (Classique {piece = 3; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = 3 && (List.mem (Classique {piece = 3; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end
  else begin
    if String.length coup = 3 then begin
      let f = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 4) do
        let dir = vect_fou.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(f + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(f + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = (-3) && (List.mem (Classique {piece = (-3); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = (-3) && (List.mem (Classique {piece = (-3); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = (-3) && (List.mem (Classique {piece = (-3); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end;
  Classique {piece = (fou trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'une dame de la notation algébrique vers la notation avec le type Mouvement*)
let origine_dame plateau coup trait_aux_blancs coups_valides_joueur =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    if String.length coup = 3 then begin
      let r = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let dir = vect_roi.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(r + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(r + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 5 && (List.mem (Classique {piece = 5; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = 5 && (List.mem (Classique {piece = 5; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = 5 && (List.mem (Classique {piece = 5; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end
  else begin
    if String.length coup = 3 then begin
      let r = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let dir = vect_roi.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(r + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(r + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = (-5) && (List.mem (Classique {piece = (-5); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat;
            s := false
          end
          else if dest = 0 then begin
            k := !k + 1
          end
          else begin
            s := false
          end
        done;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = (-5) && (List.mem (Classique {piece = (-5); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = (-5) && (List.mem (Classique {piece = (-5); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end;
  Classique {piece = (dame trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'un cavalier de la notation algébrique vers la notation avec le type Mouvement*)
let origine_cavalier plateau coup trait_aux_blancs coups_valides_joueur =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    if String.length coup = 3 then begin
      let c = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let dir = vect_cavalier.(!i) in
        if tab120.(c + dir) <> (-1) then begin
          let candidat = tab120.(c + dir) in
          if plateau.(candidat) = 2 && (List.mem (Classique {piece = 2; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end
        end;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = 2 && (List.mem (Classique {piece = 2; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = 2 && (List.mem (Classique {piece = 2; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end
  else begin
    if String.length coup = 3 then begin
      let c = tab64.(case) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let dir = vect_cavalier.(!i) in
        if tab120.(c + dir) <> (-1) then begin
          let candidat = tab120.(c + dir) in
          if plateau.(candidat) = (-2) && (List.mem (Classique {piece = (-2); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end
        end;
        i := !i + 1
      done
    end
    else begin
      let x = (int_of_char coup.[1]) in
      if (x > 48 && x < 57) then begin
        let case0 = Hashtbl.find dicocoord ("a" ^ (String.make 1 coup.[1])) in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + !i in
          if plateau.(candidat) = (-2) && (List.mem (Classique {piece = (-2); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
      else begin
        let case0 = Hashtbl.find dicocoord ((String.make 1 coup.[1]) ^ "8") in
        let i = ref 0 in
        while (!depart = (-1) && !i < 8) do
          let candidat = case0 + (8 * !i) in
          if plateau.(candidat) = (-2) && (List.mem (Classique {piece = (-2); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
            depart := candidat
          end;
          i := !i + 1
        done
      end
    end
  end;
  Classique {piece = (cavalier trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'un roi de la notation algébrique vers la notation avec le type Mouvement*)
let origine_roi plateau coup trait_aux_blancs coups_valides_joueur =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 2]) ^ (String.make 1 coup.[l - 1])) in
  if trait_aux_blancs then begin
    let c = tab64.(case) in
    let i = ref 0 in
    while (!depart = (-1) && !i < 8) do
      let dir = vect_roi.(!i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        if plateau.(candidat) = 6 && (List.mem (Classique {piece = 6; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
          depart := candidat
        end
      end;
      i := !i + 1
    done
  end
  else begin
    let c = tab64.(case) in
    let i = ref 0 in
    while (!depart = (-1) && !i < 8) do
      let dir = vect_roi.(!i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        if plateau.(candidat) = (-6) && (List.mem (Classique {piece = (-6); depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
          depart := candidat
        end
      end;
      i := !i + 1
    done
  end;
  Classique {piece = (roi trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant une promotion en notation algébrique vers la notation avec le type Mouvement*)
let origine_promotion coup trait_aux_blancs plateau =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.make 1 coup.[l - 4]) ^ (String.make 1 coup.[l - 3])) in
  let promo = ref 0 in
  if trait_aux_blancs then begin
    promo := Hashtbl.find dicosimple (Char.uppercase_ascii coup.[l - 1]);
    if String.length coup = 4 then begin
      depart := case + 8
    end
    else begin
      depart := Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) - 1))
    end
  end
  else begin
    promo := - (Hashtbl.find dicosimple (Char.uppercase_ascii coup.[l - 1]));
    if String.length coup = 4 then begin
      depart := case - 8
    end
    else begin
      depart := Hashtbl.find dicocoord ((String.make 1 (coup.[0])) ^ string_of_int ((int_of_string (String.make 1 (coup.[2]))) + 1))
    end
  end;
  Promotion {depart = !depart; arrivee = case; promotion = !promo; prise = plateau.(case)}

(*Fonction supprimant les caractères dispensables de la notation algébrique*)
let supprimer chaine =
  let nc = ref "" in
  for i = 0 to ((String.length chaine) - 1) do
    let k = chaine.[i] in
    if (k <> 'x' && k <> '(' && k <> ')' && k <> '+' && k <> '.' && k <> '?' && k <> '!') then begin
      nc := !nc ^ (String.make 1 k)
    end
  done;
  !nc

(*Fonction décomposant une chaine de caractère en liste de substring correspondants aux mots*)
let detecte_mots chaine =
   Str.split (Str.regexp " +") chaine

(*Fonction vérifiant si une chaine de caractère représente un entier*)
let est_entier_string chaine =
  let i = try int_of_string chaine with _ -> (-1) in
  i > 0

(*Fonction convertissant la notation d'un string de coups notés algébriquement, en une liste de coups en notation algébrique*)
let algebric_to_algebric_list algebric =
  let rec supprime_compteur_coups liste  = match liste with
    |[] -> []
    |h :: t ->
      if est_entier_string h then begin
        (supprime_compteur_coups t)
      end
      else begin
        h :: (supprime_compteur_coups t)
      end
  in supprime_compteur_coups (detecte_mots (supprimer algebric))

(*Dictionnaire associant une pièce en notation algébrique anglaise à la fonction à appliquer pour trouver sa case de départ*)
let dicorigine =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', origine_tour); ('N', origine_cavalier); ('B', origine_fou);
      ('Q', origine_dame); ('K', origine_roi)];
  ht

(*Traduit un coup noté en notation algébrique en sa notation avec le type mouvement*)
let mouvement_of_algebric plateau coup trait_aux_blancs coups_valides_joueur =
  let coup_traduit = ref Aucun in
  if coup = "0-0" || coup = "O-O" then begin
    coup_traduit := if trait_aux_blancs then Roque {sorte = 1} else Roque {sorte = 3}
  end
  else if coup = "0-0-0" || coup = "O-O-O" then begin
    coup_traduit := if trait_aux_blancs then Roque {sorte = 2} else Roque {sorte = 4}
  end
  else if String.contains coup 'p' then begin
    coup_traduit := origine_en_passant coup trait_aux_blancs
  end
  else if String.contains coup '=' then begin
    coup_traduit := origine_promotion coup trait_aux_blancs plateau
  end
  else begin match String.length coup with
    |2 |3 when coup.[0] = Char.lowercase_ascii coup.[0] && coup.[0] <> coup.[1] -> coup_traduit := origine_pion plateau coup trait_aux_blancs
    |5 -> coup_traduit := origine_simple coup trait_aux_blancs plateau
    |_-> coup_traduit := (Hashtbl.find dicorigine coup.[0]) plateau coup trait_aux_blancs coups_valides_joueur
  end;
  if not (List.mem !coup_traduit coups_valides_joueur) then begin
    failwith "Coup invalide"
  end
  else begin
    !coup_traduit
  end

(*Dictionnaire associant la notation algébrique française des pièces à leur notation algébrique anglaise*)
let dicofrench =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('T', "R"); ('C', "N"); ('F', "B");
      ('D', "Q"); ('R', "K")];
  ht

(*Fonction permettant une tolérance à l'approximation de l'utilisateur dans sa saisie*)
let tolerance plateau coup trait_aux_blancs coups_valides_joueur =
  try mouvement_of_algebric plateau coup trait_aux_blancs coups_valides_joueur with _ ->
  try mouvement_of_algebric plateau (coup ^ "ep") trait_aux_blancs coups_valides_joueur with _ ->
  try mouvement_of_algebric plateau (String.capitalize_ascii coup) trait_aux_blancs coups_valides_joueur with _ ->
  try mouvement_of_algebric plateau (Hashtbl.find dicofrench coup.[0] ^ String.sub coup 1 (String.length coup - 1)) trait_aux_blancs coups_valides_joueur with _ ->
  try mouvement_of_algebric plateau (Hashtbl.find dicofrench (Char.uppercase_ascii coup.[0]) ^ String.sub coup 1 (String.length coup - 1)) trait_aux_blancs coups_valides_joueur with _ ->
  Aucun

(*Fonction convertissant une liste de coups notés algébriquement en une liste de coups notés avec le type Mouvement*)
let algebric_list_to_type_mouvement algebric_list trait_aux_blancs dernier_coup droit_au_roque_initial position_de_depart =
  let plateau = Array.copy position_de_depart in
  let white_to_move = ref trait_aux_blancs in
  let last_move = ref dernier_coup in
  let right_to_castle = ref droit_au_roque_initial in
  let liste_algebric = ref algebric_list in
  let liste_type_mouvement = ref [] in
  while !liste_algebric <> [] do
    let coup = List.hd !liste_algebric in
    let coup_traduit = tolerance plateau coup !white_to_move (coups_valides plateau !white_to_move !last_move !right_to_castle) in
    liste_type_mouvement := coup_traduit :: !liste_type_mouvement;
    joue_coup_1 plateau coup_traduit white_to_move last_move right_to_castle;
    liste_algebric := List.tl !liste_algebric;
  done;
  List.rev !liste_type_mouvement

(*Fonction convertissant la notation d'un string de coups notés algébriquement en une liste de coups notés avec le type Mouvement*)
let algebric_to_type_mouvement algebric trait_aux_blancs dernier_coup droit_au_roque position_de_depart =
  algebric_list_to_type_mouvement (algebric_to_algebric_list algebric) trait_aux_blancs dernier_coup droit_au_roque position_de_depart

(*Fonction convertissant un répertoire d'ouvertures en une liste de liste de coups notés avec le type Mouvement*)
let traduction algebrique =
  let detecte_sauts_de_ligne = Str.split (Str.regexp "\n") algebrique
  in let rec fonc liste = match liste with
    |[] -> []
    |h :: t -> algebric_to_type_mouvement h true Aucun (true, true, true, true) echiquier :: fonc t
  in fonc detecte_sauts_de_ligne