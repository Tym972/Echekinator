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
  let arrivee = Hashtbl.find dicocoord ((String.sub coup 1 1) ^ (String.sub coup 2 1)) in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  let depart = Hashtbl.find dicocoord ((String.sub coup 0 1) ^ string_of_int (int_of_string (String.sub coup 2 1) - signe_joueur))
  in Enpassant {depart = depart ; arrivee = arrivee}

(*Fonction traduisant une notation algébrique exhaustive vers la notation avec le type Mouvement*)
let origine_simple coup trait_aux_blancs plateau =
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  let piece = signe_joueur * Hashtbl.find dicosimple coup.[0] in
  let depart = Hashtbl.find dicocoord ((String.sub coup 1 1) ^ (String.sub coup 2 1)) in
  let arrivee = Hashtbl.find dicocoord ((String.sub coup 3 1) ^ (String.sub coup 4 1)) in
  Classique {piece = piece; depart = depart; arrivee = arrivee; prise = plateau.(arrivee)}

(*Fonction traduisant le coup d'un pion de la notation algébrique vers la notation avec le type Mouvement*)
let origine_pion plateau coup trait_aux_blancs =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.sub coup (l - 2) 1) ^ (String.sub coup (l - 1) 1)) in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  if String.length coup = 2 then begin
    if plateau.(case + 8 * signe_joueur) = signe_joueur then begin
      depart := case + 8 * signe_joueur
    end
    else begin
      depart := case + 16 * signe_joueur
    end
  end
  else begin
    depart := Hashtbl.find dicocoord ((String.sub coup 0 1) ^ string_of_int ((int_of_string (String.sub coup 2 1)) - signe_joueur))
  end;
  Classique {piece = (pion trait_aux_blancs); depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant le coup d'une tour de la notation algébrique vers la notation avec le type Mouvement*)
let origine_piece plateau coup coups_valides_joueur piece vect_piece =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.sub coup (l - 2) 1) ^ (String.sub coup (l - 1) 1)) in
  if String.length coup = 3 then begin
    let t = tab64.(case) in
    let i = ref 0 in
    while (!depart = (-1) && !i < Array.length vect_piece) do
      let dir = vect_piece.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = plateau.(candidat) in
        if dest = piece && (List.mem (Classique {piece = piece; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
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
      let case0 = Hashtbl.find dicocoord ("a" ^ (String.sub coup 1 1)) in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let candidat = case0 + !i in
        if plateau.(candidat) = piece && (List.mem (Classique {piece = piece; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
          depart := candidat
        end;
        i := !i + 1
      done
    end
    else begin
      let case0 = Hashtbl.find dicocoord ((String.sub coup 1 1) ^ "8") in
      let i = ref 0 in
      while (!depart = (-1) && !i < 8) do
        let candidat = case0 + (8 * !i) in
        if plateau.(candidat) = piece && (List.mem (Classique {piece = piece; depart = candidat; arrivee = case; prise = plateau.(case)}) coups_valides_joueur) then begin
          depart := candidat
        end;
        i := !i + 1
      done
    end
  end;
  Classique {piece = piece; depart = !depart; arrivee = case; prise = plateau.(case)}

(*Fonction traduisant une promotion en notation algébrique vers la notation avec le type Mouvement*)
let origine_promotion coup trait_aux_blancs plateau =
  let depart = ref (-1) in
  let l = String.length coup in
  let case = Hashtbl.find dicocoord ((String.sub coup (l - 4) 1) ^ (String.sub coup (l - 3) 1)) in
  let promo = ref 0 in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  promo := signe_joueur * Hashtbl.find dicosimple (Char.uppercase_ascii coup.[l - 1]);
  if String.length coup = 4 then begin
    depart := case + signe_joueur * 8
  end
  else begin
    depart := Hashtbl.find dicocoord ((String.sub coup 0 1) ^ string_of_int ((int_of_string (String.sub coup 2 1)) - signe_joueur))
  end;
  Promotion {depart = !depart; arrivee = case; promotion = !promo; prise = plateau.(case)}

(*Fonction décomposant une chaine de caractère en liste de substring correspondants aux mots*)
let detecte_mots chaine =
  Str.split (Str.regexp " +") chaine

(*Fonction vérifiant si une chaine de caractère représente un entier*)
let est_entier_string chaine =
  let i = try int_of_string chaine with _ -> (-1) in
  i > 0

(*Fonction convertissant la notation d'un string de coups notés algébriquement, en une liste de coups en notation algébrique*)
let algebric_list_of_san algebric =
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
let dicorigin =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('R', (4, vect_tour)); ('N', (2, vect_cavalier)); ('B', (3, vect_fou));
      ('Q', (5, vect_roi)); ('K', (6, vect_roi))];
  ht

(*Traduit un coup noté en notation algébrique en sa notation avec le type mouvement*)
let move_of_algebric plateau coup trait_aux_blancs coups_valides_joueur =
  let coup_traduit = ref Aucun in
  if List.mem coup ["0-0"; "O-O"] then begin
    coup_traduit := if trait_aux_blancs then Roque {sorte = 1} else Roque {sorte = 3}
  end
  else if List.mem coup ["0-0-0"; "O-O-O"] then begin
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
    |_->
      let piece, vect_piece = (Hashtbl.find dicorigin coup.[0]) in
      coup_traduit := origine_piece plateau coup coups_valides_joueur (if trait_aux_blancs then piece else (- piece)) vect_piece
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

(*Fonction interprétant la notation UCI*)
let mouvement_of_uci uci plateau coups_valides_joueur =
  let coup = ref Aucun in
    let depart = Hashtbl.find dicocoord (String.sub uci 0 2) in
    let arrivee = Hashtbl.find dicocoord (String.sub uci 2 2) in
    let promotion = try Hashtbl.find dicosimple (Char.uppercase_ascii uci.[4]) with _ -> 10 in
    let trait_aux_blancs = plateau.(depart) > 0 in
    let signe_joueur = if trait_aux_blancs then 1 else (-1) in
    if promotion <> 10 then begin
      coup := Promotion {depart; arrivee; prise = plateau.(arrivee); promotion = signe_joueur * promotion}
    end
    else if plateau.(depart) = 6 * signe_joueur && (arrivee / 8 = depart / 8) && (abs (arrivee - depart) <> 1 || plateau.(arrivee) * signe_joueur > 0) then begin
      let roque_joueur = if trait_aux_blancs then 1 else 3 in
      let arrivee_pr, depart_tour_pr, arrivee_gr, depart_tour_gr = if trait_aux_blancs then 62, !depart_tour_blanche_pr, 58, !depart_tour_blanche_gr else 6, !depart_tour_noire_pr, 2, !depart_tour_noire_gr in
      if (not !chess_960 && arrivee = arrivee_pr) || (!chess_960 && arrivee = depart_tour_pr) then begin
        coup := Roque {sorte = roque_joueur}
      end
      else if (not !chess_960 && arrivee = arrivee_gr) || (!chess_960 && arrivee = depart_tour_gr) then begin
        coup := Roque {sorte = roque_joueur + 1}
      end
    end
    else if plateau.(depart) = signe_joueur && (depart - arrivee) mod 8 <> 0 && plateau.(arrivee) = 0 then begin
      coup := Enpassant {depart; arrivee}
    end
    else begin
      coup := Classique {piece = plateau.(depart); depart; arrivee; prise = plateau.(arrivee)}
    end;
    if not (List.mem !coup coups_valides_joueur) then begin
      failwith "Coup invalide"
    end
    else begin
      !coup
    end
  
(*Fonction permettant une tolérance à l'approximation de l'utilisateur dans sa saisie*)
let tolerance plateau coup trait_aux_blancs coups_valides_joueur =
  try mouvement_of_uci coup plateau coups_valides_joueur with _ ->
  try move_of_algebric plateau coup trait_aux_blancs coups_valides_joueur with _ ->
  try move_of_algebric plateau (coup ^ "ep") trait_aux_blancs coups_valides_joueur with _ ->
  try move_of_algebric plateau (String.capitalize_ascii coup) trait_aux_blancs coups_valides_joueur with _ ->
  try move_of_algebric plateau (Hashtbl.find dicofrench coup.[0] ^ String.sub coup 1 (String.length coup - 1)) trait_aux_blancs coups_valides_joueur with _ ->
  try move_of_algebric plateau (Hashtbl.find dicofrench (Char.uppercase_ascii coup.[0]) ^ String.sub coup 1 (String.length coup - 1)) trait_aux_blancs coups_valides_joueur with _ -> Aucun

(*Fonction convertissant un relevé de coups notés algébriquement en un relevé de coups notés avec le type Mouvement*)
let move_list_of_algebric_list algebric_list trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart =
  let plateau = Array.copy position_de_depart in
  let white_to_move = ref trait_aux_blancs_initial in
  let last_move = ref dernier_coup_initial in
  let right_to_castle = ref droit_au_roque_initial in
  let liste_algebric = ref algebric_list in
  let liste_type_mouvement = ref [] in
  let verif = ref true in
  while !verif && !liste_algebric <> [] do
    let coup = List.hd !liste_algebric in
    let coups_valides_joueur = coups_valides plateau !white_to_move !last_move !right_to_castle in
    let coup_traduit = tolerance plateau coup !white_to_move coups_valides_joueur in
    if coup_traduit <> Aucun then begin
      liste_type_mouvement := coup_traduit :: !liste_type_mouvement;
      joue_coup_1 plateau coup_traduit white_to_move last_move right_to_castle;
      liste_algebric := List.tl !liste_algebric
    end
    else begin
      verif := false
    end
  done;
  List.rev !liste_type_mouvement

(*Fonction convertissant la notation d'un string de coups notés algébriquement en une liste de coups notés avec le type Mouvement*)
let move_list_of_san san trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart =
  move_list_of_algebric_list (algebric_list_of_san san) trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart

(*Fonction convertissant un répertoire d'ouvertures en une liste de liste de coups notés avec le type Mouvement*)
let traduction algebrique =
  let detecte_sauts_de_ligne = Str.split (Str.regexp "\n") algebrique
  in let rec fonc liste = match liste with
    |[] -> []
    |h :: t -> move_list_of_san h true Aucun (true, true, true, true) echiquier :: fonc t
  in fonc detecte_sauts_de_ligne