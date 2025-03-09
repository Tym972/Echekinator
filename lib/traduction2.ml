(*Module implémentant les fonctions qui permettent de traduire un coup de la notation avec le type Mouvement vers la notation algébrique*)

open Plateau
open Generateur

(*Tableau assoicant la valeur des pièces pour le moteur (indice) à leur notation algébrique anglaise*)
let tab_english = [|""; "P"; "N"; "B"; "R"; "Q"; "K"|]

(*Fonction renvoyant un string (string vide, colonne de départ, ligne de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique*)
let precise depart antecedent precision =
  if (List.length !antecedent) > 0 then begin
    let origine = coord.(depart) in
    let colonne = origine.[0] in
    let ligne = origine.[1] in
    let rec fonc liste a n = match liste with
      |[] -> true
      |h::t -> h.[n] <> a && fonc t a n
    in if fonc !antecedent colonne 0 then begin
      precision := String.make 1 colonne
    end
    else if fonc !antecedent ligne 1 then begin
      precision := String.make 1 ligne
    end
    else begin
      precision := origine
    end
  end

(*Fonction renvoyant un string (string vide, colonne de départ, ligne de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique pour le coup d'une tour*)
let precision_piece plateau depart arrivee coups_valides_joueur piece vect_piece =
  let precision = ref "" in
  let antecedent = ref [] in
  let t = tab64.(arrivee) in
  for i = 0 to (Array.length vect_piece - 1) do
    let dir = vect_piece.(i) in
    let k = ref 1 in
    let s = ref true in
    while (tab120.(t + (!k * dir)) <> (-1) && !s) do
      let candidat = tab120.(t + (!k * dir)) in
      let dest = plateau.(candidat) in
      if (dest = piece && candidat <> depart && (List.mem (Classique {piece = piece; depart = candidat; arrivee; prise = plateau.(arrivee)}) coups_valides_joueur)) then begin
        antecedent := coord.(candidat) :: !antecedent;
        s := false
      end
      else if dest = 0 then begin
        k := !k + 1
      end
      else begin
        s := false
      end
    done
  done;
  precise depart antecedent precision;
  !precision

(*Fonction renvoyant un string (string vide, colonne de départ, ligne de départ ou case de départ), levant une éventuelle ambiguïté de la notation algébrique pour le coup d'un cavalier*)
let precision_cavalier plateau depart arrivee trait_aux_blancs coups_valides_joueur =
  let precision = ref "" in
  let antecedent = ref [] in
  let c = tab64.(arrivee) in
  let cavalier = cavalier trait_aux_blancs in
  for i = 0 to 7 do
    let dir = vect_cavalier.(i) in
    if tab120.(c + dir) <> (-1) then begin
      let candidat = tab120.(c + dir) in
      if (plateau.(candidat) = cavalier && candidat <> depart && (List.mem (Classique {piece = cavalier; depart = candidat; arrivee; prise = plateau.(arrivee)}) coups_valides_joueur)) then begin
        antecedent := coord.(candidat) :: !antecedent;
      end
    end
  done;
  precise depart antecedent precision;
  !precision

(*Tableau associant la valeur des pièces pour le moteur à la fonction à applique pour lever une éventuelle ambiguïté de la notation algébrique*)
let vect = [|vect_fou; vect_tour; vect_roi; vect_roi|]

(*Fonction participant à traduire un coup classique noté en type Mouvement en sa notation algébrique*)
let algebric_of_classique piece depart arrivee prise plateau coups_valides_joueur =
  let trait_aux_blancs =  piece > 0 in
  let algebric = ref "" in
  let prise = if prise <> 0 then "x" else "" in
  let precision = ref "" in
  if (piece = pion trait_aux_blancs && (depart - arrivee) mod 2 <> 0) then begin
    precision := String.sub coord.(depart) 0 1
  end
  else if abs piece = 2 then begin
    precision := precision_cavalier plateau depart arrivee trait_aux_blancs coups_valides_joueur
  end
  else if not (List.mem (abs piece) [1; 6]) then begin
    precision := precision_piece plateau depart arrivee coups_valides_joueur piece vect.((abs piece) - 3)
  end;
  if piece > 0 then begin
    algebric := (if piece <> 1 then tab_english.(piece) else "") ^ !precision ^ prise ^ coord.(arrivee)
  end
  else begin
    algebric := (if piece <> (-1) then tab_english.( - piece) else "") ^ !precision ^ prise ^ coord.(arrivee)
  end;
  !algebric

(*Fonction participant à traduire une promotion notée en type Mouvement en sa notation algébrique*)
let algebric_of_promotion depart arrivee promotion =
  let algebric = ref "" in
  let signe_joueur = if arrivee < 8 then 1 else (-1) in
  if abs (depart - arrivee) = 8 then begin
    algebric := coord.(arrivee) ^ "=" ^ tab_english.(signe_joueur * promotion)
  end
  else begin
    algebric := (String.sub coord.(depart) 0 1) ^ "x" ^ (coord.(arrivee)) ^ "=" ^ tab_english.(signe_joueur * promotion)
  end;
  !algebric

(*Fonction traduisant un coup noté avec le type Mouvement en sa notation algébrique*)
let algebric_of_mouvement coup plateau coups_valides_joueur = match coup with
  |Enpassant {depart = depart; arrivee = arrivee} -> String.sub coord.(depart) 0 1 ^ "x" ^ coord.(arrivee) ^ "ep"
  |Roque {sorte} -> if sorte mod 2 = 1 then "0-0" else "0-0-0"
  |Classique {piece = piece; depart = depart; arrivee = arrivee; prise} -> algebric_of_classique piece depart arrivee prise plateau coups_valides_joueur
  |Promotion {depart = depart; arrivee = arrivee; promotion = promotion; prise = _} -> algebric_of_promotion depart arrivee promotion
  |_ -> ""

(*Fonction traduisant un relevé de coups en type Mouvement en sa notation algébrique*)
let type_mouvement_to_algebric liste position_de_depart dernier_coup_initial droit_au_roque_initial =
  let plateau = Array.copy position_de_depart in
  let trait_aux_blancs = ref true in
  let dernier_coup = ref dernier_coup_initial in
  let droit_au_roque = ref droit_au_roque_initial in
  let liste_coups = ref (List.rev liste) in
  let mot = ref "" in
  let ligne = ref "" in
  let algebric = ref "" in
  let compteur = ref 1 in
  while !liste_coups <> [] do
    let coups_valides_joueur = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
    let coup = List.hd !liste_coups in
    liste_coups := List.tl !liste_coups;
    if coup <> Aucun then begin
      if !trait_aux_blancs then begin
        mot := (string_of_int !compteur) ^ ". " ^ algebric_of_mouvement coup plateau coups_valides_joueur;
      incr compteur;
      end
      else begin
        if !mot = "" then mot := (string_of_int (!compteur - 1)) ^ "...";
        mot := !mot ^ " " ^ algebric_of_mouvement coup plateau coups_valides_joueur
      end;
      joue_coup_1 plateau coup trait_aux_blancs dernier_coup droit_au_roque;
      if menacee plateau (index_tableau plateau (roi !trait_aux_blancs)) !trait_aux_blancs then begin
        mot := !mot ^ "+"
      end;
      if !liste_coups <> [] then begin
        if !trait_aux_blancs then begin
          if (String.length (!ligne ^ !mot) > 80) then begin
            algebric := !algebric ^ "\n" ^ !ligne;
            ligne := !mot ^ " "
          end
          else begin
            ligne := !ligne ^ !mot ^ " "
          end;
        end
      end
      else begin
        if (gagne plateau !trait_aux_blancs coup) = defaite !trait_aux_blancs then begin
          let resultat = if !trait_aux_blancs then "0-1" else "1-0" in
          mot := String.sub !mot 0 (String.length !mot - 1) ^ "# " ^ resultat
        end
        else begin
          mot := !mot ^ " 1/2-1/2"
        end;
        if (String.length (!ligne ^ !mot) > 80) then begin
          algebric := !algebric ^ "\n" ^ !ligne ^ "\n" ^ !mot
        end
        else begin
          algebric := !algebric ^ "\n" ^ !ligne ^ !mot
        end
      end
    end
    else begin
      if !trait_aux_blancs then begin
        incr compteur
      end;
      trait_aux_blancs := not !trait_aux_blancs
    end
  done;
  !algebric

(*Fonction traduisant un coup en sa notation UCI*)
let uci_of_mouvement coup = match coup with
  |Roque {sorte} ->
    let depart_roi, arrivee_pr, arrivee_gr = if sorte < 3 then !depart_roi_blanc, 62, 58 else !depart_roi_noir, 6, 2 in
    let arrivee_roque = if sorte mod 2 = 1 then arrivee_pr else arrivee_gr in
    coord.(depart_roi) ^ coord.(arrivee_roque)
  |Promotion {depart = _; arrivee = _; prise = _; promotion} -> coord.(depart coup) ^ coord.(arrivee coup) ^ (String.lowercase_ascii tab_english.(abs promotion))
  |Aucun -> "0000"
  |_ -> coord.(depart coup) ^ coord.(arrivee coup)