(*Modules implémentant les traductions FEN*)

open Plateau
open Generateur
open Traduction1
open Zobrist

let tabfen_blanc = [|"P"; "N"; "B"; "R"; "Q"; "K"|]

let tabfen_noir = [|"p"; "n"; "b"; "r"; "q"; "k"|]

let tabstring =
  [|
  "0"; "1";"2";"3";"4";"5";"6";"7";
  "8"; "9";"10";"11";"12";"13";"14";"15";
  "16";"17";"18";"19";"20";"21";"22";"23";
  "24";"25";"26";"27";"28";"29";"30";"31";
  "32";"33";"34";"35";"36";"37";"38";"39";
  "40";"41";"42";"43";"44";"45";"46";"47";
  "48";"49";"50";"51";"52";"53";"54";"55";
  "56";"57";"58";"59";"60";"61";"62";"63"
  |]

(*Fonction représentant un plateau en sa notation FEN*)
let fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  let fen = ref "" in
  let vides = ref 0 in
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case = 0 then begin
      vides := !vides + 1
    end
    else begin
      if !vides > 0 then begin
        fen := !fen ^ (string_of_int !vides) ^ (if case > 0 then tabfen_blanc.(case - 1) else tabfen_noir.(- case - 1));
        vides := 0
      end
      else begin
        fen := !fen ^ (if case > 0 then tabfen_blanc.(case - 1) else tabfen_noir.(- case - 1))
      end
    end;
    if (i + 1) mod 8 = 0 then begin
      if !vides <> 0 then begin
        fen := !fen ^ (string_of_int !vides);
        vides := 0
      end;
      if i <> 63 then begin
        fen := !fen ^ "/"
      end
    end
  done;
  if !vides > 0 then begin
    fen := !fen ^ (string_of_int !vides)
  end;
  if trait_aux_blancs then
    fen := !fen ^ " w "
  else begin
    fen := !fen ^ " b "
  end;
  let prb, grb, prn, grn = droit_au_roque in
  if not (prb || grb || prn || grn) then begin
    fen := !fen ^ "-"
  end
  else begin
    if prb then fen := !fen ^ "K";
    if grb then fen := !fen ^ "Q";
    if prn then fen := !fen ^ "k";
    if grn then fen := !fen ^ "q"
  end;
  let pep = enpassant plateau trait_aux_blancs dernier_coup in
  fen := !fen ^ " " ^ (try coord.(arrivee (List.hd pep)) ^ " " with _ -> "- ");
  fen := !fen ^ (string_of_int (max 0 (List.length releve_plateau - 1)) ^ " ");
  fen := !fen ^ string_of_int (1 + (List.length releve_coups)/ 2);
  !fen

(*Dictionnaire associant la repsésentation des pièces dans les tableau-échiquier à une chaîne de caractères*)
let dicofen =
  let ht = Hashtbl.create 13 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [ ('p', (-1)); ('n', (-2)); ('b', (-3)); ('r', (-4)); ('q', (-5)); ('k', (-6));
    ('P', 1); ('N', 2); ('B', 3); ('R', 4); ('Q', 5); ('K', 6)];
  ht

(*Fonction actualisant le plateau en fonction de la partie "pièces" du FEN*)
let plateau_of_fen plateau liste_lignes fen_correct =
  for i = 0 to 63 do
    plateau.(i) <- 0
  done;
  for i = 0 to 7 do
    let ligne = List.nth liste_lignes i in
    let j = ref 0 in
    let k = ref 0 in
    while !j < 8 do
      let elt = ligne.[!k] in
      let piece = try Hashtbl.find dicofen elt with _ ->
        let vides = (int_of_char elt) - 48 in if vides > 0 && vides < 10 then begin
          j := !j + vides;
          0
        end
        else begin
          fen_correct := false;
          0
        end
      in if piece <> 0 then begin
        plateau.(8 * i + !j) <- piece;
        incr j
      end;
      incr k
    done
  done

(*Fonction permettant de déduire le dernier coup en fonction d'une prise en passant possible*)
let deduction_ep position_de_depart case_ep trait_aux_blancs =
  let case = try Hashtbl.find dicocoord case_ep with _ -> (-1) in
  if case <> (-1) then begin
    if trait_aux_blancs && position_de_depart.(case - 8) = 0 && position_de_depart.(case) = 0 && position_de_depart.(case + 8) = (-1) then begin
      Classique {piece = (-1); depart = case - 8; arrivee = case + 8; prise = 0}
    end
    else if position_de_depart.(case + 8) = 0 && position_de_depart.(case) = 0 && position_de_depart.(case - 8) = 1 then begin
      Classique {piece = 1; depart = case + 8; arrivee = case - 8; prise = 0}
    end
    else begin
      Aucun
    end
  end
  else begin
    Aucun
  end

(*Fonction vérifiant la validité des roques d'une position FEN*)
let roque_valide position_de_depart roques droit_au_roque =
  let roi_blanc_roque = position_de_depart.(60) = 6 in
  let roi_noir_roque = position_de_depart.(4) = (-6) in
  let tour_blanc_petit_roque = position_de_depart.(63) = 4 in
  let tour_blanc_grand_roque = position_de_depart.(56) = 4 in
  let tour_noir_petit_roque = position_de_depart.(7) = (-4) in
  let tour_noir_grand_roque = position_de_depart.(0) = (-4) in
  droit_au_roque :=
  (String.contains roques 'K') && roi_blanc_roque && tour_blanc_petit_roque,
  (String.contains roques 'Q') && roi_blanc_roque && tour_blanc_grand_roque,
  (String.contains roques 'k') && roi_noir_roque && tour_noir_petit_roque,
  (String.contains roques 'q') && roi_noir_roque && tour_noir_grand_roque

(*Fonction permettant de réinitialiser un plateau à l'état d'origine*)
let reinitialise plateau dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial = 
  for i = 0 to 63 do
    plateau.(i) <- position_de_depart.(i)
  done;
  dernier_coup := dernier_coup_initial;
  droit_au_roque := droit_au_roque_initial;
  releve_coups := releve_coups_initial;
  releve_plateau := releve_plateau_initial

(*Fonction traduisant une position FEN en l'int array correspondant. Par défaut si non rensigné, le trait est au blancs, il n'y a plus de roques, pas de prise en passant, aucun coup joué*)
let position_of_fen chaine position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  let split_fen = ref (detecte_mots chaine) in
  let longueur_fen = List.length !split_fen in
  if longueur_fen > 0 then begin
    let position_pieces = (List.nth !split_fen 0) in
    let validite chaine =
      let rois = ref "" in
      let longueur = String.length chaine in
      for i = 0 to longueur - 1 do
        if String.contains "kK" chaine.[i] then rois := (String.make 1 chaine.[i]) ^ !rois
      done;
      String.length !rois = 2 && String.contains !rois 'K' && String.contains !rois 'k'
    in if validite position_pieces then begin
      let split_ligne = Str.split (Str.regexp "/") position_pieces in
      if List.length split_ligne = 8 && not (String.exists (fun c -> String.contains "pP" c) (List.hd split_ligne ^ (List.nth split_ligne 7))) then begin
        let fen_correct = ref true in
        begin
          try plateau_of_fen position_de_depart split_ligne fen_correct with _ -> fen_correct := false
        end;
        if !fen_correct then begin
          let complete longueur = 
            let rec aux acc longueur = match longueur with
              |5 -> aux ("1" :: acc) 6
              |4 -> aux ("0" :: acc) 5
              |3 | 2 -> aux  ("-" :: acc) (longueur + 1)
              |1 -> aux ("w" :: acc) 2
              |_ -> acc
            in List.rev (aux [] longueur)
          in split_fen := !split_fen @ (complete longueur_fen);
          trait_aux_blancs := not (List.nth !split_fen 1 = "b");
          if (menacee position_de_depart (index position_de_depart (roi (not !trait_aux_blancs))) || coups_valides position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque = []) then begin
            trait_aux_blancs := not !trait_aux_blancs
          end;
          roque_valide position_de_depart (List.nth !split_fen 2) droit_au_roque;
          let poussee_pep = deduction_ep position_de_depart (List.nth !split_fen 3) !trait_aux_blancs in 
          if poussee_pep <> Aucun then begin
            dernier_coup := poussee_pep
          end;
          releve_plateau := [];
          let rec ajoute liste_ref i =
            if i > 0 then begin
              liste_ref := i :: !releve_plateau;
              ajoute liste_ref (i - 1)
            end
          in ajoute releve_plateau (try int_of_string (List.nth !split_fen 4) with _ -> 0);
          releve_plateau := (zobrist position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque) :: !releve_plateau;
          let nombre_coup trait_aux_blancs coups_complets =
            if trait_aux_blancs then begin
              for _ = 1 to try (2 * (int_of_string (List.nth !split_fen 5) - 1)) with _ -> 0 do 
                releve_coups := Aucun :: !releve_coups
              done
            end
            else begin
              for _ = 1 to try (2 * (int_of_string coups_complets - 1) + 1) with _ -> 0 do 
                releve_coups := Aucun :: !releve_coups
              done
            end
          in nombre_coup !trait_aux_blancs (List.nth !split_fen 5)
        end;
        if (not !fen_correct)|| menacee position_de_depart (index position_de_depart (roi (not !trait_aux_blancs))) || coups_valides position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque = [] then begin
          reinitialise position_de_depart dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)]
        end
      end
    end
  end