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

(*Fonction traduisant une position FEN en l'int array correspondant. Par défaut si non rensigné, le trait est au blancs, il n'y a plus de roques, pas de prise en passant, aucun coup joué*)
let position_of_fen chaine position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau =
  let split_fen = ref (detecte_mots chaine) in
  let longueur_fen = List.length !split_fen in
  if longueur_fen > 0 then begin
    let position_pieces = (List.nth !split_fen 0) in
    if String.contains position_pieces 'K' && String.contains position_pieces 'k' then begin
      let plateau_of_fen position_pieces =
        let plateau = Array.make 64 0 in
        let split_ligne = Str.split (Str.regexp "/") position_pieces in
        for i = 0 to 7 do
          let ligne = List.nth split_ligne i in
          let j = ref 0 in
          let k = ref 0 in
          while !j < 8 do
            let elt = ligne.[!k] in
            let piece = try Hashtbl.find dicofen elt with _ ->
              let vides = (int_of_char elt) - 48 in
              j := !j + vides;
              0 in
            if piece <> 0 then begin
              plateau.(8 * i + !j) <- piece;
              incr j
            end;
            incr k
          done
        done;
        plateau
      in let plateau_fen = plateau_of_fen position_pieces in
      for i = 0 to 63 do 
        position_de_depart.(i) <- plateau_fen.(i)
      done;
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
      let roque_valide roques =
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
      in roque_valide (List.nth !split_fen 2);
      let deduction_ep case_ep trait_aux_blancs =
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
      in let poussee_pep = deduction_ep (List.nth !split_fen 3) !trait_aux_blancs in 
      if poussee_pep <> Aucun then dernier_coup := poussee_pep;
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
      in nombre_coup !trait_aux_blancs (List.nth !split_fen 5);
      if menacee position_de_depart (index position_de_depart (roi (not !trait_aux_blancs))) || coups_valides position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque = [] then begin
        for i = 0 to 63 do 
          position_de_depart.(i) <- echiquier.(i)
        done;
        dernier_coup := Aucun;
        droit_au_roque := (true, true, true, true);
        releve_coups := [];
        releve_plateau := [zobrist echiquier true Aucun (true, true, true, true)]
      end
    end
  end