(*Modules implémentant les traductions FEN*)

open Plateau
open Generateur
open Traduction1
open Zobrist

(*Tableau contenant la représentation algébrique des pièces*)
let tabfen_blanc = [|"P"; "N"; "B"; "R"; "Q"; "K"|]

let tabfen_noir = [|"p"; "n"; "b"; "r"; "q"; "k"|]

(*Tableau dont dont les élément sont le strind de l'indice*)
let tabstring =
  [|
  "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7";
  "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15";
  "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23";
  "24"; "25"; "26"; "27"; "28"; "29"; "30"; "31";
  "32"; "33"; "34"; "35"; "36"; "37"; "38"; "39";
  "40"; "41"; "42"; "43"; "44"; "45"; "46"; "47";
  "48"; "49"; "50"; "51"; "52"; "53"; "54"; "55";
  "56"; "57"; "58"; "59"; "60"; "61"; "62"; "63"
  |]

(*Tableau utilisé pour expliciter la notation des roques dans la notation FEN en cas d'ambiguïté*)
let tab_roques = [|"q"; "b"; "c"; "d"; "e"; "f"; "g"; "k"|]

(*Fonction utilisée pour expliciter la notation des roques dans la notation FEN en cas d'ambiguïté*)
let xfen_roque plateau =
  let representation_roques = [|"K"; "Q"; "k"; "q"|] in
  let aux plateau depart_tour_pr depart_tour_gr trait_aux_blancs i =
    let decrement = if trait_aux_blancs then 56 else 0 in
    let case = ref decrement in
    let case_droite = decrement + 8 in
    let tour = tour trait_aux_blancs in
    let indice_pr = depart_tour_pr - decrement in
    let indice_gr = depart_tour_gr - decrement in
    let maj = if trait_aux_blancs then String.uppercase_ascii else fun s -> s in
    while !case < case_droite do
      let piece = plateau.(!case) in
      if piece = tour then begin
        if !case < depart_tour_gr then begin
          representation_roques.(i + 1) <- maj (tab_roques.(indice_gr))
        end
        else if !case > depart_tour_pr then begin
          representation_roques.(i) <- maj (tab_roques.(indice_pr))
        end
      end;
      incr case
    done
  in List.iter (fun (depart_tour_pr, depart_tour_gr, trait_aux_blancs, i) -> aux plateau depart_tour_pr depart_tour_gr trait_aux_blancs i)
  [(!depart_tour_blanche_pr, !depart_tour_blanche_gr, true, 0); (!depart_tour_noire_pr, !depart_tour_noire_gr, false, 2)];
  representation_roques

(*Fonction représentant un plateau en sa notation FEN*)
let fen plateau trait_aux_blancs dernier_coup (prb, grb, prn, grn) releve_coups releve_plateau =
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
  if not (prb || grb || prn || grn) then begin
    fen := !fen ^ "-"
  end
  else begin
    let representation_roques = xfen_roque plateau in
    if prb then fen := !fen ^ representation_roques.(0);
    if grb then fen := !fen ^ representation_roques.(1);
    if prn then fen := !fen ^ representation_roques.(2);
    if grn then fen := !fen ^ representation_roques.(3)
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

(*Tableau utilisé pour expliciter la notation des roques dans la notation FEN en cas d'ambiguïté*)
let dicoroque_xfend =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
  [('q', 0); ('a', 0); ('b', 1); ('c', 2); ('d', 3); ('e', 4); ('f', 5); ('g', 6); ('k', 7); ('h', 7)];
  ht

(*Fonction vérifiant la validité des roques d'une position XFEN*)
let roque_valide position_de_depart roques droit_au_roque =
  let prb = ref false in
  let grb = ref false in
  let prn = ref false in
  let grn = ref false in
  let nombre_roques_blancs = ref 0 in
  let nombre_roques_noirs = ref 0 in
  let n = if roques = "-" then 0 else String.length roques in
  for i = 0 to (n - 1) do 
    let roque = roques.[i] in
    if roque = Char.uppercase_ascii roque then begin
      incr nombre_roques_blancs
    end
    else begin
      incr nombre_roques_noirs
    end
  done;
  let position_tours_blanches = ref [] in
  let position_tours_noires = ref [] in
  let position_roi_blanc = ref (-1) in
  let position_roi_noir = ref (-1) in
  let aux_plateau plateau trait_aux_blancs position_tours position_roi =
    let increment = if trait_aux_blancs then 56 else 0 in
    let case = ref increment in
    let case_droite = increment + 8 in
    let tour = tour trait_aux_blancs in
    let roi = roi trait_aux_blancs in
    while !case < case_droite do
      let piece = plateau.(!case) in
      if piece = tour then begin
        position_tours := !case :: !position_tours
      end
      else if piece = roi then begin
        position_roi := !case
      end;
      incr case
    done
  in List.iter
  (fun (trait_aux_blancs, positions_tour, position_roi) -> aux_plateau position_de_depart trait_aux_blancs positions_tour position_roi)
  [(true, position_tours_blanches, position_roi_blanc); (false, position_tours_noires, position_roi_noir)];
  let indice = ref 0 in
  let aux_string position_roi nombres_roques trait_aux_blancs indice =
  let increment = if trait_aux_blancs then 56 else 0 in
    if nombres_roques = 2 then begin
      let result = (try Hashtbl.find dicoroque_xfend (Char.lowercase_ascii roques.[!indice]) + increment with _ -> (-2)), (try Hashtbl.find dicoroque_xfend (Char.lowercase_ascii roques.[!indice + 1]) + increment with _ -> (-2))
      in indice := !indice + 2;
      result
    end
    else if nombres_roques = 1 then begin
      let depart_tour = (try Hashtbl.find dicoroque_xfend (Char.lowercase_ascii roques.[!indice]) + increment with _ -> (-2)) in
      incr indice;
      if depart_tour > position_roi then begin
        depart_tour, (-2)
      end
      else begin
        (-2), depart_tour
      end
    end
    else begin
      (-2), (-2)
    end
  in let petit_roque_blanc, grand_roque_blanc = aux_string !position_roi_blanc !nombre_roques_blancs true indice
  in let petit_roque_noir, grand_roque_noir = aux_string !position_roi_noir !nombre_roques_noirs false indice in
  if !nombre_roques_blancs <> 0 && !position_roi_blanc <> (-1) then begin
    if (grand_roque_blanc <> (-2) && List.exists (fun position_tour -> !position_roi_blanc > position_tour) !position_tours_blanches) then begin
      grb := true
    end;
    if (petit_roque_blanc <> (-2) && List.exists (fun position_tour -> !position_roi_blanc < position_tour) !position_tours_blanches) then begin
      prb := true
    end
  end;
  if !nombre_roques_noirs <> 0 && !position_roi_noir <> (-1) then begin
    if (grand_roque_noir <> (-2) && List.exists (fun position_tour -> !position_roi_noir > position_tour) !position_tours_noires) then begin
      grn := true
    end;
    if (petit_roque_noir <> (-2) && List.exists (fun position_tour -> !position_roi_noir < position_tour) !position_tours_noires) then begin
      prn := true
    end
  end;
  droit_au_roque := !prb, !grb, !prn, !grn;
  if !prb || !prn || !grb || !grn then begin
    let plateau_provisoire = Array.make 64 0
    in let depart_gr_blanc = ref (if grand_roque_blanc = (-2) then (-2) else if grand_roque_blanc = 56 then (List.hd (List.rev !position_tours_blanches)) else grand_roque_blanc) in
    let depart_pr_blanc = ref (if petit_roque_blanc = (-2) then (-1) else if petit_roque_blanc = 63 then (List.hd !position_tours_blanches) else petit_roque_blanc) in
    let depart_gr_noir = ref (if grand_roque_noir = (-2) then (-2) else if grand_roque_noir = 0 then (List.hd (List.rev !position_tours_noires)) else grand_roque_noir) in
    let depart_pr_noir = ref (if petit_roque_noir = (-2) then (-1) else if petit_roque_noir = 7 then (List.hd !position_tours_noires) else petit_roque_noir) in
    let liste_occupee_blanche = List.filter (fun case -> case > (-1)) [!depart_gr_blanc; !depart_pr_blanc; !position_roi_blanc] in
    let liste_occupee_noire = List.filter (fun case -> case > (-1)) [!depart_gr_noir; !depart_pr_noir; !position_roi_noir] in
    let aux_depart depart trait_aux_blancs liste_occupee =
      if !depart < 0 then begin
        let candidate, sens = if !depart = (-1) then 7, incr else 0, decr in
        let increment = if trait_aux_blancs then 56 else 0 in
        let b = ref true in
        let case = ref (increment + candidate) in
        while !b do
          if List.mem !case liste_occupee then
            sens case
          else begin
            depart := !case;
            b := false
          end
        done
      end
    in List.iter (fun (depart, trait_aux_blancs, liste_occupee) -> aux_depart depart trait_aux_blancs liste_occupee)
    [(depart_gr_blanc, true, liste_occupee_blanche); (depart_pr_blanc, true, liste_occupee_blanche); (position_roi_blanc, true, liste_occupee_blanche);
    (depart_gr_noir, false, liste_occupee_noire); (depart_pr_noir, false, liste_occupee_noire); (position_roi_noir, false, liste_occupee_noire)];
    List.iter (fun (position, piece) -> plateau_provisoire.(position) <- piece)
    [(!position_roi_blanc, 6); (!position_roi_noir, (-6)); (!depart_gr_blanc, 4); (!depart_pr_blanc, 4); (!depart_gr_noir, (-4)); (!depart_pr_noir, (-4))];
    actualisation_roque plateau_provisoire
  end

(*Fonction permettant de réinitialiser un plateau à l'état d'origine*)
let reinitialise plateau dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial = 
  for i = 0 to 63 do
    plateau.(i) <- position_de_depart.(i)
  done;
  actualisation_roque position_de_depart;
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
          actualisation_roque position_de_depart;
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
          if (menacee position_de_depart (index_tableau position_de_depart (roi (not !trait_aux_blancs))) (not !trait_aux_blancs) || coups_valides position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque = []) then begin
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
        if (not !fen_correct) || menacee position_de_depart (index_tableau position_de_depart (roi (not !trait_aux_blancs))) (not !trait_aux_blancs) || coups_valides position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque = [] then begin
          reinitialise position_de_depart dernier_coup droit_au_roque releve_coups releve_plateau echiquier Aucun (true, true, true, true) [] [zobrist echiquier true Aucun (true, true, true, true)]
        end
      end
    end
  end