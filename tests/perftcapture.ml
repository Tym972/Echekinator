open Libs.Board
open Libs.Generator
open Libs.Fen
open Config

let rec update_compteur liste_coup compteur_captures compteur_en_passant compteur_roques compteur_promotions = match liste_coup with
|[] -> ()
  |Normal {piece = _; from = _; to_ = _; capture} :: t when capture <> 0 -> incr compteur_captures; update_compteur t compteur_captures compteur_en_passant compteur_roques compteur_promotions
  |Castling _ :: t -> incr compteur_roques; update_compteur t compteur_captures compteur_en_passant compteur_roques compteur_promotions
  |Promotion {from = _; to_ = _; promotion = _; capture} :: t ->
    incr compteur_promotions;
    if capture <> 0 then incr compteur_captures;
    update_compteur t compteur_captures compteur_en_passant compteur_roques compteur_promotions
  |Enpassant _ :: t -> incr compteur_captures; incr compteur_en_passant; update_compteur t compteur_captures compteur_en_passant compteur_roques compteur_promotions
  |_ :: t -> update_compteur t compteur_captures compteur_en_passant compteur_roques compteur_promotions

let coups_valides_perft plateau trait_aux_blancs dernier_coup (prb, grb, prn, grn) position_roi roi_en_echec =
  let liste_coups = ref [] in
  let aux coup position_roi =
    make plateau coup;
    if not (threatened plateau position_roi trait_aux_blancs) then begin
      liste_coups := coup :: !liste_coups
    end;
    unmake plateau coup
  in List.iter (fun prise_en_passant -> aux prise_en_passant position_roi) (enpassant plateau trait_aux_blancs dernier_coup);
  if roi_en_echec then begin
    let coups, coups_roi = pseudo_legal_moves plateau trait_aux_blancs position_roi in
    List.iter (fun coup_roi -> aux coup_roi (to_ coup_roi)) coups_roi;
    List.iter (fun coup_autre -> aux coup_autre position_roi) coups;
    !liste_coups
  end
  else begin
    let droit_au_roque = if trait_aux_blancs then prb || grb else prn || grn in
    let piece_clouees = clouees plateau position_roi trait_aux_blancs in
    if piece_clouees = [] then begin
      let coups, coups_roi = pseudo_legal_moves plateau trait_aux_blancs position_roi in
      List.iter (fun coup_roi -> aux coup_roi (to_ coup_roi)) coups_roi;
      if droit_au_roque then (castlings plateau trait_aux_blancs (prb, grb, prn, grn)) @ !liste_coups @ coups else !liste_coups @ coups
    end
    else begin
      let coups, coups_roi, coups_clouees = pin_moves plateau trait_aux_blancs position_roi piece_clouees in
      List.iter (fun coup_roi -> aux coup_roi (to_ coup_roi)) coups_roi;
      List.iter (fun coup_clouees -> aux coup_clouees position_roi) coups_clouees;
      if droit_au_roque then (castlings plateau trait_aux_blancs (prb, grb, prn, grn)) @ !liste_coups @ coups else !liste_coups @ coups
    end
  end

let compteur_captures = ref 0
let compteur_en_passant = ref 0
let compteur_roques = ref 0
let compteur_promotions = ref 0
let compteur_echec = ref 0

let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur =
  let position_roi = index_array plateau (king trait_aux_blancs) in
  let roi_en_echec = threatened plateau position_roi trait_aux_blancs in
  if profondeur = 0 then begin
    if roi_en_echec then incr compteur_echec; 1
  end
  else begin
    let cp = ref (coups_valides_perft plateau trait_aux_blancs dernier_coup droit_au_roque position_roi roi_en_echec) in
    if profondeur = 1 then begin
      update_compteur !cp compteur_captures compteur_en_passant compteur_roques compteur_promotions
    end;
    let nodes = ref 0 in
    while !cp <> [] do
      let coup = List.hd !cp in
      make plateau coup;
      cp := List.tl !cp;
      nodes := !nodes + (algoperft plateau (not trait_aux_blancs) coup (modification_roque coup droit_au_roque) (profondeur - 1));
      unmake plateau coup
    done;
    !nodes 
  end

let algoperftime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur =
  let t = Sys.time () in
  let fx = algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur in
  fx, (Sys.time () -. t)

let perftcapture profondeur plateau =
  let nodes, time = algoperftime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur in
  print_newline ();
  print_board plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int profondeur));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Captures : " ^ string_of_int !compteur_captures);
  print_endline ("Prise en passant : " ^ string_of_int !compteur_en_passant);
  print_endline ("Roques : " ^ string_of_int !compteur_roques);
  print_endline ("Promotion : " ^ string_of_int !compteur_promotions);
  print_endline ("Echecs : " ^ string_of_int !compteur_echec);
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))

let () = perftcapture profondeur_perft plateau