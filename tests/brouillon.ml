open Libs.Plateau
open Config
open Libs.Interfaces
open Libs.Config

let main plateau =
  let b = ref true in
  for i = 0 to 959 do
    fischer i plateau releve_plateau;
    if not (est_960 chaine_fen) then begin
      b := false;
      print_endline "Ce n'est pas une position 960";
      affiche plateau
    end
  done;
  print_endline (if !b then  "Fonction correcte" else "Les problèmes")

let () = main plateau


  (*  affiche position_de_depart;
  print_endline (coord.(!depart_roi_blanc) ^ " " ^ coord.(!depart_tour_blanche_pr) ^ " " ^ coord.(!depart_tour_blanche_gr));
  print_endline (coord.(!depart_roi_noir) ^ " " ^ coord.(!depart_tour_noire_pr) ^ " " ^ coord.(!depart_tour_noire_gr));
  print_endline (string_of_bool !roi_blanc_clouable ^ " " ^ string_of_bool !roi_noir_clouable);
  print_endline (coord.(!clouage_roi_blanc_1) ^ " " ^ coord.(!clouage_roi_blanc_2) ^ " " ^ coord.(!clouage_roi_noir_1) ^ " " ^ coord.(!clouage_roi_noir_2));
  print_endline ((string_of_int !longueur_chemin_roi_blanc_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_blanc_gr));
  print_endline ((string_of_int !longueur_chemin_roi_noir_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_noir_gr));
  print_endline (string_of_bool !tour_blanche_gr_en_a ^ " " ^ string_of_bool !tour_blanche_gr_en_b ^ " " ^ string_of_bool !tour_blanche_pr_en_h);
  print_endline (string_of_bool !tour_noire_gr_en_a ^ " " ^ string_of_bool !tour_noire_gr_en_b ^ " " ^ string_of_bool !tour_noire_pr_en_h);
  print_newline ();
  List.iter (fun tab -> Array.iter (fun c -> if c <> 0 then print_string (coord.(c) ^ " ")) tab; print_newline ()) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr]; print_newline ();
  List.iter (fun list -> List.iter (fun c -> print_string (coord.(c) ^ " ")) list; print_newline ()) [!vides_blanc_pr; !vides_blanc_gr; !vides_noir_pr; !vides_noir_gr]
*)

(*


let deplacements_all2 plateau trait_aux_blancs position_roi piece_clouees =
  let liste_coups = ref [] in
  let liste_coups_roi = deplacements_roi2 plateau position_roi in
  let liste_coups_clouees = ref [] in
  if trait_aux_blancs then begin
    let aux depart arrivee = 
    for i = depart downto arrivee do
      if not (List.mem i piece_clouees) then begin
        let piece = plateau.(i) in
        if piece > 0 then begin
          (tabfun.(piece - 1) plateau i liste_coups)
        end
      end
      else begin
        let piece = plateau.(i) in (tabfun.(piece - 1) plateau i liste_coups_clouees)
      end
    done in List.iter (fun (a, b) -> aux a b) [63, position_roi + 1; position_roi - 1, 0]
  end
  else begin
    let aux depart arrivee =
      for i = depart to arrivee do
        if not (List.mem i piece_clouees) then begin
          let piece = plateau.(i) in
          if piece < 0 then begin
            (tabfun.(- piece - 1) plateau i liste_coups)
          end
        end
        else begin
          let piece = plateau.(i) in (tabfun.(- piece - 1) plateau i liste_coups_clouees)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, position_roi - 1; position_roi + 1, 63]
  end;
  !liste_coups, liste_coups_roi, !liste_coups_clouees


(*let menacee plateau case trait_aux_blancs =
  let b = ref false in
  let m = tab64.(case) in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  let vect_pion = [|(-9) * signe_joueur; (-11) * signe_joueur|] in
  let i = ref 0 in
  while (not !b && !i < 2) do
    let dir = vect_pion.(!i) in
    if tab120.(m + dir) <> (-1) then begin
      let candidat = tab120.(m + dir) in
      if plateau.(candidat) = (- signe_joueur) then begin
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
        if plateau.(candidat) = (-2) * signe_joueur then begin
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
        let dest = plateau.(candidat) * signe_joueur in
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
        let dest = plateau.(candidat) * signe_joueur in
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
  end;
  !b*)


let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur racine zobrist_position =
  if profondeur = 0 then begin
    1
  end
  else begin
    let nombre = try ZobristHashtbl.find table_perft zobrist_position with _ -> (-1) in
    if nombre <> (-1) then begin incr a;
      nombre
    end
    else begin
      let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque) in
      let nodes = ref 0 in
      while !cp <> [] do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque lxor profondeur in
        let perft = (algoperft plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (profondeur - 1) false nouveau_zobrist) in
        nodes := !nodes + perft;
        if racine then begin
          print_endline (uci_of_mouvement coup ^ ": " ^ string_of_int perft)
        end;
        dejoue plateau coup
      done;
      ZobristHashtbl.add table_perft zobrist_position !nodes;
      !nodes;
    end
  end

let algoperftime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur =
  let t = Sys.time () in
  let fx = algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur true (zobrist plateau trait_aux_blancs dernier_coup droit_au_roque lxor profondeur) in
  fx, (Sys.time () -. t)

let perft profondeur plateau =
  let nodes, time = algoperftime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur in
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int profondeur));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft profondeur_perft plateau; print_endline (string_of_int !a)


  let roi_blanc_roque = position_de_depart.(!depart_roi_blanc) = 6 in
  let roi_noir_roque = position_de_depart.(!depart_roi_noir) = (-6) in
  let tour_blanc_petit_roque = position_de_depart.(!depart_tour_blanche_pr) = 4 in
  let tour_blanc_grand_roque = position_de_depart.(!depart_tour_blanche_gr) = 4 in
  let tour_noir_petit_roque = position_de_depart.(!depart_tour_noire_pr) = (-4) in
  let tour_noir_grand_roque = position_de_depart.(!depart_tour_noire_gr) = (-4) in
  droit_au_roque :=
  (String.contains roques 'K') && roi_blanc_roque && tour_blanc_petit_roque,
  (String.contains roques 'Q') && roi_blanc_roque && tour_blanc_grand_roque,
  (String.contains roques 'k') && roi_noir_roque && tour_noir_petit_roque,
  (String.contains roques 'q') && roi_noir_roque && tour_noir_grand_roque*)