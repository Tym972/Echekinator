open Libs.Plateau
open Libs.Generateur
open Libs.Strategie1
open Libs.Zobrist
open Libs.Quiescence
open Libs.Transposition
open Config


let (table_pv : (noeuds * int * int * mouvement list * int) ZobristHashtbl.t) =  ZobristHashtbl.create taille_transposition

let traitement_hash_pv (hash_node_type : noeuds) (hash_depth : int) (hash_value : int) (hash_moves : mouvement list) depth alpha beta best_score best_move continuation = incr compteur_trans;
  if depth <= hash_depth then begin
    match hash_node_type with
      |Pv ->
        best_score := hash_value;
        best_move := hash_moves;
        continuation := false
      |Cut ->
        alpha := max !alpha hash_value;
        if hash_value >= !beta then begin
          best_score := 99999;
          continuation := false
        end
      |All ->
        beta := min !beta hash_value;
        if !alpha >= hash_value then begin
          best_score := (-99999);
          continuation := false
        end 
  end

let rec negalphabeta_total_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_moves = ref [] in
  let presence = ref true in
  let hash_node_type, hash_depth, hash_value, hash_moves, _ = try ZobristHashtbl.find table_pv zobrist_position with _ -> begin
    presence := false;
    (All, (-1), 0, [], 0)
  end
  in let hash_move = try List.hd hash_moves with _ -> Aucun
  in if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let continuation = ref true in
    if !presence then begin
      traitement_hash_pv hash_node_type hash_depth hash_value hash_moves profondeur alpha0 beta0 best_score best_moves continuation
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0;
      end
      else begin
        let b = ref true in
        if (hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun then begin
          joue plateau hash_move;
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = zobrist plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau in
          let note, pv = negalphabeta_total_pv plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
          in let score = - note in
          if score > !best_score then begin
            best_score := score;
            best_moves := hash_move :: pv;
            alpha0 := max !alpha0 score;
            if score >= !beta0 then begin
              b := false
            end
          end;
          dejoue plateau hash_move;
        end;
        if !b then begin
          let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
          in if !cp = [] then begin incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := (profondeur_initiale - (profondeur + 99999))
            end 
            else begin
              best_score := 0
            end
          end
          else begin
            while (!b && !cp <> []) do
              let coup = List.hd !cp in
              joue plateau coup;
              cp := List.tl !cp;
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque in
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau in
              let note, pv = negalphabeta_total_pv plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
              in let score = - note in
              if score > !best_score then begin
                best_score := score;
                best_moves := coup :: pv;
                alpha0 := max !alpha0 score;
                if score >= !beta0 then begin
                  b := false
                end
              end;
              dejoue plateau coup
            done
          end
        end
      end
    end
  end;
  let node_type =
    if !best_score <= alpha then begin
      All
    end
    else if !best_score >= beta then begin
      Cut
    end
    else begin
      Pv
    end in
  if !presence then begin
    if profondeur > hash_depth then begin
      ZobristHashtbl.replace table_pv zobrist_position (node_type, profondeur, !best_score, !best_moves, 0)
    end
  end
  else begin
    ZobristHashtbl.add table_pv zobrist_position (node_type, profondeur, !best_score, !best_moves, 0)
  end;
  !best_score, !best_moves

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax*)
let rec negalphabeta_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation =
  let best_score = ref (-99999) in
  let best_moves = ref [] in
  if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
    in if !cp = [] then begin
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := (profondeur_initiale - (profondeur + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let note, pv = negalphabeta_pv plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
        in let score = - note in
        if score > !best_score then begin
          best_score := score;
          best_moves := coup :: pv;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_moves

let negalphabetime_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let negalphabetime_total_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_total_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale (-99999) 99999 evaluation (List.hd releve_plateau) in
  fx, (Sys.time () -. t)

let runnegalphabeta_pv b1 b2 plateau =
  if b1 then begin
    affiche plateau
  end;
  let (a,b),c = negalphabetime_pv plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_float ((float_of_int a)/. 1000.)));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  affiche_liste b plateau (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque);
  print_newline ()

let runnegalphabeta_total_pv b1 b2 plateau =
  if b1 then begin
    affiche plateau
  end;
  let (a,b),c = negalphabetime_total_pv plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_float ((float_of_int a)/. 1000.)));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  affiche_liste b plateau (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque);
  print_newline ()

let main b1 b2 plateau =
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau;
  if b1 then begin
    print_endline "Negalphabeta";
    runnegalphabeta_pv false true (Array.copy plateau);
  end;
  if b2 then begin
    print_endline "Negalphabeta Total";
    runnegalphabeta_total_pv false true (Array.copy plateau);
  end

let () = main false true plateau

(*let bulle liste elt = 
  let rec supprime liste elt = match liste with
    |[] -> []
    |h::t -> if h = elt then t else h :: supprime t elt
  in elt :: supprime liste elt*)