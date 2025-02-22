open Plateau
open Generateur
open Strategie1
open Quiescence
open Transposition
open Zobrist
(*open Traduction2
open Traduction3*)

let rec negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  let presence = ref true in
  let hash_node_type, hash_depth, hash_value, hash_move, _ = try ZobristHashtbl.find table zobrist_position with _ -> begin
    presence := false;
    (Pv, (-1), 0, Aucun, 0)
  end
  in if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let continuation = ref true in
    if !presence then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0;
      end
      else begin
        let b = ref true in
        (*if hash_move <> Aucun (*&& hash_node_type <> All*) then begin
          joue plateau hash_move;
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = zobrist plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau
          in let score =
            let note, _ = negalphabeta_total plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
            in - note
          in if score > !best_score then begin
            best_score := score;
            best_move := hash_move;
            alpha0 := max !alpha0 score;
            if score >= !beta0 then begin
              b := false
            end
          end;
          dejoue plateau hash_move;
        end;*)
        if !b then begin
          let cp =
            (*if (hash_node_type = All) then begin
              if hash_move = Aucun then begin
                ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)
              end
              else begin
                ref (List.filter (fun c -> c <> hash_move) (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque))
              end
            end
            else begin
              if hash_move = Aucun then begin*)
                ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
              (*end
              else begin
                ref (List.filter (fun c -> c <> hash_move) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta_quiescent))
              end
            end*)
          (*in let copie_cp = !cp in if hash_move <> Aucun then begin
            let j = index_liste hash_move !cp in tab_hash_1.(j) <- tab_hash_1.(j) + 1
          end;*)
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
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau
              in let score =
                let note, _ = negalphabeta_total plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
                in - note 
              in if score > !best_score then begin
                best_score := score;
                best_move := coup;
                alpha0 := max !alpha0 score;
                if score >= !beta0 then begin
                  b := false
                end
              end;
              dejoue plateau coup
            done
          end; 
          (*if !best_move <> Aucun then begin
            if !best_move = hash_move then tab_hash_best.(0) <- tab_hash_best.(0) + 1 else tab_hash_best.(1) <- tab_hash_best.(1) + 1;
            let j = index_liste !best_move copie_cp in tab_hash_2.(j) <- tab_hash_2.(j) + 1
          end;*)
        end;
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
  (*if hash_node_type = Pv && !presence && hash_value <> !best_score && node_type = Pv && profondeur = hash_depth then begin
    print_endline (Printf.sprintf "hash_value : %i best_score : %i" hash_value !best_score);
    print_endline (Printf.sprintf "hash_depth : %i et profondeur : %i"  profondeur hash_depth);
    print_endline (Printf.sprintf "hash_best : %s et bestmove : %s" (algebric_of_mouvement hash_move plateau []) (algebric_of_mouvement !best_move plateau []));
    print_endline (Printf.sprintf " alpha : %i et beta : %i" alpha beta);
    print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] releve_plateau);
    affiche plateau
  end;*)
  if !presence then begin
    if profondeur > hash_depth then begin
      ZobristHashtbl.replace table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
    end
  end
  else begin
    ZobristHashtbl.add table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
  end;
  !best_score, !best_move

  let negalphabetime_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
    let t = Sys.time () in
    let fx = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-99999) 99999 evaluation (List.hd releve_plateau) in
    fx, (Sys.time () -. t)

let iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let score = ref 0 in
  let move = ref Aucun in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
  done;
  !score, !move

let iid_time_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)