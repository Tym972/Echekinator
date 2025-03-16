(*Module implémentant la table de tansposition*)
open Plateau
open Generateur
open Strategie1
open Zobrist
let taille_transposition = 10000000

module ZobristHash =
  struct
  type t = int
  let equal i j = i = j
  let hash i = i mod taille_transposition
  end

module ZobristHashtbl = Hashtbl.Make(ZobristHash)

type noeuds =
  |Pv
  |Cut
  |All

let (table : (noeuds * int * int * mouvement * int) ZobristHashtbl.t) =  ZobristHashtbl.create taille_transposition

let compteur_trans = ref 0

let adapte_releve2 zobrist_position coup profondeur releve_plateau =
  if est_irremediable coup then begin
    if profondeur < 8 then begin
      []
    end
    else begin
      [zobrist_position]
    end
  end
  else if List.length releve_plateau + profondeur < 8 then begin
    []
  end
  else begin 
    zobrist_position :: releve_plateau
  end

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let traitement_hash (hash_node_type : noeuds) (hash_depth : int) (hash_value : int) (hash_move : mouvement) depth alpha beta best_score best_move continuation ply =
  incr compteur_trans;
  if depth <= hash_depth then begin
    let score = ref hash_value in
    if abs hash_value > 99000 then begin
      if !score >= 0 then score := !score - ply else score := !score + ply
    end;
    match hash_node_type with
      |Pv ->
        best_score := !score;
        best_move := hash_move;
        continuation := false
      |Cut ->
        alpha := max !alpha !score;
        if !score >= !beta then begin
          best_score := !score;
          continuation := false
        end
      |All ->
        beta := min !beta !score;
        if !alpha >= !score then begin
          best_score := !score;
          continuation := false
        end 
  end

(*Fonction retirant les entrées de la hash table datant d'avant le n-ième coup.*)
let actualise table n =
  ZobristHashtbl.iter (fun key value -> let _, _, _, _, coup = value in if coup < n then ZobristHashtbl.remove table key ) table

let index_liste element liste = 
  let rec aux liste n = match liste with
  |[] -> (-1)
  |h::t -> if h = element then n else aux t (n + 1)
  in aux liste 0

let tab_hash_1 = Array.make 100 0
let tab_hash_2 = Array.make 100 0
let tab_hash_best = Array.make 2 0

let a1 = ref 0
let a2 = ref 0

let rec negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position = incr compteur_recherche;
  if !stop_calculating || repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    0, Aucun
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in 
    let best_score = ref (-infinity) in
    let best_move = ref Aucun in
    let ply = profondeur_initiale - profondeur in
    let presence = ref true in
    let hash_node_type, hash_depth, hash_value, hash_move, _ = try ZobristHashtbl.find table zobrist_position with _ -> begin
      presence := false;
      (All, (-1), 0, Aucun, 0)
    end
    in let continuation = ref true in
    if !presence then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation ply
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
      end
      else begin
        let b = ref true in
        let hash_ordering =
          (*not (hash_node_type = All || hash_move = Aucun) && hash_value > beta*)
          ((hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun)
        in if hash_ordering then begin
          joue plateau hash_move;
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau
          in let score =
            let note, _ = negalphabeta_trans plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
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
        end;
        if !b then begin
          let cp =
            if hash_ordering then
              ref (List.filter (fun c -> c <> hash_move) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta))
            else
              ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
          in if !cp = [] && not hash_ordering then begin incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := ply - 99999
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
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau
              in let score =
                let note, _ = negalphabeta_trans plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
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
      end
    in let stored_value =
      if abs !best_score < 99000 then begin
        !best_score
      end
      else begin
        if !best_score >= 0 then begin
          !best_score + ply
        end
        else begin
          !best_score - ply
        end
      end
    in if !presence then begin
      if profondeur > hash_depth then begin
        ZobristHashtbl.replace table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
      end
    end
    else begin
      ZobristHashtbl.add table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
    end;
    !best_score, !best_move
  end

let negalphabetime_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation (List.hd releve_plateau) in
  fx, (Sys.time () -. t)

let iid_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur alpha beta evaluation =
  let score = ref 0 in
  let move = ref Aucun in
  for i = 1 to profondeur do
    let new_score, new_move = negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau i i alpha beta evaluation (List.hd releve_plateau) in
    move := new_move;
    score := new_score;
  done;
  !score, !move

let iid_time_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = iid_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)