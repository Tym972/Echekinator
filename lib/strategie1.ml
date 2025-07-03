(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Plateau
open Generateur
open Zobrist
open Evaluations
open Quiescence

(*Variable utilisée pour arrêter la recherche de force*)
let stop_calculating = ref false

(*Bigest integer*)
let infinity = (Int64.to_int (Random.int64 4611686018427387903L))

(*Max depth reached by the search*)
let max_depth = 255
let compteur_recherche = ref 0
let compteur_noeuds_terminaux = ref 0
let compteur_transposition = ref 0

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition liste_releve_plateau n = match liste_releve_plateau with
  |[] -> false
  |h::q ->
    let rec aux liste k = match liste with
      |[] | [_] -> false
      |_::j::t -> (h = j && (k + 1 = n || aux t (k + 1))) || aux t k
    in aux q 1

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

(*PV node : Exact value, Cut Node : Lower Bound, All Node : Upper Bound*)
let traitement_hash (hash_node_type : noeuds) (hash_depth : int) (hash_value : int) (hash_move : mouvement) depth alpha beta best_score best_move no_tt_cut ply =
  if depth <= hash_depth then begin
    let score = ref hash_value in
    if abs hash_value > 99000 then begin
      if !score >= 0 then score := !score - ply else score := !score + ply
    end;
    match hash_node_type with
      |Pv ->
        best_score := !score;
        best_move := hash_move;
        no_tt_cut := false
      |Cut ->
        alpha := max !alpha !score;
        if !score >= !beta then begin
          best_score := !score;
          no_tt_cut := false
        end
      |All ->
        beta := min !beta !score;
        if !alpha >= !score then begin
          best_score := !score;
          no_tt_cut := false
        end 
  end

(*Fonction retirant les entrées de la hash table datant d'avant le n-ième coup.*)
let actualise table n =
  ZobristHashtbl.iter (fun key value -> let _, _, _, _, coup = value in if coup < n then ZobristHashtbl.remove table key) table

let adapte_releve zobrist_position coup profondeur releve_plateau demi_coups =
  if est_irremediable coup then begin
    if profondeur < 8 then begin
      [], 0
    end
    else begin
      [zobrist_position], 0
    end
  end
  else if demi_coups + profondeur < 7 then begin
    [], demi_coups + 1
  end
  else begin 
    zobrist_position :: releve_plateau, demi_coups + 1
  end

(*Fonction triant une liste de coups selon la logique Most Valuable Victim - Least Valuable Agressor*)
let mvvlva coup = match coup with
  |Classique {piece; depart = _; arrivee = _; prise} when prise <> 0 ->
    10 * tabvalue.(abs prise) - tabvalue.(abs piece)
  |Enpassant {depart = _; arrivee = _} ->
    10 * tabvalue.(1) - tabvalue.(1)
  |Promotion {depart = _; arrivee = _; prise; promotion} ->
    10 * (tabvalue.(abs prise) + tabvalue.(abs promotion)) - tabvalue.(1)
  |_ -> 0

let killer_moves = Array.make (2 * max_depth) Aucun

let history_moves = [| Array.init 64 (fun _ -> Array.make 64 0); Array.init 64 (fun _ -> Array.make 64 0)|]

let aux_history trait_aux_blancs =
  if trait_aux_blancs then 0 else 1

let nouveau_tri plateau trait_aux_blancs dernier_coup droit_au_roque ply =
  let coups_valides = coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque in
  let score coup =
    if isquiet coup then begin
      if killer_moves.(2 * ply) = coup then begin
        90000000
      end
      else if killer_moves.(2 * ply + 1) = coup then begin
        80000000
      end
      else begin
        history_moves.(aux_history trait_aux_blancs).(depart coup).(arrivee coup)
      end
    end
    else begin
      100000000 + mvvlva coup
    end
  in List.map snd (tri_fusion (List.map (fun coup -> (score coup, coup)) coups_valides))

let rec pvs plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau demi_coups profondeur profondeur_initiale alpha beta evaluation zobrist_position ispv =
  incr compteur_recherche;
  if !stop_calculating || repetition releve_plateau 3 || demi_coups = 100 then begin
    incr compteur_noeuds_terminaux;
    0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let best_score = ref (-infinity) in
    let best_move = ref Aucun in
    let ply = profondeur_initiale - profondeur in
    let presence = ref true in
    let hash_node_type, hash_depth, hash_value, hash_move, _ =
      try
        ZobristHashtbl.find table zobrist_position
      with _ ->
        presence := false;
        (All, (-1), 0, Aucun, 0)
    in let no_tt_cut = ref true in
    if !presence && not ispv then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move no_tt_cut ply
    end;
    if !no_tt_cut then begin
      if profondeur = 0 then begin
        incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
      end
      else begin
        let no_cut = ref true in
        let hash_ordering = hash_move <> Aucun in
        if hash_ordering then begin
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve, nouveau_demi_coups = adapte_releve nouveau_zobrist hash_move profondeur releve_plateau demi_coups in
          joue plateau hash_move;
          let score = - pvs plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
          in if score > !best_score then begin
            best_score := score;
            best_move := hash_move;
            alpha0 := max !alpha0 score;
            let quiet_move = isquiet !best_move in
            if quiet_move then begin
              history_moves.(aux_history trait_aux_blancs).(depart !best_move).(arrivee !best_move) <- profondeur * profondeur
            end;
            if score >= !beta0 then begin
              no_cut := false;
              if quiet_move then begin
                killer_moves.(2 * ply + 1) <- killer_moves.(2 * ply);
                killer_moves.(2 * ply) <- !best_move;
              end;
            end
          end;
          dejoue plateau hash_move;
        end;
        if !no_cut then begin
          let cp =
            if hash_ordering then
              ref (List.filter (fun c -> c <> hash_move) (nouveau_tri plateau trait_aux_blancs dernier_coup droit_au_roque ply))
            else
              ref (nouveau_tri plateau trait_aux_blancs dernier_coup droit_au_roque ply)
          in if !cp = [] && not hash_ordering then begin
            incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := ply - 99999
            end 
            else begin
              best_score := 0
            end
          end
          else begin
            let first_move = ref (not hash_ordering) in
            while (!no_cut && !cp <> []) do
              let coup = List.hd !cp in
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve, nouveau_demi_coups = adapte_releve nouveau_zobrist coup profondeur releve_plateau demi_coups in
              joue plateau coup;
              cp := List.tl !cp;
              let score =
                if !first_move then begin
                  first_move := false;
                  - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
                end
                else begin
                  let note0 = - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !alpha0 - 1) (- !alpha0) evaluation nouveau_zobrist false
                  in if (note0 > !alpha0 && ispv) then begin 
                    - pvs plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist true
                  end
                  else begin
                    note0
                  end
                end 
              in if score > !best_score then begin
                best_score := score;
                best_move := coup;
                alpha0 := max !alpha0 score;
                let quiet_move = isquiet !best_move in
                if quiet_move then begin
                  history_moves.(aux_history trait_aux_blancs).(depart !best_move).(arrivee !best_move) <- profondeur * profondeur
                end;
                if score >= !beta0 then begin
                  no_cut := false;
                  if quiet_move then begin
                    killer_moves.(2 * ply + 1) <- killer_moves.(2 * ply);
                    killer_moves.(2 * ply) <- !best_move;
                  end;
                end
              end;
              dejoue plateau coup
            done
          end
        end
      end
    end;
    if not !stop_calculating then begin
      let node_type =
        if !best_score <= alpha then begin
          best_move := Aucun;
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
        if profondeur > hash_depth || ispv then begin
          ZobristHashtbl.replace table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
        end
      end
      else begin
        incr compteur_transposition;
        ZobristHashtbl.add table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
      end
    end;
    !best_score
  end