(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Plateau
open Generateur
open Zobrist
open Evaluations
open Quiescence

(*Fonction permettant d'évaluer un plateau à la profondeur 0*)
let traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta =
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  if (menacee plateau position_roi trait_aux_blancs) then begin
    let cp = coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)
    in if cp = [] then begin
      (- 99950)
    end
    else begin
      evaluation plateau trait_aux_blancs position_roi true alpha beta
    end
  end
  else begin
    evaluation plateau trait_aux_blancs position_roi false alpha beta
  end

(*Fonction adaptant le relevé des positions*)
let adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau =
  if est_irremediable coup then begin
    if profondeur < 8 then begin
      []
    end
    else begin
      [zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque]
    end
  end
  else if List.length releve_plateau + profondeur < 8 then begin
    []
  end
  else begin 
    zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque :: releve_plateau
  end

(*Fonction triant une liste de coups selon leur potentiel*)
let tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        joue plateau coup;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let x, _ = algo plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau) profondeur profondeur (-99999) 99999 evaluation in
        dejoue plateau coup;
        (- x, coup) :: association liste_coup
      end
  in List.map snd (tri_fusion (association (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)))

(*Fonction triant une liste de coups selon la logique Most Valuable Victim - Least Valuable Agressor*)
let mvvlva liste =
  let rec association liste_coups = match liste_coups with
    |[] -> []
    |Classique {piece; depart; arrivee; prise} :: t ->
      ((tabvalue.(abs prise) - abs (piece)), Classique {piece; depart; arrivee; prise}) :: association t
    |Promotion {depart; arrivee; prise; promotion} :: t ->
      ((tabvalue.(abs prise) + tabvalue.(abs promotion) - 10), Promotion {depart; arrivee; prise; promotion}) :: association t
    |h :: t -> (0, h) :: association t
  in List.map snd (tri_fusion (association liste))

(*Fonction renvoyant la liste des coups après mvvlva*)
let tri_mvvlva plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo =
  let _ = evaluation, releve_plateau, algo in
  mvvlva (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)

(*Fonction renvoyant la liste de coups non triée*)
let non_tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo =
  let _ = evaluation, algo, releve_plateau in
  coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque

(*Fonction renvoyant la liste de coups après tri 0 pour des profondeurs paires*)
let tri_0 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation =
  tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 0 evaluation

(*Fonction renvoyant la liste de coups après tri 1 pour des profondeurs paires*)
let tri_1 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation =
  tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 1 evaluation

(*Fonction renvoyant la liste de coups après tri 2 pour des profondeurs paires*)
let tri_2 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation =
  tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 2 evaluation

(*Fonction renvoyant la liste de coups après tri 3 pour des profondeurs paires*)
let tri_3 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation =
  tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 3 evaluation

(*Fonction renvoyant la liste de coups après tri 4 pour des profondeurs paires*)
let tri_4 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation =
  tri plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 4 evaluation

(*Tableaux contenant les stratégies d'ordonnancement des coups aux pofondeurs correspondant à l'index + 1*)
let tab_tri = Array.concat [[|non_tri; tri_mvvlva; tri_0; tri_0; tri_1; tri_1|]; Array.make 300 tri_2]

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

(*Variable utilisée pour arrêter la recherche de force*)
let stop_calculating = ref false
let infinity = (Int64.to_int (Random.int64 4611686018427387903L))

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax, utilisé après l'ouverture. Les pat par répétitions sont pris en comptes*)
let rec negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = (*incr compteur_recherche;*)
  let best_score = ref (-infinity) in
  let best_move = ref Aucun in
  if !stop_calculating || repetition releve_plateau 3 then begin (*incr compteur_noeuds_terminaux;*)
    best_score := 0
  end
  else if profondeur = 0 then begin (*incr compteur_noeuds_terminaux;*)
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
    in if !cp = [] then begin (*incr compteur_noeuds_terminaux;*)
      if menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := profondeur_initiale - (profondeur + 99999)
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
        in let score =
          let note, _ = negalphabeta plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
          in - note
        in if score > !best_score then begin
          best_score := score;
          best_move := coup;
          if score >= beta then begin
            b := false
          end
          else begin
            alpha0 := max !alpha0 score
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_move

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

let adapte_releve3 zobrist_position coup profondeur releve_plateau demi_coups =
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
        if ispv then raise Not_found;
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
        let hash_ordering = hash_move <> Aucun && not (hash_node_type = Cut && hash_value <= beta) in
        if hash_ordering then begin
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve, nouveau_demi_coups = adapte_releve3 nouveau_zobrist hash_move profondeur releve_plateau demi_coups in
          joue plateau hash_move;
          let score = - pvs plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve nouveau_demi_coups (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist ispv
          in if score > !best_score then begin
            best_score := score;
            best_move := hash_move;
            alpha0 := max !alpha0 score;
            if score >= !beta0 then begin
              no_cut := false
            end
          end;
          dejoue plateau hash_move;
        end;
        if !no_cut then begin
          let cp =
            if hash_ordering then
              ref (List.filter (fun c -> c <> hash_move) (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta))
            else
              ref (tab_tri.(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
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
            let first_move = ref true in
            while (!no_cut && !cp <> []) do
              let coup = List.hd !cp in
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve, nouveau_demi_coups = adapte_releve3 nouveau_zobrist coup profondeur releve_plateau demi_coups in
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
                if score >= !beta0 then begin
                  no_cut := false
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
        if profondeur > hash_depth (*|| (ispv && not (hash_node_type = Pv)*) then begin
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