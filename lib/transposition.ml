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

let enfant noeud = match noeud with
  |Pv -> Cut
  |Cut -> All 
  |All -> Cut

let (table : (noeuds * int * int * mouvement * int) ZobristHashtbl.t) =  ZobristHashtbl.create taille_transposition

let compteur_trans = ref 0

let traitement_hash (hash_node_type : noeuds) (hash_profondeur : int) (hash_valeur : int) (hash_best : mouvement) (profondeur : int) alpha beta valeur best continuation hash_depth = incr compteur_trans;
  if profondeur <= hash_profondeur then begin
    hash_depth := hash_profondeur;
    match hash_node_type with
    |Pv ->
      valeur := hash_valeur;
      best := hash_best;
      continuation := false
    |Cut ->
      alpha := max !alpha hash_valeur;
      if hash_valeur >= !beta then begin
        valeur := !alpha;
        continuation := false
      end
    |All ->
      beta := min !beta hash_valeur;
      if !alpha >= hash_valeur then begin
        valeur := !alpha;
        continuation := false
      end
  end

(*Fonction retirant les entrées de la hash table datant d'avant le n-ième coup.*)
let actualise table n =
  ZobristHashtbl.iter (fun key value -> let _, _, _, _, coup = value in if coup < n then ZobristHashtbl.remove table key ) table

let tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation node_type algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        joue plateau coup;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let x, _ = algo plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau) profondeur profondeur (-99999) 99999 evaluation (enfant node_type) (zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque) in
        dejoue plateau coup;
        (- x, coup) :: association liste_coup
      end
  in List.map snd (tri_fusion (association (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)))

let tri_mvvlva_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo node_type =
  let _ = evaluation, releve_plateau, algo, node_type, nouveau_zobrist in
  mvvlva (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)

(*Fonction renvoyant la liste de coups non triée*)
let non_tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo node_type =
  let _ = evaluation, algo, releve_plateau, node_type, nouveau_zobrist in
  coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque

let tri_0_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation node_type =
  tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 0 evaluation node_type

let tri_1_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation node_type =
  tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 1 evaluation node_type

let tri_2_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation node_type =
  tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 2 evaluation node_type

let tri_3_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation node_type =
  tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 3 evaluation node_type

let tri_4_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation node_type =
  tri_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau 4 evaluation node_type

(*Tableaux contenant les stratégies d'ordonnancement des coups aux pofondeurs correspondant à l'index + 1*)
(*let tab_tri_trans = [|non_tri_trans; tri_mvvlva_trans; tri_0_trans; tri_0_trans; tri_1_trans; tri_1_trans; tri_2_trans; tri_2_trans; tri_2_trans; tri_2_trans|]*)

let rec negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation node_type zobrist_position = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let continuation = ref true in
    let presence = ref true in
    let hash_depth = ref 0 in
    let hash_node_type, hash_profondeur, hash_valeur, hash_best, _ = try ZobristHashtbl.find table zobrist_position with _ -> begin
      presence := false; (Pv, 0, 0, Aucun, 0)
    end
    in if !presence then begin 
      traitement_hash hash_node_type hash_profondeur hash_valeur hash_best profondeur alpha0 beta0 best_score best_move continuation hash_depth
    end;
    if !continuation then begin
      if profondeur = 0 then begin
        best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0;
        ZobristHashtbl.add table zobrist_position (node_type, 0, !best_score, Aucun, 0)
      end
      else begin
        let cp = ref (tab_tri(*_trans*).(profondeur - 1) plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation (*node_type*) negalphabeta(*_trans*))
        in if !cp = [] then begin
          if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
            best_score := (profondeur_initiale - (profondeur + 99999))
          end 
          else begin
            best_score := 0
          end
        end
        else begin
          let b = ref true in
          while (!b && !cp <> []) do
            let coup = List.hd !cp in
            joue plateau coup;
            cp := List.tl !cp;
            let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
            let nouveau_zobrist = zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque in
            let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
            in let score =
              let note, _ = negalphabeta_trans plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation (enfant node_type) nouveau_zobrist
              in - note 
            in if score > !best_score then begin
              best_score := score;
              best_move := coup;
              alpha0 := max !alpha0 !best_score;
              if !alpha0 >= !beta0 then begin
                b := false
              end
            end;
            dejoue plateau coup
          done
        end;
        if !presence then begin
          if profondeur > !hash_depth then begin
            ZobristHashtbl.replace table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
          end
        end
        else begin
          ZobristHashtbl.add table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
        end
      end
    end
  end;
  !best_score, !best_move

let negalphabetime_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale (-99999) 99999 evaluation Pv (zobrist plateau trait_aux_blancs dernier_coup droit_au_roque) in
  fx, (Sys.time () -. t)