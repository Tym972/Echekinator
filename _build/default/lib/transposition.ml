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

let traitement_hash (hash_node_type : noeuds) (hash_profondeur : int) (hash_valeur : int) (hash_best : mouvement) (profondeur : int) alpha beta valeur best continuation hash_depth = let _ = alpha, beta in
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

let rec negalphabeta_trans plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation node_type zobrist_position =
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  if repetition releve_plateau 3 then begin
    best_score := 0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let continuation = ref true in
    let presence = ref false in
    let hash_depth = ref 0 in
    if ZobristHashtbl.mem table zobrist_position then begin
      presence := true;
      let hash_node_type, hash_profondeur, hash_valeur, hash_best, _ = ZobristHashtbl.find table zobrist_position in
      traitement_hash hash_node_type hash_profondeur hash_valeur hash_best profondeur alpha0 beta0 best_score best_move continuation hash_depth
    end;
    if !continuation then begin
      if profondeur = 0 then begin
        best_score := traitement_profondeur_0  evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0;
        ZobristHashtbl.add table zobrist_position (node_type, 0, !best_score, Aucun, 0)
      end
      else begin
        let cp = ref (coups_joueur plateau profondeur trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta_valide)
        in if !cp = [] then begin
          if (menacee plateau (index plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
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
            let nouveau_releve =
              if est_irremediable coup then begin
                if profondeur < 8 then begin
                  []
                end
                else begin
                  [nouveau_zobrist]
                end
              end
              else if List.length releve_plateau + profondeur < 8 then begin
                []
              end
              else begin
                (nouveau_zobrist :: releve_plateau)
              end
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