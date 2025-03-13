(*Module implémentant la recherche Minimax et des fonctions nécessaire à l'élaboration de la stratégie*)

open Plateau
open Generateur
open Ouvertures
open Zobrist
open Evaluations

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

(*Fonction indiquant si un coup est irrémédiable (poussée de pion ou capture)*)
let est_irremediable coup = match coup with
  |Enpassant _ | Promotion _ -> true
  |Classique {piece; depart = _; arrivee = _; prise} when (abs piece = 1 || prise <> 0) -> true
  |_ -> false

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
let tab_tri = [|non_tri; tri_mvvlva; tri_0; tri_0; tri_1; tri_1; tri_2; tri_2; tri_2; tri_2;tri_2; tri_2; tri_2; tri_2;tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2;tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2;tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2; tri_2;tri_2; tri_2; tri_2; tri_2|]

let compteur_recherche = ref 0

let compteur_noeuds_terminaux = ref 0

(*Fonction détectant les répétitions à partir d'une liste de code zobrist*)
let repetition liste_releve_plateau n = match liste_releve_plateau with
  |[] -> false
  |h::q ->
    let rec aux liste k = match liste with
      |[] | [_] -> false
      |_::j::t -> (h = j && (k + 1 = n || aux t (k + 1))) || aux t k
    in aux q 1

(*Fonction indiquant si chaque joueur à moins de 3 pièces hors roi et pion sur l'échiquier, ou si leur nombre est inférieur à 6*)
let pieces_esseulee plateau =
  let pieces_blanches = ref 0 in
  let pieces_noires = ref 0 in
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case > 1 then begin
      pieces_blanches := !pieces_blanches + 1
    end
    else if case < (-1) then begin
      pieces_noires := !pieces_noires + 1
    end
  done;
  (!pieces_blanches < 3 && !pieces_noires < 3) || ((!pieces_blanches + !pieces_noires) < 6)

(*Fonction indiquant si l'un des deux joueurs n'a plus que son roi*)
let roi_seul plateau=
  let blancs = ref true in
  let noirs = ref true in
  for i = 0 to 63 do
    let case = plateau.(i) in
    if case > 0 && case <> 6 then begin
      blancs := false
    end;
    if case < 0 && case <> (-6) then begin
      noirs := false
    end
  done;
  !blancs || !noirs

(*Fonction indiquant si une partie est dans sa phase finale*)
let finale plateau =
  pieces_esseulee plateau || roi_seul plateau

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

(*Fonction renvoyant un appel à la fonction alphabeta_valide ansi que le temps nécessaire à l'éxécution*)
let negalphabetime plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation in
  fx, (Sys.time () -. t)

(*Premier coup évité par le moteur s'il joue les blancs*)
let a_eviter = 
  let ht = Hashtbl.create 5 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ (classic ('P', "a2", "a3"), ());
      (classic ('P', "a2", "a4"), ());
      (classic ('P', "b2", "b3"), ());
      (classic ('P', "b2", "b4"), ());
      (classic ('P', "c2", "c3"), ());
      (classic ('P', "c2", "c4"), ());
      (classic ('P', "d2", "d3"), ());
      (classic ('P', "e2", "e3"), ());
      (classic ('P', "f2", "f3"), ());
      (classic ('P', "f2", "f4"), ());
      (classic ('P', "g2", "g3"), ());
      (classic ('P', "g2", "g4"), ());
      (classic ('P', "h2", "h3"), ());
      (classic ('P', "h2", "h4"), ());
      (classic ('C', "b1", "a3"), ());
      (classic ('C', "b1", "c3"), ());
      (classic ('C', "g1", "f3"), ());
      (classic ('C', "g1", "h3"), ()); ];
  ht

(*Fonction établissant une liste des coups théoriques possibles*)
let theorie_search historique repertoire = 
  let l = ref [] in
  let coups_joues = List.length historique in
  if coups_joues = 0 then begin
    let rec fonc1 liste = match liste with
      |[] -> ()
      |h::t ->
        begin match h with
          |[]->()
          |h::_ -> if not (List.mem h !l || Hashtbl.mem a_eviter h) then 
            l := h :: !l
        end;
        fonc1 t
    in fonc1 repertoire
  end
  else begin
    let rec fonc2 liste = match liste with
      |[] -> ()
      |h::t -> begin
        if List.length h > coups_joues then begin
          let correspondance = ref true in
          let l1 = ref h in
          let l2 = ref (List.rev historique) in
          let i = ref 0 in
          while (!i < coups_joues && !l1 <> []) do
            incr i;
            if List.hd !l1 <> List.hd !l2 then begin
              correspondance := false
            end;
            l1 := List.tl !l1;
            l2 := List.tl !l2
          done;
          if !correspondance then begin
            let coup_theorique = List.nth h coups_joues in
            if not (List.mem coup_theorique !l) then begin
              l := coup_theorique :: !l
            end
          end
        end
      end;
      fonc2 t
    in fonc2 repertoire;
    if !l = [] then begin
      let rec fonc3 liste = match liste with
      |[] -> ()
      |h::t -> begin
        if List.length h > coups_joues then begin
          let rec fonc4 liste n = match liste,n with
            |_, 0 -> []
            |[], _ -> []
            |h::t, k ->  begin h :: fonc4 t (k - 1) end
          in let l1 = fonc4 h coups_joues in
          let rec fonc5 liste = match liste with
            |[] -> true
            |k::w -> (List.mem k l1 && fonc5 w)
          in if fonc5 historique then begin
            let coup_theorique = List.nth h coups_joues in
            if not (List.mem coup_theorique !l) then begin
              l := coup_theorique :: !l
            end
          end
        end
      end;
      fonc3 t
      in fonc3 repertoire
    end
  end;
  !l

(*Fonction renvoyant la liste des coups théoriques possibles*)
let theoriques_possibles historique =
  let l = theorie_search historique ouvertures_efficaces in if
  l <> [] then begin
    l
  end
  else begin
    theorie_search historique ouvertures_exhaustif
  end

(*Fonction indiquant si les deux tours d'un joueur son connectées*)
let tours_connectees plateau joueur = 
  let b = ref false in
  if Array.mem (tour joueur) plateau then begin
    let t = tab64.(index_tableau plateau (tour joueur)) in
    let s1 = ref true in
    let i = ref 0 in
    while (!i < 4 && !s1) do
      let dir = vect_tour.(!i) in
      let k = ref 1 in
      let s2 = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s2) do
        let candidat = coord.(tab120.(t + (!k * dir))) in
        let dest = plateau.(Hashtbl.find dicocoord candidat) in
        if dest <> 0 then begin
          if dest = tour joueur then begin
            b := true;
            s1 := false
          end;
          s2 := false
        end
        else
          incr k
      done;
      incr i
    done;
  end;
  !b