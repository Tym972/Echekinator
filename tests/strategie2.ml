(*Module implémentant des stratégies à suivre par le programme*)

open Libs.Plateau
open Libs.Generateur
open Libs.Strategie1
open Libs.Evaluations
open Libs.Quiescence
open Libs.Traduction1
open Libs.Traduction3
open Ouvertures
open Libs.Zobrist

(*Premier coup évité par le moteur s'il joue les blancs*)
let a_eviter =
  let coups_valides_joueur = coups_valides echiquier true Aucun (false, false, false, false) in
  let ht = Hashtbl.create 5 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ move_of_algebric echiquier "a3" true coups_valides_joueur, ();
      move_of_algebric echiquier "a4" true coups_valides_joueur, ();
      move_of_algebric echiquier "b3" true coups_valides_joueur, ();
      move_of_algebric echiquier "b4" true coups_valides_joueur, ();
      move_of_algebric echiquier "c3" true coups_valides_joueur, ();
      move_of_algebric echiquier "c4" true coups_valides_joueur, ();
      move_of_algebric echiquier "d3" true coups_valides_joueur, ();
      move_of_algebric echiquier "e3" true coups_valides_joueur, ();
      move_of_algebric echiquier "f3" true coups_valides_joueur, ();
      move_of_algebric echiquier "f4" true coups_valides_joueur, ();
      move_of_algebric echiquier "g3" true coups_valides_joueur, ();
      move_of_algebric echiquier "g4" true coups_valides_joueur, ();
      move_of_algebric echiquier "h3" true coups_valides_joueur, ();
      move_of_algebric echiquier "h4" true coups_valides_joueur, ();
      move_of_algebric echiquier "Na3" true coups_valides_joueur, ();
      move_of_algebric echiquier "Nc3" true coups_valides_joueur, ();
      move_of_algebric echiquier "Nf3" true coups_valides_joueur, ();
      move_of_algebric echiquier "Nh3" true coups_valides_joueur, (); ];
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

(*Affiche le plateau et l'enregistrement FEN*)
let affiche_coup plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau = 
  affiche plateau;
  print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau);
  print_newline ()

(*Fonction permettant d'afficher l'évaluation que fait le moteur d'une position*)
let affiche_score score profondeur_initiale trait_aux_blancs =
  if trait_aux_blancs then begin
    if abs score < 99000 then begin
      print_endline (Printf.sprintf "Score : %f" (float_of_int (score) /. 1000.))
    end
    else begin
      if score = -99950 then begin
        print_endline (Printf.sprintf "Score : #-%i" (profondeur_initiale / 2))
      end
      else if score = 99950 then begin
        print_endline (Printf.sprintf "Score : #%i" (profondeur_initiale / 2))
      end
      else if score = 99998 then begin
        print_endline "Score : -"
      end
      else if score mod 2 = 0 then begin
        print_endline (Printf.sprintf "Score : #%i" ((99999 - score) / 2))
      end
      else begin
        print_endline (Printf.sprintf "Score : #-%i" ((99999 + score) / 2))
      end
    end
  end
  else begin
    if abs score < 99000 then begin
      print_endline (Printf.sprintf "Score : %f" (-. float_of_int (score) /. 1000.))
    end
    else begin
      if score = -99950 then begin
        print_endline (Printf.sprintf "Score : #%i" (profondeur_initiale / 2))
      end
      else if score = 99950 then begin
        print_endline (Printf.sprintf "Score : #-%i" (profondeur_initiale / 2))
      end
      else if score = 99998 then begin
        print_endline "Score : -"
      end
      else if score mod 2 = 0 then begin
        print_endline (Printf.sprintf "Score : #-%i" ((99999 - score) / 2))
      end
      else begin
        print_endline (Printf.sprintf "Score : #%i" ((99999 + score) / 2))
      end
    end
  end

(*Fonction indiquant si une case est blanche*)
let est_blanche case =
  ((case / 8) mod 2 = 0 && (case mod 2) = 0) || ((case / 8) mod 2 = 1 && (case mod 2) = 1)

(*Fonction servant à cloturer la partie si un mat est strictement impossible. 4 cas sont considérés : roi contre roi, roi et fou contre roi, roi et cavalier contre roi et roi et fou contre roi et fou avec fous de même couleur*)
let manque_de_materiel plateau =
  let b = ref true in
  let compteur = ref 0 in
  let cavaliers_blancs = ref 0 in
  let cavaliers_noirs = ref 0 in
  let fous_blancs_cb = ref 0 in
  let fous_noirs_cb = ref 0 in
  let i = ref 0 in
  while (!b && !i < 64) do
    let case = plateau.(!i) in
    if case <> 0 then begin
      if tab_manque_de_materiel.(abs case) then begin
        b := false
      end
      else if abs case <> 6 then begin
        compteur := !compteur + 1;
        if !compteur > 2 then begin
          b := false
        end
        else begin
          match case with
            |2 -> cavaliers_blancs := !cavaliers_blancs + 1
            |(-2) -> cavaliers_noirs := !cavaliers_noirs + 1
            |3 -> if est_blanche !i then fous_blancs_cb := !fous_blancs_cb + 1
            |(-3) -> if est_blanche !i then fous_noirs_cb := !fous_noirs_cb + 1
            |_ -> ()
        end
      end
    end;
    incr i
  done;
  if !b then begin
    if !compteur = 2 then begin
      if (!cavaliers_blancs + !cavaliers_noirs > 0) then begin
        b := false
      end
      else if (!fous_blancs_cb <> !fous_noirs_cb) then begin
        b := false
      end
    end
  end;
  !b

(*Fonction permettant de jouer un coup donné et d'actualiser les variables relatives à l'état de la partie*)
let changement_du_trait plateau coup trait_aux_blancs dernier_coup releve_coups releve_plateau droit_au_roque regle_des_50_coups verif partie_finie =
  joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  if gagne plateau !trait_aux_blancs coup <> 2 then begin
    verif := (if !trait_aux_blancs then 1 else 2);
    partie_finie := true
  end
  else if repetition !releve_plateau 3 || List.length !releve_plateau = regle_des_50_coups || manque_de_materiel plateau then begin
    partie_finie := true
  end

(*Fonction permettant au moteur de joueur ses coups*)
let algo_decisionnel plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur profondeur_max duree_theorie duree_ouverture duree_finale temps_limite_court phase1 position_de_depart affichage evaluation_ouverture evaluation_mdj evaluation_finale droit_au_roque recherche =
  let profondeur_ajustee = ref profondeur in
  let score = ref 0 in
  let evaluation = ref evalue_simple in
  let coup = ref Aucun in
  let phase3 = ref false in
  let ref_temps = ref 0. in
  if affichage then begin
    affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau
  end;
  let theorie = ref (List.length !releve_coups < duree_theorie && position_de_depart = echiquier)
  in if !theorie then begin
    let openings = theoriques_possibles !releve_coups in
    if openings = [] then begin
      theorie := false
    end
    else begin
      if affichage then begin
        print_endline "Théorie"
      end;
      Random.self_init ();
      coup := List.nth openings (Random.int (List.length openings))
    end
  end;
  if not !theorie then begin
    let valides = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
    if List.length valides = 1 then begin
      if affichage then begin
        print_endline "Coup forcé"
      end;
      coup := List.hd valides
    end
    else begin
      if !phase1 then begin
        if (List.length !releve_coups > duree_ouverture || tours_connectees plateau !trait_aux_blancs) then begin
          phase1 := false
        end;
        if !phase1 then begin
          if affichage then begin
            print_endline "Ouverture"
          end;
          evaluation := evaluation_ouverture
        end
      end;
      if not !phase3 then begin
        if (finale plateau || List.length !releve_coups > duree_finale) then begin
          phase3 := true
        end
      end;
      if not (!phase1 || !phase3) then begin
        if affichage then begin
          print_endline "Milieu de jeu"
        end;
        evaluation := evaluation_mdj
      end;
      if !phase3 then begin
        if affichage then begin
          print_endline "Finale"
        end;
        evaluation := evaluation_finale
      end;
      while (!ref_temps < temps_limite_court && !profondeur_ajustee <= profondeur_max) do
        let (score_provisoire, candidat), temps = recherche plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau !profondeur_ajustee !evaluation in 
        ref_temps := temps;
        profondeur_ajustee := !profondeur_ajustee + 1;
        coup := candidat;
        score := score_provisoire;
      done;
      if affichage then begin
        print_endline (Printf.sprintf "Profondeur %i" (!profondeur_ajustee - 1));
        affiche_score !score (!profondeur_ajustee - 1) !trait_aux_blancs
      end
    end
  end;
  changement_du_trait plateau !coup trait_aux_blancs dernier_coup releve_coups releve_plateau droit_au_roque regle_des_50_coups verif partie_finie

(*Fonction permmetant de jouer les coups au hasard*)
let algo_hasard plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie (profondeur : int) (profondeur_max : int) (duree_theorie : int) (duree_ouverture : int) (duree_finale : int) (temps_limite_court : float) (phase1 : bool ref) (position_de_depart : int array) (affichage : bool)  (evaluation_ouverture : int array -> bool -> int -> bool -> int -> int -> int) (evaluation_mdj : int array -> bool -> int -> bool -> int -> int -> int) (evaluation_finale : int array -> bool -> int -> bool -> int -> int -> int) droit_au_roque (recherche : int array -> bool -> mouvement -> bool * bool * bool * bool -> int list -> int -> (int array -> bool -> int -> bool -> int -> int -> int) -> (int * mouvement) * float) =
  let _ = profondeur, profondeur_max, duree_theorie, duree_ouverture, duree_finale, temps_limite_court, phase1, position_de_depart, affichage, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche in
  if affichage then begin
    affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau;
    print_endline "Hasard"
  end;
  let valides = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
  Random.self_init ();
  let coup = List.nth valides (Random.int (List.length valides)) in
  changement_du_trait plateau coup trait_aux_blancs dernier_coup releve_coups releve_plateau droit_au_roque regle_des_50_coups verif partie_finie

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

(*Fonction renvoyant la liste des coups après mvvlva*)
let tri_mvvlva plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation algo =
  let _ = evaluation, releve_plateau, algo in
  List.map snd (tri_fusion (List.map (fun coup -> (mvvlva coup, coup)) (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque)))

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

let compteur_noeuds_terminaux = ref 0

let rec negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  let best_score = ref (-infinity) in
  let best_move = ref Aucun in
  if !stop_calculating || repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if profondeur = 0 then begin incr compteur_noeuds_terminaux;
    best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
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
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
        in let score =
          let note, _ = negalphabeta_quiescent plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
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

let negalphabetime_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation = (*if dernier_coup = Aucun then begin affiche plateau end;*)
  let t = Sys.time () in
  let fx = negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation in
  fx, (Sys.time () -. t)

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

let traitement_mat_profondeur_0 plateau trait_aux_blancs dernier_coup =
  let position_roi = index_tableau plateau (roi trait_aux_blancs) in
  if (menacee plateau position_roi trait_aux_blancs) then begin
    let cp = coups_valides plateau trait_aux_blancs dernier_coup (false, false, false, false)
    in if cp = [] then begin
      (- 99950)
    end
    else begin
      0
    end
  end
  else begin
    0
  end

let rec negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position =
  incr compteur_recherche;
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
    let hash_node_type, hash_depth, hash_value, hash_move, _ =
      try ZobristHashtbl.find table zobrist_position with _ ->
        begin
          presence := false;
          (All, (-1), 0, Aucun, 0)
        end
    in let continuation = ref true in
    if !presence (*&& not (List.length releve_plateau > 6 && (repetition (nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque (modification_roque hash_move droit_au_roque) :: releve_plateau) 3)*) then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation ply
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 profondeur_initiale evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0
      end
      else begin
        let b = ref true in
        let hash_ordering =
          (*not (hash_node_type = All || hash_move = Aucun) && hash_value > beta*)
          ((hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun)
        in if hash_ordering then begin
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = nouveau_zobrist hash_move dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau in
          joue plateau hash_move;
          let score =
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
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque plateau in
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau in
              joue plateau coup;
              cp := List.tl !cp;
              let score =
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
          end
        end
      end
    end;
    if not !stop_calculating then begin
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
        incr compteur_transposition;
        ZobristHashtbl.add table zobrist_position (node_type, profondeur, stored_value, !best_move, 0)
      end
    end;
    !best_score, !best_move
  end

let negalphabetime_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur (-infinity) infinity evaluation (List.hd releve_plateau) in
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
  let fx = iid_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur (-infinity) infinity evaluation in
  fx, (Sys.time () -. t)