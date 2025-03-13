(*Module implémentant des stratégies à suivre par le programme*)

open Plateau
open Generateur
open Strategie1
open Evaluations
open Traduction3
open Zobrist

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

(*Fonction renvoyant le relevé des positions actualisé*)
let nouveau_releve_plateau dernier_coup releve_plateau plateau trait_aux_blancs droit_au_roque =
  if est_irremediable dernier_coup then begin
    [zobrist plateau trait_aux_blancs dernier_coup droit_au_roque]
  end
  else begin
    zobrist plateau trait_aux_blancs dernier_coup droit_au_roque :: !releve_plateau
  end

(*Fonction permettant de jouer un coup en actualisant les variables d'états de la partie*)
let joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau = 
  joue_coup_1 plateau coup trait_aux_blancs dernier_coup droit_au_roque;
  releve_coups := coup :: !releve_coups;
  releve_plateau := nouveau_releve_plateau coup releve_plateau plateau !trait_aux_blancs !droit_au_roque

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