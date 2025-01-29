(*Module implémentant les fonctions qui permettent de configurer les parties*)

open Plateau
open Generateur
open Evaluations
open Traduction1
open Traduction3
open Zobrist
open Strategie1
open Strategie2
open Quiescence
open Transposition
open Total

(*Fonction permettant d'acquérir l'entrée clavier*)
let est_oui reponse =
  List.mem (String.lowercase_ascii reponse) ["oui"; "o"; "yes"; "y"]

(*Fonction permettant de jouer une liste de coups*)
let joue_liste liste_coups plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs =
  let rec func liste_coups plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs controle = match liste_coups with
    |[] -> ()
    |coup :: t when !controle ->
      if List.mem coup (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque) then begin
        joue_coup_2 plateau coup trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
        func t plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs controle
      end
      else if coup = Aucun then begin
        func t plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs controle
      end
      else begin
        controle := false
      end
    |_ -> ()
  in func liste_coups plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs (ref true)

(*Affiche le plateau et l'enregistrement FEN*)
let affiche_coup plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau = 
  affiche plateau;
  print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau);
  print_newline ()

let remplir position_de_depart dernier_coup droit_au_roque releve_coups releve_plateau trait_aux_blancs = let _ = dernier_coup, droit_au_roque, releve_coups, releve_plateau in
  Array.fill position_de_depart 0 64 0;
  for i = 0 to 63 do
    position_de_depart.(i) <- 13;
    affiche position_de_depart;
    print_string "Entrez la pièce à la case marquée, ignorez si elle est vide : ";
    flush stdout;
    let piece_saisie = input_char stdin in
    if piece_saisie <> '\n' then begin
      ignore (input_line stdin);
      let piece = try Hashtbl.find dicorespondance piece_saisie with _ -> 0 in
      position_de_depart.(i) <- piece
    end
    else begin
      position_de_depart.(i) <- 0
    end
  done;
  print_string "Le trait est-il aux noirs? : ";
  flush stdout;
  let trait = input_line stdin in
  trait_aux_blancs := not (est_oui trait)
    
(*Fonction permettant à l'utilisateur de rentrer la position choisie*)
let initialisation () =
  let position_de_depart = Array.copy echiquier in
  let dernier_coup = ref Aucun in
  let droit_au_roque = ref (true, true, true, true) in
  let releve_coups = ref [] in
  let releve_plateau = ref [zobrist echiquier true Aucun (true, true, true, true)] in
  let trait_aux_blancs = ref true in
  print_string "Voulez-vous une position initiale exotique : ";
  flush stdout;
  let exotique = input_line stdin in
  if est_oui exotique then begin
    print_string "Tapez 1 pour remplir l'échiquier manuellement, 2 pour renseigner une position FEN, 3 pour une position aléatoire de Fischer : ";
    flush stdout;
    let choix = input_line stdin in
    if choix = "1" then begin
      remplir position_de_depart dernier_coup droit_au_roque releve_coups releve_plateau trait_aux_blancs
    end
    else if choix = "2" then begin
      print_string "Rentrez l'enregistrement fen souhaité : ";
      flush stdout;
      let chaine_fen = input_line stdin in
      position_of_fen chaine_fen position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau
    end
    else if choix = "3" then begin
      
    end;
    affiche_coup position_de_depart !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau
  end;
  position_de_depart, dernier_coup, droit_au_roque, releve_coups, releve_plateau, trait_aux_blancs


(*Fonction permettant la configuration de la partie*)
let config () =
  let affichage = true in
  let phase1 = ref true in
  let position_de_depart, dernier_coup, droit_au_roque, releve_coups, releve_plateau, trait_aux_blancs = initialisation () in
  let dernier_coup_initial = !dernier_coup  in
  let droit_au_roque_initial = !droit_au_roque in
  let releve_coups_initial = !releve_coups in
  let releve_plateau_initial = !releve_plateau in
  let trait_aux_blancs_initial = !trait_aux_blancs in
  let plateau = Array.copy position_de_depart in
  print_string "Des coups ont-ils été joués? : ";
  flush stdout;
  let historique = input_line stdin in
  if est_oui historique then begin
    print_string "Entrez la notation algébrique du début de partie : ";
    flush stdout;
    let reverse_historique = try algebric_to_type_mouvement (input_line stdin) !trait_aux_blancs !dernier_coup !droit_au_roque position_de_depart with _ -> [] in
    joue_liste reverse_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs;
    affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau
  end;
  print_string "Tapez 2 pour jouer à deux, tapez 1 pour jouer seul et tapez 0 pour être spectateur : ";
  flush stdout;
  let mode = input_line stdin in
  let temps_limite_court = ref 1.5 in
  let profondeur = ref 8 in
  let profondeur_max = ref 8 in
  if List.mem mode ["0"; "1"] then begin
    let pf = ref true in
    print_string "Jouez-vous avec une cadence lente? : ";
    flush stdout;
    let decision1 = input_line stdin in
    if est_oui decision1 then begin
      profondeur := 10;
      profondeur_max := 10;
      pf := false;
    end
    else begin
      print_string "Jouez-vous en blitz? : ";
      flush stdout;
      let decision2 = input_line stdin in
      if est_oui decision2 then begin
        profondeur := 6;
        profondeur_max := 6;
        pf := false
      end
      else begin
        print_string "Jouez-vous en bullet? : ";
        flush stdout;
        let decision2 = input_line stdin in
        if est_oui decision2 then begin
          profondeur := 4;
          profondeur_max := 4;
          pf := false
        end
      end
    end;
    if !pf then begin
      print_string "Voulez-vous utiliser une profondeur variable? : ";
      flush stdout;
      let decision2 = input_line stdin in
      if est_oui decision2 then begin
        profondeur_max := 10
      end
    end
  end;
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let regle_des_50_coups = 101 in
  let verif = ref 0 in
  let partie_finie = ref false in
  plateau, verif, mode, !temps_limite_court, !profondeur, !profondeur_max, duree_theorie, duree_ouverture, duree_finale, regle_des_50_coups, dernier_coup, releve_coups, releve_plateau, trait_aux_blancs, partie_finie, phase1, position_de_depart, trait_aux_blancs_initial, affichage, droit_au_roque, dernier_coup_initial, droit_au_roque_initial, releve_coups_initial, releve_plateau_initial

(*Configuration permettant de choisir les coups au hasard*)
let hasard =
  let nom = "Hasard" in
  let algo = algo_hasard in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = 0 in
  let profondeur_max = 0 in
  let temps_limite_court = 0. in
  let duree_theorie = 0 in
  let duree_ouverture = 0 in
  let duree_finale = 0 in
  let phase_1 = ref true in
  let recherche = negalphabetime_valide in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

let config_pf_quiescent n =
  let nom = Printf.sprintf "PF%i, RQ" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_quiescent in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

let config_pf_simple n =
  let nom = Printf.sprintf "PF%i, ES" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_simple in
  let evaluation_mdj = evalue_simple in
  let evaluation_finale = evalue_simple in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_valide in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur variable, supérieure ou égale à n*)
let config_pv n =
  let nom = Printf.sprintf "PV%i" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = n in
  let profondeur_max = 20 in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_valide in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur fixée n*)
let config_antirepet_pf n =
  let nom = Printf.sprintf "PF%i, ARS" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_valide in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur fixée n*)
let config_transpo_pf n =
  let nom = Printf.sprintf "PF%i, Transpo" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabeta_trans in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur fixée n*)
let config_pf_nve n =
  let nom = Printf.sprintf "PF%i, N.E" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = eval1 in
  let evaluation_mdj = eval2 in
  let evaluation_finale = eval3 in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_valide in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

let config_pf_totale n =
  let nom = Printf.sprintf "PF%i, A" n in
  let algo = algo_decisionnel in
  let evaluation_ouverture = evalue_ouverture in
  let evaluation_mdj = evalue_mdj in
  let evaluation_finale = evalue_finale in
  let profondeur = n in
  let profondeur_max = n in
  let temps_limite_court = 1.5 in
  let duree_theorie = 40 in
  let duree_ouverture = 30 in
  let duree_finale = 90 in
  let phase_1 = ref true in
  let recherche = negalphabetime_total in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche