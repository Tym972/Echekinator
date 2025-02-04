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

(*Fonction permettant la lecture d'une réponse*)
let lire_entree message =
  print_string message;
  flush stdout;
  let entree = input_line stdin in
  supprimer entree

(*Permet de lire une entrée avec des sauts de lignes*)
let lire_entree_multiligne message =
  let temps_reel = ref 0. in
  print_string message;
  flush stdout;
  let rec lire_lignes acc b =
    let ligne = input_line stdin in
    if b && String.length ligne <> (-1) then begin 
      temps_reel := Unix.gettimeofday ()
    end;
    if (Unix.gettimeofday () -. !temps_reel) > 0.1 || (b && (String.length ligne < 60 || String.length ligne > 80)) then begin
      List.rev (ligne :: acc)
    end
    else begin
      lire_lignes (ligne :: acc) false
    end
  in
  String.concat " " (lire_lignes [] true)

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

(*Tableau représentant les dispositions KRN, l'indice correspondant au code*)
let krn = 
  [|[|2; 2; 4; 6; 4|]; [|2; 4; 2; 6; 4|]; [|2; 4; 6; 2; 4|]; [|2; 4; 6; 4; 2|]; [|4; 2; 2; 6; 4|];
    [|4; 2; 6; 2; 4|]; [|4; 2; 6; 4; 2|]; [|4; 6; 2; 2; 4|]; [|4; 6; 2; 4; 2|]; [|4; 6; 4; 2; 2|]|]

(*Fonction permettant de jouer une partie d'échec 960*)
let fischer code_string position_de_depart releve_plateau =
  for i = 0 to 7 do
    position_de_depart.(i) <- 0;
    position_de_depart.(i + 56) <- 0;
  done;
  Random.self_init ();
  let code = ref 518 in
  code := (try int_of_string code_string with _ -> (Random.int 960));
  if !code < 0 || !code > 959 then begin
    code := Random.int 960
  end;
  let x = !code / 4 in
  let xmod = !code mod 4 in
  let y = x / 4 in
  let ymod = x mod 4 in
  let z = y / 6 in
  let zmod = y mod 6 in
  let placement_fou_blanc placement = position_de_depart.(2 * placement + 1) <- (-3); position_de_depart.(2 * placement + 57) <- 3 in
  placement_fou_blanc xmod;
  let placement_fou_noir placement = position_de_depart.(2 * placement) <- (-3); position_de_depart.(2 * placement + 56) <- 3 in
  placement_fou_noir ymod;
  let placement_dame placement =
    let rec aux placement case = match placement with
      |0 ->
        if position_de_depart.(case) = 0 then begin
          position_de_depart.(case) <- (-5); position_de_depart.(case + 56) <- 5
        end
        else begin 
          aux 0 (case + 1)
        end
      |n ->
        if position_de_depart.(case) = 0 then begin
          aux (n - 1) (case + 1)
        end
        else begin
          aux n (case + 1)
        end
    in aux placement 0
  in placement_dame zmod;
  let placement_krn placement =
    let pieces = krn.(placement) in
    let case = ref 0 in
    for i = 0 to 4 do
      let piece = pieces.(i) in
      while position_de_depart.(!case) <> 0 do
        incr case
      done;
      position_de_depart.(!case) <- (- piece); position_de_depart.(!case + 56) <- piece;
    done
  in placement_krn z;
  actualisation_roque position_de_depart;
  releve_plateau := [zobrist position_de_depart true Aucun (true, true, true, true)]

(*Affiche le plateau et l'enregistrement FEN*)
let affiche_coup plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau = 
  affiche plateau;
  print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau);
  print_newline ()
    
(*Fonction permettant à l'utilisateur de rentrer la position choisie*)
let initialisation () =
  let position_de_depart = Array.copy echiquier in
  let dernier_coup = ref Aucun in
  let droit_au_roque = ref (true, true, true, true) in
  let releve_coups = ref [] in
  let releve_plateau = ref [zobrist echiquier true Aucun (true, true, true, true)] in
  let trait_aux_blancs = ref true in
  let exotique = lire_entree "Voulez-vous une position initiale exotique : " in
  if est_oui exotique then begin
    let choix = lire_entree "Tapez 1 pour renseigner une position FEN, 2 pour une position aléatoire de Fischer : " in
    if choix = "1" then begin
      let chaine_fen = lire_entree "Rentrez l'enregistrement fen souhaité : " in
      position_of_fen chaine_fen position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau
    end
    else if choix = "2" then begin
      let code_string = ref "" in
      let choice = lire_entree "Souhaitez-vous une position spécifique : " in
      if est_oui choice then begin
        code_string := lire_entree "Saisissez le code de la position : "
      end;
      fischer !code_string position_de_depart releve_plateau
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
  let historique = lire_entree "Des coups ont-ils été joués? : " in
  if est_oui historique then begin
    let reverse_historique = try algebric_to_type_mouvement (lire_entree_multiligne "Entrez la notation algébrique du début de partie : ") !trait_aux_blancs !dernier_coup !droit_au_roque position_de_depart with _ -> [] in
    joue_liste reverse_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs;
    affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau
  end;
  let mode = lire_entree "Tapez 2 pour jouer à deux, tapez 1 pour jouer seul et tapez 0 pour être spectateur : " in
  let temps_limite_court = ref 1.5 in
  let profondeur = ref 8 in
  let profondeur_max = ref 8 in
  if List.mem mode ["0"; "1"] then begin
    let pf = ref true in
    let decision1 = lire_entree "Jouez-vous avec une cadence lente? : " in
    if est_oui decision1 then begin
      profondeur := 10;
      profondeur_max := 10;
      pf := false;
    end
    else begin
      let decision2 = lire_entree "Jouez-vous en blitz? : " in
      if est_oui decision2 then begin
        profondeur := 6;
        profondeur_max := 6;
        pf := false
      end
      else begin
        let decision2 = lire_entree "Jouez-vous en bullet? : " in
        if est_oui decision2 then begin
          profondeur := 4;
          profondeur_max := 4;
          pf := false
        end
      end
    end;
    if !pf then begin
      let decision2 = lire_entree "Voulez-vous utiliser une profondeur variable? : " in
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
  let recherche = negalphabetime in
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
  let recherche = negalphabetime in
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
  let recherche = negalphabetime in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur fixée n*)
let config_pf n =
  let nom = Printf.sprintf "PF%i" n in
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
  let recherche = negalphabetime in
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
  let recherche = negalphabetime in
  nom, algo, profondeur, profondeur_max, temps_limite_court, duree_theorie, duree_ouverture, duree_finale, phase_1, evaluation_ouverture, evaluation_mdj, evaluation_finale, recherche

(*Configuration pour une profondeur fixée n*)
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