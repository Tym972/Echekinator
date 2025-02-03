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
    let tour_grand_roque = ref true in
    let case = ref 0 in
    for i = 0 to 4 do
      let piece = pieces.(i) in
      while position_de_depart.(!case) <> 0 do
        incr case
      done;
      position_de_depart.(!case) <- (- piece); position_de_depart.(!case + 56) <- piece;
      if piece = 4 then begin
        if !tour_grand_roque then begin
          depart_tour_noire_gr := !case;
          depart_tour_blanche_gr := !case + 56;
          tour_grand_roque := false
        end
        else begin
          depart_tour_noire_pr := !case;
          depart_tour_blanche_pr := !case + 56
        end
      end
      else if piece = 6 then begin
        depart_roi_noir := !case;
        depart_roi_blanc := !case + 56;
        clouage_roi_noir_1 := !case + 8;
        clouage_roi_noir_2 := !case + 16;
        clouage_roi_blanc_1 := !case + 48;
        clouage_roi_blanc_2 := !case + 40;
        if !case > 5 || !case < 3 then begin
          rois_centrees := false
        end
      end
    done
  in placement_krn z;
  releve_plateau := [zobrist position_de_depart true Aucun (true, true, true, true)];
  List.iter (fun tab -> Array.fill tab 0 (Array.length tab) 0) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr];
  List.iter (fun mut -> mut := 0) [longueur_chemin_roi_pr; longueur_chemin_roi_gr];
  List.iter (fun mut -> mut := []) [vides_blanc_pr; vides_blanc_gr; vides_noir_pr; vides_noir_gr];
  let j = ref 0 in
  let aux_vides vides_blanc vides_noir i =
    vides_blanc := i :: !vides_blanc;
    vides_noir := (i - 56) :: !vides_noir
  in let aux chemin_blanc chemin_noir longueur_chemin_roi vides_blanc vides_noir j i =
    chemin_blanc.(!j) <- i;
    chemin_noir.(!j) <- i - 56;
    incr longueur_chemin_roi;
    aux_vides vides_blanc vides_noir i;
    incr j
  in
  for i = !depart_roi_blanc + 1 to 62 do
    aux chemin_blanc_pr chemin_noir_pr longueur_chemin_roi_pr vides_blanc_pr vides_noir_pr j i
  done;
  j := 0;
  for i = !depart_roi_blanc - 1 downto 58 do
    aux chemin_blanc_gr chemin_noir_gr longueur_chemin_roi_gr vides_blanc_gr vides_noir_gr j i
  done;
  j := 0;
  for i = !depart_tour_blanche_gr + 1 to 59 do
    aux_vides vides_blanc_gr vides_noir_gr i
  done;
  for i = !depart_tour_blanche_pr - 1 downto 61 do
    aux_vides vides_blanc_pr vides_noir_pr i
  done;
  let rec supprime_doublon_triee liste = match liste with
    |[] -> []
    |[h] -> [h]
    |h :: g :: t -> if h = g then h :: supprime_doublon_triee t else h :: (supprime_doublon_triee (g :: t))
  in let rec aux_liste liste depart_tour depart_roi = match liste with
    |[] -> []
    |h::t when not (List.mem h [depart_tour; depart_roi]) -> h :: aux_liste t depart_tour depart_roi
    |_::t -> aux_liste t depart_tour depart_roi
  in vides_blanc_pr := List.rev (supprime_doublon_triee (tri_fusion (aux_liste !vides_blanc_pr !depart_tour_blanche_pr !depart_roi_blanc)));
  vides_noir_pr := List.rev (supprime_doublon_triee (tri_fusion (aux_liste !vides_noir_pr !depart_tour_noire_pr !depart_roi_noir)));
  vides_blanc_gr := supprime_doublon_triee (tri_fusion (aux_liste !vides_blanc_gr !depart_tour_blanche_gr !depart_roi_blanc));
  vides_noir_gr := supprime_doublon_triee (tri_fusion (aux_liste !vides_noir_gr !depart_tour_noire_gr !depart_roi_noir));
  
  print_endline (coord.(!depart_roi_blanc) ^ " " ^ coord.(!depart_tour_blanche_pr) ^ " " ^ coord.(!depart_tour_blanche_gr)
  ^ "\n" ^ coord.(!depart_roi_noir) ^ " " ^ coord.(!depart_tour_noire_pr) ^ " " ^ coord.(!depart_tour_noire_gr)
  ^ "\n" ^ string_of_bool !rois_centrees); print_newline ();
  print_endline ((string_of_int !longueur_chemin_roi_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_gr)); print_newline ();
  List.iter (fun tab -> Array.iter (fun c -> if c <> 0 then print_string (coord.(c) ^ " ")) tab; print_newline ()) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr]; print_newline ();
  List.iter (fun list -> List.iter (fun c -> print_string (coord.(c) ^ " ")) list; print_newline ()) [!vides_blanc_pr; !vides_blanc_gr; !vides_noir_pr; !vides_noir_gr]


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
  print_string "Voulez-vous une position initiale exotique : ";
  flush stdout;
  let exotique = input_line stdin in
  if est_oui exotique then begin
    print_string "Tapez 1 pour renseigner une position FEN, 2 pour une position aléatoire de Fischer : ";
    flush stdout;
    let choix = input_line stdin in
    if choix = "1" then begin
      print_string "Rentrez l'enregistrement fen souhaité : ";
      flush stdout;
      let chaine_fen = input_line stdin in
      position_of_fen chaine_fen position_de_depart trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau
    end
    else if choix = "2" then begin
      let code_string = ref "" in
      print_string "Souhaitez-vous une position spécifique : ";
      flush stdout;
      let choice = input_line stdin in
      if est_oui choice then begin
        print_string "Saisissez le code de la position : ";
        flush stdout;
        code_string := input_line stdin
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