(*Module implémentant les différents différents mode de jeu*)

open Plateau
open Generateur
open Traduction1
open Traduction2
open Traduction3
open Zobrist
open Strategie1
open Strategie2
open Evaluations
open Ouvertures
open Config

(*Fonction permettant de traiter une proposition de coup du joueur*)
let traitement choix coups_valides_joueur plateau dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie regle_des_50_coups proposition_invalide droit_au_roque =
  if List.mem choix coups_valides_joueur then begin
    changement_du_trait plateau choix trait_aux_blancs dernier_coup releve_coups releve_plateau droit_au_roque regle_des_50_coups verif partie_finie
  end
  else begin
    print_string "Le coup tenté est invalide, réessayez\n";
    proposition_invalide := true;
  end

(*Fonction gérant l'annulation d'un coup*)
let annule_coup plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau affiche_joueur position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial =
  let coup_intermediaire = List.nth !releve_coups 1 in
  let aux liste = match liste with
    |_ :: _ :: k :: t -> List.rev (k :: t)
    |_ -> []
  in let ancien_historique = aux !releve_coups in
  reinitialise plateau dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial;
  joue_liste ancien_historique plateau dernier_coup releve_coups releve_plateau droit_au_roque (ref trait_aux_blancs_initial);
  print_endline ("Annulation du dernier coup des " ^ affiche_joueur);
  joue plateau coup_intermediaire;
  affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau;
  dejoue plateau coup_intermediaire

(*Fonction permettant à un utilisateur connaissant la notation algébrique de jouer ses coups*)
let jeu_humain plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif proposition_invalide partie_finie droit_au_roque position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial =
  affiche_coup plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau;
  let affiche_joueur = if !trait_aux_blancs then "blancs" else "noirs" in
  print_string ("Au tour des " ^ affiche_joueur ^ " de jouer\n");
  let coups_valides_joueur = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
  print_string "Entrez votre coup : ";
  flush stdout;
  let coup = supprimer (input_line stdin) in
  if coup = "X-X-X" && (List.length (List.filter (fun coup -> coup <> Aucun) !releve_coups)) > 1 then begin
    annule_coup plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau affiche_joueur position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
  end
  else if coup = "ABANDON" then begin
    partie_finie := true;
    verif := (if !trait_aux_blancs then 1 else 2)
  end
  else begin
    let choix = tolerance plateau coup !trait_aux_blancs coups_valides_joueur
    in traitement choix coups_valides_joueur plateau dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie regle_des_50_coups proposition_invalide droit_au_roque
  end

(*Fonction indiquant le résultat d'une partie (unique)*)
let fin_de_partie plateau dernier_coup droit_au_roque releve_coups releve_plateau regle_des_50_coups verif position_de_depart dernier_coup_initial droit_au_roque_initial =
  let pgn_partie = ref (type_mouvement_to_algebric releve_coups position_de_depart dernier_coup_initial droit_au_roque_initial) in
  let annonce_resultat =
    if repetition releve_plateau 3 then begin
      "La partie est nulle par triple répétition"
    end
    else if (manque_de_materiel plateau) then begin
      "La partie est nulle par manque de matériel"
    end
    else if (verif = 0 && List.length (releve_plateau) = regle_des_50_coups) then begin
      (string_of_int ((regle_des_50_coups - 1) / 2)) ^ " coups se sont écoulés depuis la dernière capture ou la dernière poussée d'un pion. La partie est nulle."
    end
    else begin
      if verif = 1 then begin
        let resultat = gagne plateau true dernier_coup in
        if resultat = -1 then begin
          "Les noirs ont gagné"
        end
        else if resultat = 0 then begin
          "La partie est nulle par pat"
        end
        else begin
          pgn_partie := (try String.sub !pgn_partie 0 (String.length !pgn_partie - 7)  with _ -> "") ^ "0-1";
          "Les blancs ont abandonné"
        end
      end
      else if verif = 2 then begin
        let resultat = gagne plateau false dernier_coup in
        if resultat = 1 then begin
          "Les blancs ont gagné"
        end
        else if resultat = 0 then begin
          "La partie est nulle par pat"
        end
        else begin
          pgn_partie := (try String.sub !pgn_partie 0 (String.length !pgn_partie - 7) with _ -> "") ^ "1-0";
          "Les noirs ont abandonné"
        end
      end
      else begin
        ""
      end
    end
  in if !pgn_partie <> "" then begin
    affiche plateau;
    print_endline (fen plateau (List.length releve_coups mod 2 = 0) dernier_coup droit_au_roque releve_coups releve_plateau);
    print_endline !pgn_partie;
    print_newline ();
    print_endline annonce_resultat
  end

(*Permet de lancer une partie*)
let partie_unique () =
  let plateau, verif, mode, temps_limite_court, profondeur, profondeur_max, duree_theorie, duree_ouverture, duree_finale, regle_des_50_coups, dernier_coup, releve_coups, releve_plateau, trait_aux_blancs, partie_finie, phase1, position_de_depart, trait_aux_blancs_initial, affichage, droit_au_roque, dernier_coup_initial, droit_au_roque_initial, releve_coups_initial, releve_plateau_initial = config () in
  if mode = "2" then begin
    while not !partie_finie do
      let proposition_invalide = ref false in
      if !trait_aux_blancs then begin
        jeu_humain plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif proposition_invalide partie_finie droit_au_roque position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      end;
      if not (!trait_aux_blancs || !partie_finie) then begin
        jeu_humain plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif proposition_invalide partie_finie droit_au_roque position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
      end
    done
  end
  else if mode = "1" then begin
    print_string "Tapez 1 si vous voulez jouez les blancs, 2 si vous voulez jouez les noirs : ";
    flush stdout;
    let ordre = input_line stdin in
    if ordre = "1" then begin
      while not !partie_finie do
        let proposition_invalide = ref false in
        if !trait_aux_blancs then begin
          jeu_humain plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif proposition_invalide partie_finie droit_au_roque position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
        end;
        if not (!trait_aux_blancs || !partie_finie) then begin
          algo_decisionnel plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur profondeur_max duree_theorie duree_ouverture duree_finale temps_limite_court phase1 position_de_depart affichage evalue_ouverture evalue_mdj evalue_finale droit_au_roque negalphabetime_valide
        end
      done
    end
    else if ordre = "2" then begin
      while not !partie_finie do
        let proposition_invalide = ref false in
        if !trait_aux_blancs then begin
          algo_decisionnel plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur profondeur_max duree_theorie duree_ouverture duree_finale temps_limite_court phase1 position_de_depart affichage evalue_ouverture evalue_mdj evalue_finale droit_au_roque negalphabetime_valide
        end;
        if not (!trait_aux_blancs || !partie_finie) then begin
          jeu_humain plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif proposition_invalide partie_finie droit_au_roque position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial
        end
      done  
    end
  end
  else if mode = "0" then begin
    while not !partie_finie do
      if !trait_aux_blancs then begin
        algo_decisionnel plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur profondeur_max duree_theorie duree_ouverture duree_finale temps_limite_court phase1 position_de_depart affichage evalue_ouverture evalue_mdj evalue_finale droit_au_roque negalphabetime_valide
      end;
      if not (!trait_aux_blancs || !partie_finie) then begin
        algo_decisionnel plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur profondeur_max duree_theorie duree_ouverture duree_finale temps_limite_court phase1 position_de_depart affichage evalue_ouverture evalue_mdj evalue_finale droit_au_roque negalphabetime_valide
      end
    done
  end;
  if List.mem mode ["0"; "1"; "2"] then begin
    fin_de_partie plateau !dernier_coup !droit_au_roque !releve_coups !releve_plateau regle_des_50_coups !verif position_de_depart dernier_coup_initial droit_au_roque_initial
  end

(*Date locale format yyyy.mm.dd*)
let date () =
  let time = Unix.time () in
  let local_time = Unix.gmtime time in
  let year = local_time.tm_year + 1900 in
  let month = local_time.tm_mon + 1 in
  let day = local_time.tm_mday in 
  Printf.sprintf "%04d.%02d.%02d" year month day

(*Heure locale format hh:mm:ss*)
let heure () =
  let time = Unix.time () in
  let local_time = Unix.localtime time in
  let hour = local_time.tm_hour in
  let minute = local_time.tm_min in
  let second = local_time.tm_sec in
  Printf.sprintf "%02d:%02d:%02d" hour minute second

(*Fonction formattant une durée*)
let format_time duration =
  let hours = int_of_float (duration /. 3600.0) in
  let minutes = int_of_float ((duration -. (float_of_int (hours * 3600))) /. 60.0) in
  let seconds = int_of_float (duration -. (float_of_int (hours * 3600 + minutes * 60))) in
  let centiseconds = int_of_float ((duration -. (float_of_int (hours * 3600 + minutes * 60 + seconds))) *. 100.0) in
  Printf.sprintf "%02d:%02d:%02d.%02d" hours minutes seconds centiseconds

(*Fonction traitant la fin d'un ensemble de parties*)
let fin_de_partie2 plateau dernier_coup droit_au_roque releve_coups releve_plateau regle_des_50_coups verif position_de_depart dernier_coup_initial droit_au_roque_initial doc victoire_blanches_j1 victoire_noires_j1 victoire_blanches_j2 victoire_noires_j2 manque_materiel pat triple_repetition coups50 i m nom1 nom2 affichage htbl =
  let round = string_of_int (i + 1) in
  let resultat = ref "1/2-1/2" in
  let releve = type_mouvement_to_algebric releve_coups position_de_depart dernier_coup_initial droit_au_roque_initial in
  let fonc = try Hashtbl.replace htbl releve (let nombre, rondes = (Hashtbl.find htbl releve) in nombre + 1, (i + 1) :: rondes) with _ -> Hashtbl.add htbl releve (1, [i + 1]) in fonc;
  let white = if i < m then nom1 else nom2 in
  let black = if i < m then nom2 else nom1 in
  if affichage then begin
    affiche plateau;
    print_endline (fen plateau (List.length releve_coups mod 2 = 0) dernier_coup droit_au_roque releve_coups releve_plateau);
    print_endline releve;
    print_newline ()
  end;
  if repetition releve_plateau 3 then begin
    if affichage then begin
      print_endline "La partie est nulle par triple répétition"
    end;
    triple_repetition := !triple_repetition + 1
  end
  else if (manque_de_materiel plateau) then begin
    if affichage then begin
      print_endline "La partie est nulle par manque de matériel"
    end;
    manque_materiel := !manque_materiel + 1
  end
  else if (verif = 0 && List.length releve_plateau = regle_des_50_coups) then begin
    if affichage then begin
      print_endline ((string_of_int ((regle_des_50_coups - 1) / 2)) ^ " coups se sont écoulés depuis la dernière capture ou la dernière poussée d'un pion. La partie est nulle.")
    end;
    coups50 := !coups50 + 1
  end
  else begin
    if verif = 1 then begin
      if gagne plateau true dernier_coup = -1 then begin
        if i < m then begin
          if affichage then begin
            print_endline (nom2 ^ " a gagné avec les noirs")
          end;
          victoire_noires_j2 := !victoire_noires_j2 + 1;
          resultat := "0-1"
        end
        else begin
          if affichage then begin
            print_endline (nom1 ^ " a gagné avec les noirs")
          end;
          victoire_noires_j1 := !victoire_noires_j1 + 1;
          resultat := "0-1"
        end
      end
      else begin
        if affichage then begin
          print_endline "La partie est nulle par pat"
        end;
        pat := !pat + 1
      end
    end
    else begin
      if gagne plateau false dernier_coup = 1 then begin
        if i < m then begin
          if affichage then begin
            print_endline (nom1 ^ " a gagné avec les blancs")
          end;
          victoire_blanches_j1 := !victoire_blanches_j1 + 1;
          resultat := "1-0"
        end
        else begin
          if affichage then begin
            print_endline (nom2 ^ " a gagné avec les blancs")
          end;
          victoire_blanches_j2 := !victoire_blanches_j2 + 1;
          resultat := "1-0"
        end
      end
      else begin
        if affichage then begin
          print_endline "La partie est nulle par pat"
        end;
        pat := !pat + 1
      end
    end
  end;
  if affichage then begin
    print_newline ()
  end;
  doc := !doc ^ "[Event \"?\"]" ^ "\n" 
        ^ "[Site \"?\"]" ^ "\n" 
        ^ "[Date \"" ^ date () ^ "\"]" ^ "\n"
        ^ "[Heure \"" ^ heure () ^ "\"]" ^ "\n"
        ^ "[Round \"" ^ round ^ "\"]" ^ "\n" 
        ^ "[White \"" ^ white ^ "\"]" ^ "\n" 
        ^ "[Black \"" ^ black ^ "\"]" ^ "\n" 
        ^ "[Result \"" ^ !resultat ^ "\"]" ^ "\n"
        ^ releve ^ "\n" ^ "\n"

(*Fonction générant de façon aléatoire une liste de k nombres distincts dans l'intervalle [0 ; n[*)
let ensemble_aleatoire k n =
  if (k < 0 || k > n) then begin
    failwith "Invalid Argument"
  end
  else begin
    let l = ref [] in
    let table = Hashtbl.create k in
    while List.length !l < k do
      let i = Random.int n in
      if not (Hashtbl.mem table i) then begin
        Hashtbl.add table i ();
        l := i :: !l
      end
    done;
    !l
  end

(*Permet de lancer un ensemble de partie en consignant le résultat*)
let ensemble all_openings anti_repet nombre affichage =
  let heure_debut = Sys.time () in
  let repertoire_ouverture = ouvertures_echantillon in
  let nombre_ouvertures = List.length repertoire_ouverture in
  let doc = ref "" in
  let config_j1 = config_pf 4 in
  let config_j2 = config_pf 4 in
  let nom_j1, algo_j1, profondeur_j1, profondeur_max_j1, temps_limite_court_j1, duree_theorie_j1, duree_ouverture_j1, duree_finale_j1, phase1_j1, evaluation_ouverture_j1, evaluation_mdj_j1, evaluation_finale_j1, recherche_j1 = config_j1 in
  let nom_j2, algo_j2, profondeur_j2, profondeur_max_j2, temps_limite_court_j2, duree_theorie_j2, duree_ouverture_j2, duree_finale_j2, phase1_j2, evaluation_ouverture_j2, evaluation_mdj_j2, evaluation_finale_j2, recherche_j2 = config_j2 in
  let init_phase1_j1 = !phase1_j1 in
  let init_phase1_j2 = !phase1_j2 in
  let victoires_blanches_j1 = ref 0 in
  let victoires_noires_j1 = ref 0 in
  let victoires_blanches_j2 = ref 0 in
  let victoires_noires_j2 = ref 0 in
  let nulles = ref 0 in
  let manque_materiel = ref 0 in
  let pat = ref 0 in
  let triple_repetition = ref 0 in
  let coups50 = ref 0 in
  let n = if (all_openings || (anti_repet && nombre > nombre_ouvertures)) then nombre_ouvertures else nombre in
  let m = (n / 2) in
  let htbl = Hashtbl.create n in
  let ens = ref [] in
  if anti_repet then begin
    ens := ensemble_aleatoire n nombre_ouvertures
  end;
  for i = 0 to (n - 1) do
    phase1_j1 := init_phase1_j1;
    phase1_j2 := init_phase1_j2;
    print_endline ("Ronde " ^ string_of_int (i + 1));
    let plateau = Array.copy echiquier in
    let verif = ref 0 in
    let droit_au_roque = ref (true, true, true, true) in
    let regle_des_50_coups = 101 in
    let dernier_coup = ref Aucun in
    let releve_coups = ref [] in
    let releve_plateau = ref [zobrist plateau true !dernier_coup !droit_au_roque] in
    let trait_aux_blancs = ref true in
    if all_openings then begin
      joue_liste (List.nth repertoire_ouverture i) plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs;
    end;
    if anti_repet then begin
      joue_liste (List.nth repertoire_ouverture (List.nth !ens i)) plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs;
    end;
    let partie_finie = ref false in
    let position_de_depart = echiquier in
    let dernier_coup_initial = !dernier_coup  in
    let droit_au_roque_initial = !droit_au_roque in
    while not !partie_finie do
      if !trait_aux_blancs then begin
        if i < m then begin
          algo_j1 plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur_j1 profondeur_max_j1 duree_theorie_j1 duree_ouverture_j1 duree_finale_j1 temps_limite_court_j1 phase1_j1 position_de_depart affichage evaluation_ouverture_j1 evaluation_mdj_j1 evaluation_finale_j1 droit_au_roque recherche_j1
        end
        else begin
          algo_j2 plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur_j2 profondeur_max_j2 duree_theorie_j2 duree_ouverture_j2 duree_finale_j2 temps_limite_court_j2 phase1_j2 position_de_depart affichage evaluation_ouverture_j2 evaluation_mdj_j2 evaluation_finale_j2 droit_au_roque recherche_j2
        end
      end;
      if not (!trait_aux_blancs || !partie_finie) then begin
        if i < m then begin
          algo_j2 plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur_j2 profondeur_max_j2 duree_theorie_j2 duree_ouverture_j2 duree_finale_j2 temps_limite_court_j2 phase1_j2 position_de_depart affichage evaluation_ouverture_j2 evaluation_mdj_j2 evaluation_finale_j2 droit_au_roque recherche_j2
        end
        else begin
          algo_j1 plateau regle_des_50_coups dernier_coup releve_coups releve_plateau trait_aux_blancs verif partie_finie profondeur_j1 profondeur_max_j1 duree_theorie_j1 duree_ouverture_j1 duree_finale_j1 temps_limite_court_j1 phase1_j1 position_de_depart affichage evaluation_ouverture_j1 evaluation_mdj_j1 evaluation_finale_j1 droit_au_roque recherche_j1
        end
      end
    done;
    fin_de_partie2 plateau !dernier_coup !droit_au_roque !releve_coups !releve_plateau regle_des_50_coups !verif position_de_depart dernier_coup_initial droit_au_roque_initial doc victoires_blanches_j1 victoires_noires_j1 victoires_blanches_j2 victoires_noires_j2 manque_materiel pat triple_repetition coups50 i m nom_j1 nom_j2 affichage htbl
  done;
  let temps = Sys.time () -. heure_debut in
  let temps_moyen = temps /. float_of_int n in
  nulles := !manque_materiel + !pat + !triple_repetition + !coups50;
  let victoires_j1 = !victoires_blanches_j1 + !victoires_noires_j1 in
  let victoires_j2 = !victoires_blanches_j2 + !victoires_noires_j2 in
  let victoires_blanches = !victoires_blanches_j1 + !victoires_blanches_j2 in
  let victoires_noires = !victoires_noires_j1 + !victoires_noires_j2 in
  let parties_deja_jouees = ref 0 in
  let rondes_similaires = ref [] in
  Hashtbl.iter (fun _ (nombre, rondes) -> (if nombre > 1 then begin parties_deja_jouees := !parties_deja_jouees + (nombre - 1); rondes_similaires := rondes :: !rondes_similaires end)) htbl;
  let rec fonc2 liste = match liste with
    |h::t when h <> [] -> begin
      let rec aux liste = match liste with
        |[] -> ""
        |g :: [] -> Printf.sprintf "%i, " g
        |g::k -> Printf.sprintf "%i, " g ^ aux k
      in Printf.sprintf "Les parties %i, " (List.hd h) ^ aux (List.tl h) ^ "sont identiques." ^ "\n" ^ fonc2 t
    end
    |_ -> ""
  in let repet = fonc2 (List.rev ((tri_fusion (List.map (fun l -> List.rev (tri_fusion l)) !rondes_similaires))))
  in let bilan = 
    Printf.sprintf "%s a gagné %i parties, soit %.2f%% des parties. " nom_j1 victoires_j1 (((float_of_int victoires_j1) /. (float_of_int n)) *. 100.)  ^ 
    Printf.sprintf "%i l'ont été avec les blancs (%.2f%% des victoires). " !victoires_blanches_j1 (((float_of_int !victoires_blanches_j1) /. (float_of_int victoires_j1)) *. 100.)  ^ 
    Printf.sprintf "%i l'ont été avec les noirs (%.2f%% des victoires). " !victoires_noires_j1 (((float_of_int !victoires_noires_j1) /. (float_of_int victoires_j1 )) *. 100.) ^ "\n" ^ "\n" ^
    Printf.sprintf "%s a gagné %i parties, soit %.2f%% des parties. " nom_j2 victoires_j2 (((float_of_int victoires_j2) /. (float_of_int n)) *. 100.)  ^
    Printf.sprintf "%i l'ont été avec les blancs (%.2f%% des victoires). " !victoires_blanches_j2 (((float_of_int !victoires_blanches_j2) /. (float_of_int victoires_j2)) *. 100.)  ^ 
    Printf.sprintf "%i l'ont été avec les noirs (%.2f%% des victoires). " !victoires_noires_j2 (((float_of_int !victoires_noires_j2) /. (float_of_int victoires_j2)) *. 100.) ^ "\n" ^ "\n" ^
    Printf.sprintf "%i parties sont nulles, soit %.2f%% des parties. " !nulles (((float_of_int !nulles) /. (float_of_int n)) *. 100.)  ^ 
    Printf.sprintf "%i l'ont été par manque de matériel (%.2f%% des nulles). " !manque_materiel (((float_of_int !manque_materiel) /. (float_of_int !nulles)) *. 100.) ^ 
    Printf.sprintf "%i l'ont été par pat (%.2f%% des nulles). " !pat (((float_of_int !pat) /. (float_of_int !nulles)) *. 100.)  ^ "\n" ^
    Printf.sprintf "%i l'ont été par triple répétition (%.2f%% des nulles). " !triple_repetition (((float_of_int !triple_repetition) /. (float_of_int !nulles)) *. 100.)  ^
    Printf.sprintf "%i l'ont été par la règle des 50 coups (%.2f%% des nulles). " !coups50 (((float_of_int !coups50) /. (float_of_int !nulles)) *. 100.)  ^ "\n" ^ "\n" ^ 
    Printf.sprintf "Les blancs ont gagné %i partie (%.2f%% des parties). Les noirs en ont gagné %i (%.2f%%)." (victoires_blanches) (((float_of_int victoires_blanches) /. (float_of_int n)) *. 100.) (victoires_noires) (((float_of_int victoires_noires) /. (float_of_int n)) *. 100.) ^ "\n" ^ "\n" ^
    Printf.sprintf "%i parties avaient déjà était jouées, soit %.2f%% des parties" !parties_deja_jouees (((float_of_int !parties_deja_jouees) /. (float_of_int n)) *. 100.) ^ "\n" ^ repet ^ "\n" ^
    Printf.sprintf "Temps d'exécution : %s" (format_time temps) ^ "\n" ^ 
    Printf.sprintf "Temps moyen d'exécution : %s" (format_time temps_moyen)
  in
  if not affichage then begin
    print_newline ()
  end;
  print_endline bilan;
  doc := !doc ^ bilan;
  let dossier_de_sortie = Printf.sprintf "/home/%s/%s/Résultats/" (Sys.getenv "USER") nom_du_projet in
  let fichier_sortie = open_out (Printf.sprintf "%s%s contre %s %i parties - %s.txt" dossier_de_sortie nom_j1 nom_j2 n (date () ^ " " ^ heure ())) in
  output_string fichier_sortie !doc;
  close_out fichier_sortie