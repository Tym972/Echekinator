open Libs.Plateau
open Libs.Generateur
open Libs.Zobrist
open Libs.Config
open Libs.Evaluations
open Libs.Traduction1
open Libs.Traduction2
open Libs.Traduction3
open Positions

let plateau = Array.copy echiquier
let trait_aux_blancs = ref true
let dernier_coup = ref Aucun
let droit_au_roque = ref (true, true, true, true)
let releve_coups = ref []
let releve_plateau = ref [zobrist plateau true !dernier_coup !droit_au_roque]

let position_de_depart = Array.copy echiquier
let trait_aux_blancs_initial = ref true
let dernier_coup_initial = ref Aucun
let droit_au_roque_initial = ref (true, true, true, true)
let releve_coups_initial = ref []
let releve_plateau_initial = ref [zobrist plateau true !dernier_coup !droit_au_roque]

let chaine_fen = discorde_25

let liste_coup = discordieux_25

let () = 
  position_of_fen chaine_fen position_de_depart trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial releve_coups_initial releve_plateau_initial;
  reinitialise plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau position_de_depart !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial !releve_coups_initial !releve_plateau_initial;
  joue_liste (move_list_of_san liste_coup !trait_aux_blancs !dernier_coup !droit_au_roque plateau) plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs

let evaluation = eval1_q

let profondeur = 8
let profondeur_perft = 6
let nombre_de_coups = 1

let coups_valides_joueur = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque

let rec affiche_liste liste plateau coups_valides_joueur = match liste with
  |h :: t ->
    let coup = algebric_of_move h plateau coups_valides_joueur in
    if coup <> "" then begin
      print_string (coup ^ " ")
    end
    else begin
      print_string ("erreur ")
    end;
    affiche_liste t plateau coups_valides_joueur
  |_ -> ()

let uci_of_list liste_coups =
  let rec aux liste = match liste with
    |[] -> ""
    |h::t -> uci_of_mouvement h ^ " " ^ aux t
  in aux liste_coups

let uci_of_san algebric trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart =
  uci_of_list (move_list_of_san algebric trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart)

let san_of_uci uci trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart =
  san_of_move_list (List.rev (move_list_of_san uci trait_aux_blancs_initial dernier_coup_initial droit_au_roque_initial position_de_depart) @ (if trait_aux_blancs_initial then [] else [Aucun])) position_de_depart dernier_coup_initial droit_au_roque_initial