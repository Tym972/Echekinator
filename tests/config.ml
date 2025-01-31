open Libs.Plateau
open Libs.Generateur
open Libs.Zobrist
open Libs.Config
open Libs.Evaluations
open Libs.Traduction1
open Libs.Traduction2
open Libs.Traduction3
open Positions

let releve_coups = ref []

let dernier_coup = ref Aucun

let droit_au_roque = ref (true, true, true, true)

let plateau = Array.copy echiquier

let releve_plateau = ref [zobrist plateau true !dernier_coup !droit_au_roque]

let chaine_fen = kiwipete

let liste_coup = ""

let trait_aux_blancs = ref true

let () = 
  position_of_fen chaine_fen plateau trait_aux_blancs dernier_coup droit_au_roque releve_coups releve_plateau;
  joue_liste (algebric_to_type_mouvement liste_coup !trait_aux_blancs !dernier_coup !droit_au_roque echiquier) plateau dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs

let evaluation = evalue_ouverture

let profondeur = 8

let profondeur_perft = 5

let coups_valides_joueur = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque

let rec affiche_liste liste plateau coups_valides_joueur = match liste with
  |h :: t ->
    let coup = algebric_of_mouvement h plateau coups_valides_joueur in
    if coup <> "" then begin
      print_string (coup ^ " ")
    end
    else begin
      print_string ("erreur ")
    end;
    affiche_liste t plateau coups_valides_joueur
  |_ -> ()