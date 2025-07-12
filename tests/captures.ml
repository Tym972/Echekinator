open Config
open Libs.Board
open Libs.Generator

let main =
  print_newline ();
  affiche plateau;
  let coups_valides_joueur = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
  print_string "Coups : "; affiche_liste coups_valides_joueur plateau coups_valides_joueur; print_newline ();
  print_string "Prises : "; affiche_liste (captures plateau !trait_aux_blancs !dernier_coup) plateau coups_valides_joueur; print_newline ()

let () = main