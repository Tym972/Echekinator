open Libs.Plateau
open Libs.Strategie1
open Libs.Quiescence
open Config

let run_tpf0 plateau =
  let score = traitement_profondeur_0  evaluation plateau !trait_aux_blancs !dernier_coup (-99999) 99999 in
  print_int score;
  print_newline ()

let run_tpfq0 plateau = 
  let score = traitement_quiescent_profondeur_0 0 evaluation plateau !trait_aux_blancs !dernier_coup (-99999) 99999 in
  print_int score;
  print_newline ()

let main b1 b2 plateau =
  print_newline ();
  affiche plateau;
  if b1 then begin
    print_string "Traitement profondeur 0 : ";
    run_tpf0 plateau
  end;
  if b2 then begin
    print_string "Traitement quiescent profondeur 0 : ";
  run_tpfq0 plateau
  end

let () = main true true plateau