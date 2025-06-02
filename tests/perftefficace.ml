open Libs.Plateau
open Libs.Strategie1
open Libs.Traduction3
open Libs.Uci
open Config

let table_perft = ZobristHashtbl.create taille_transposition

let algoperftime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur releve_plateau =
  let t = Sys.time () in
  let fx = algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur true ((List.hd releve_plateau) lxor profondeur) table_perft in
  fx, (Sys.time () -. t)

let perft profondeur plateau =
  let nodes, time = algoperftime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur !releve_plateau in
  print_newline ();
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int profondeur));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft profondeur_perft plateau