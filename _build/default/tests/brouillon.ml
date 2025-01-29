open Libs.Plateau
open Config

let main plateau =
  let _ = trait_aux_blancs in
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau

let () = main plateau