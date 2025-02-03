open Libs.Plateau
open Config

let main plateau =
  let _ = trait_aux_blancs in
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau

let () = main plateau


  (*let pseudo_e2 = plateau.(!clouage_roi_blanc_1) in
  if not (!rois_centrees && (pseudo_e2 = (-1) || plateau.(!clouage_roi_blanc_2) = (-2))) then begin
    if prb && Array.for_all (fun case -> plateau.(case) = 0) (chemin_blanc_pr.(0)) && Array.for_all (fun case -> plateau.(case) = 0) (chemin_blanc_pr.(1)) then begin*)