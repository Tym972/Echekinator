open Config
open Libs.Plateau
open Libs.Generateur
open Libs.Traduction2
let main =
  print_newline ();
  let position_roi = index_tableau plateau (roi !trait_aux_blancs) in
  let roi_en_echec = menacee plateau position_roi !trait_aux_blancs in
  affiche plateau;
  let word =
    "Note : " ^ (string_of_int (evaluation plateau !trait_aux_blancs position_roi roi_en_echec (-99999) 99999)) ^ "\n" ^ type_mouvement_to_algebric !releve_coups echiquier !dernier_coup !droit_au_roque
  in print_endline word

let () = main