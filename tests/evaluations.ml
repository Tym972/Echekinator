open Config
open Libs.Board
open Libs.Generator
open Libs.To_algebraic
let main =
  print_newline ();
  let position_roi = index_tableau plateau (roi !trait_aux_blancs) in
  let roi_en_echec = menacee plateau position_roi !trait_aux_blancs in
  affiche plateau;
  let word =
    "Note : " ^ (string_of_int (evaluation plateau !trait_aux_blancs position_roi roi_en_echec (-99999) 99999)) ^ "\n" ^ san_of_move_list !releve_coups echiquier !dernier_coup !droit_au_roque
  in print_endline word

let () = main