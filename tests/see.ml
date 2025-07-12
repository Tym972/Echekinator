open Config
open Libs.Board
open Libs.Move_ordering

(*let depart = Hashtbl.find dicocoord "d8"*)

let arrivee = Hashtbl.find dicocoord "a6"

(*let coup = Classique {piece = plateau.(depart); depart; arrivee; prise = plateau.(arrivee)}*)

let main =
  affiche plateau;
  print_endline ("SEE : " ^ string_of_int (see plateau arrivee !trait_aux_blancs))

let () = main