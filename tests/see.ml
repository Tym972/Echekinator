open Config
open Libs.Plateau
open Libs.Quiescence

(*let depart = Hashtbl.find dicocoord "d8"*)

let arrivee = Hashtbl.find dicocoord "d2"

(*let coup = Classique {piece = plateau.(depart); depart; arrivee; prise = plateau.(arrivee)}*)

let main =
  affiche plateau;
  print_endline ("SEE : " ^ string_of_int (see plateau arrivee !trait_aux_blancs))

let () = main