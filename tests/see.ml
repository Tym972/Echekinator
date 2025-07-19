open Config
open Libs.Board
open Libs.Move_ordering

(*let depart = Hashtbl.find dicocoord "d8"*)

let arrivee = Hashtbl.find hash_coord "d5"

(*let coup = Classique {piece = plateau.(depart); depart; arrivee; prise = plateau.(arrivee)}*)

let main =
  print_board plateau;
  print_endline ("SEE : " ^ string_of_int (see plateau arrivee !trait_aux_blancs))

let () = main