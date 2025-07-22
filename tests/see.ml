open Config
open Libs.Board
open Libs.Move_ordering

(*let depart = Hashtbl.find dicocoord "d8"*)

let arrivee = Hashtbl.find hash_coord "d5"

(*let coup = Classique {piece = board.(depart); depart; arrivee; prise = board.(arrivee)}*)

let main =
  print_board board;
  print_endline ("SEE : " ^ string_of_int (see board arrivee !white_to_move))

let () = main