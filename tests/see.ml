open Config
open Libs.Board
open Libs.Move_ordering

(*let depart = Hashtbl.find dicocoord "d8"*)

let depart = Hashtbl.find hash_coord "d3"
let attaquant = board.(depart)
let arrivee = Hashtbl.find hash_coord "e5"
let prise = board.(arrivee)
let coup1 = Normal {piece = attaquant; from = depart; to_ = arrivee; capture = prise}
let coup2 = Promotion {from = depart; to_ = arrivee; capture = prise; promotion = if prise > 0 then -5 else 5}
let coup = if not (abs board.(depart) = 1 && (arrivee < 8 || arrivee > 55)) then coup1 else coup2
let main =
  print_board board;
  (*print_endline ("SEE : " ^ string_of_int (see board arrivee !white_to_move));*)
  print_endline ("New SEE : " ^ string_of_int (new_see board coup));
  print_endline ("F : " ^ string_of_int (f board coup))

let () = main