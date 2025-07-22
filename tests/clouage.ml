open Libs.Board
open Libs.Generator
open Config

let print_list liste = 
  List.iter (fun a -> print_string (coord.(a) ^ " ")) liste

let main b1 b11 =
  print_newline ();
  print_board board;
  if b1 then begin
    let c = pinned_squares board (index_array board (king !white_to_move)) !white_to_move in
    print_string "Clouées : ";
    print_list c;
    print_newline ();
    let cv = legal_moves board !white_to_move !last_move !castling_right in
    print_string "Coups valides : ";
    affiche_liste cv board cv;
    print_newline ();
    if b11 then begin
      List.iter (fun coup -> print_endline (string_of_bool (is_legal board coup !white_to_move)); print_board board) cv;
      print_board board
    end
  end

let () = main true false