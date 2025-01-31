open Libs.Plateau
open Libs.Generateur
open Config

let print_list liste = 
  List.iter (fun a -> print_string (coord.(a) ^ " ")) liste

let main b1 b11 =
  print_newline ();
  affiche plateau;
  if b1 then begin
    let c = clouees plateau (index plateau (roi !trait_aux_blancs)) !trait_aux_blancs in
    print_string "Clouées : ";
    print_list c;
    print_newline ();
    let cv = coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque in
    print_string "Coups valides : ";
    affiche_liste cv plateau cv;
    print_newline ();
    if b11 then begin
      List.iter (fun coup -> print_endline (string_of_bool (est_valide plateau coup !trait_aux_blancs)); affiche plateau) cv;
      affiche plateau
    end
  end

let () = main true false