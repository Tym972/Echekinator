open Libs.Board
open Libs.Generator
open Libs.Fen
open Config

let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur =
  if profondeur = 0 then begin
    1
  end
  else begin
    let cp = ref (legal_moves plateau trait_aux_blancs dernier_coup droit_au_roque) in
    let nodes = ref 0 in
    while !cp <> [] do
      let coup = List.hd !cp in
      make plateau coup;
      cp := List.tl !cp;
      nodes := !nodes + (algoperft plateau (not trait_aux_blancs) coup (modification_roque coup droit_au_roque) (profondeur - 1));
      unmake plateau coup
    done;
    !nodes 
  end

let algoperftime plateau trait_aux_blancs historique droit_au_roque profondeur =
  let t = Sys.time () in
  let fx = algoperft plateau trait_aux_blancs historique droit_au_roque profondeur in
  fx, (Sys.time () -. t)

let perft profondeur plateau =
  let nodes, time = algoperftime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur in
  print_newline ();
  print_board plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int profondeur));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft profondeur_perft plateau