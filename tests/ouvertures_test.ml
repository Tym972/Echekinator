
(*Module vérifiant que les ouvertures de la base de donnée sont correctes*)

open Libs.Board
open Libs.Generator
open Ouvertures
open Libs.Of_algebraic
open Config

let ouvertures = translate chess_openings_exhaustif

let verif =
  let b = ref true in
  let trait_aux_blancs = ref true in
  let dernier_coup = ref Null in
  let droit_au_roque = ref (true, true, true, true) in
  let rec fonc1 liste plateau = match liste with
    |[] -> ()
    |h :: t when !b ->
      if List.mem h (legal_moves plateau !trait_aux_blancs !dernier_coup !droit_au_roque) then begin
        make_move_1 plateau h trait_aux_blancs dernier_coup droit_au_roque;
        print_board plateau;
        fonc1 t plateau
      end
      else begin
        b := false
      end
    |_ -> ()
  in let rec fonc2 liste = match liste with
    |[] -> ()
    |h::t when !b ->
      let plateau = Array.copy chessboard in 
      trait_aux_blancs := true; 
      dernier_coup := Null;
      droit_au_roque := (true, true, true, true);
      fonc1 h plateau;
      if not !b then begin
        affiche_liste h chessboard [];
        print_endline "Erreur"
      end;
      fonc2 t
    |_ -> ()
  in fonc2 ouvertures;
  if !b then begin
    print_endline "Toutes les ouvertures sont correctes"
  end
  else begin
    print_endline "Erreur"
  end

let main () = verif

let () = main ()