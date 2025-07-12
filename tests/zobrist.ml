open Libs.Board
open Libs.Generator
open Libs.Of_algebraic
open Libs.Zobrist

let plateau_apres = Array.copy chessboard

let plateau_avant = Array.copy plateau_apres

let trait_aux_blancs = ref true

let dernier_coup = ref Null

let droit_au_roque = ref (true, true, true, true)

let releve_coups = ref []

let releve_plateau = ref [zobrist plateau_apres true !dernier_coup !droit_au_roque]

let liste_coup = "1 e4 e5"

let liste_traduite = move_list_of_san liste_coup !trait_aux_blancs !dernier_coup !droit_au_roque plateau_apres

let () =
  make_list liste_traduite plateau_apres (ref !dernier_coup) (ref !releve_coups) (ref !releve_plateau) (ref !droit_au_roque) (ref !trait_aux_blancs);
  make_list liste_traduite plateau_avant dernier_coup releve_coups releve_plateau droit_au_roque trait_aux_blancs

let coup = try List.hd !releve_coups with _ -> Null

let k = 
  unmake plateau_avant coup;
  zobrist plateau_avant !trait_aux_blancs (try (List.hd (List.tl !releve_coups)) with _ -> Null) !droit_au_roque

let droit_au_roque = roques_possibles !releve_coups

let i = zobrist plateau_apres (not !trait_aux_blancs) !dernier_coup droit_au_roque

let j = new_zobrist coup !dernier_coup k droit_au_roque (modification_roque coup droit_au_roque) plateau_avant (*Pas sûr mais blc*)

let main  b2 =
  if b2 then begin
    print_board plateau_apres;
    print_board plateau_avant;
  end;
  print_endline ("Zobrist : " ^ string_of_int i);
  prerr_endline ("Nouveau zobrist : " ^ string_of_int j)

let () = main true