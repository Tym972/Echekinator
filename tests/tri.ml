open Config
open Libs.Plateau
open Libs.Generateur
open Libs.Evaluations
open Libs.Zobrist
open Strategie2
open Libs.Traduction2
open Libs.Traduction3
open Libs.Quiescence

let rec affiche_liste liste plateau coups_valides_joueur = match liste with
  |(x, coup) :: t ->
    print_endline ("Score : " ^ (string_of_float ((float_of_int x)/. 1000.)) ^ " " ^ (algebric_of_move coup plateau coups_valides_joueur) ^ " ");
    affiche_liste t plateau coups_valides_joueur
  |_ -> ()

let tri_algo_1 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        joue plateau coup;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let x, _ = algo plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau) profondeur profondeur (-99999) 99999 evaluation in
        dejoue plateau coup;
        (- x, coup) :: association liste_coup
      end
  in tri_fusion (association (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque))

let tri_algo_2 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        joue plateau coup;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau in
        let x, _ = algo plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve profondeur profondeur (-99999) 99999 evaluation (zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque) in
        dejoue plateau coup;
        (- x, coup) :: association liste_coup
      end
  in tri_fusion (association (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque))

let tri_algo3 plateau trait_aux_blancs dernier_coup droit_au_roque =
let rec association liste_coups =
  match liste_coups with
  |[] -> []
  |Classique {piece; depart; arrivee; prise} :: t ->
    joue plateau (Classique {piece; depart; arrivee; prise});
    let note = see plateau arrivee trait_aux_blancs in
    dejoue plateau (Classique {piece; depart; arrivee; prise});
    ((tabvalue.(abs prise) - note), Classique {piece; depart; arrivee; prise}) :: association t
  |Enpassant {depart; arrivee} :: t ->
    joue plateau (Enpassant {depart; arrivee});
    let note = see plateau arrivee trait_aux_blancs in
    dejoue plateau (Enpassant {depart; arrivee});
    ((tabvalue.(1) - note), Enpassant {depart; arrivee}) :: association t
  |Promotion {depart; arrivee; promotion; prise} :: t ->
    joue plateau (Promotion {depart; arrivee; promotion; prise});
    let note = see plateau arrivee trait_aux_blancs in
    dejoue plateau (Promotion {depart; arrivee; promotion; prise});
    ((tabvalue.(abs promotion) + tabvalue.(abs prise) - note), Promotion {depart; arrivee; promotion; prise}) :: association t
  |h :: t -> (0, h) :: association t
  in List.map (fun (note, coup) -> (100 * note, coup) ) (tri_fusion (association (detecte_extension (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque))))

let tri_negalphabeta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  tri_algo_1 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabeta

let tri_negalphabeta_quiescent plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  tri_algo_1 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabeta_quiescent

let tri_negalphabeta_total plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  tri_algo_2 plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation negalphabeta_total

let do_tri_negalphabeta () =
  print_newline ();
  print_endline "Tri negalphabeta";
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau (profondeur - 1) evaluation) plateau coups_valides_joueur;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_negalphabeta_quiescent () =
  print_newline ();
  print_endline "Tri negalphabeta quiescent";
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta_quiescent plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau (profondeur - 1) evaluation) plateau coups_valides_joueur;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_negalphabeta_total () =
  print_newline ();
  print_endline "Tri negalphabeta total";
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta_total plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau (profondeur - 1) evaluation) plateau coups_valides_joueur;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_see () =
  print_newline ();
  print_endline "Tri static exchange variation";
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_algo3 plateau !trait_aux_blancs !dernier_coup !droit_au_roque) plateau coups_valides_joueur;
  print_endline (string_of_float (Sys.time () -. t))

let rec auxmvvlva liste = match liste with
  |[] -> []
  |h :: t -> (0, h) :: auxmvvlva t

let do_tri_mvvlva () =
  print_newline ();
  print_endline "Tri MVV-LVA";
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (auxmvvlva (tri_mvvlva plateau !trait_aux_blancs !dernier_coup !droit_au_roque releve_plateau evaluation 0)) plateau coups_valides_joueur;
  print_endline (string_of_float (Sys.time () -. t))

let main b1 b2 b3 b4 b5 =
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  if b1 then begin
    do_tri_negalphabeta ()
  end;
  if b2 then begin
    do_tri_negalphabeta_quiescent ()
  end;
  if b3 then begin
    do_tri_negalphabeta_total ()
  end;
  if b4 then begin
    do_tri_see ();
  end;
  if b5 then begin
    do_tri_mvvlva ()
  end

let () = main false false false false true