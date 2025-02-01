open Libs.Plateau
open Libs.Generateur
open Libs.Strategie1
open Libs.Zobrist
open Config

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax*)
let rec negalphabeta_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation =
  let best_score = ref (-99999) in
  let best_moves = ref [] in
  if profondeur = 0 then begin
    best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
  end
  else begin
    let cp = ref (coups_joueur plateau profondeur trait_aux_blancs dernier_coup droit_au_roque releve_plateau evaluation negalphabeta)
    in if !cp = [] then begin
      if menacee plateau (index plateau (roi trait_aux_blancs)) trait_aux_blancs then begin
        best_score := (profondeur_initiale - (profondeur + 99999))
      end 
      else begin
        best_score := 0
      end
    end
    else begin
      let b = ref true in
      let alpha0 = ref alpha in
      while (!b && !cp <> []) do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_releve =
          if est_irremediable coup then begin
            if profondeur < 8 then begin
              []
            end
            else begin
              [zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque]
            end
          end
          else if List.length releve_plateau + profondeur < 8 then begin
            []
          end
          else begin 
            ((zobrist plateau (not trait_aux_blancs) coup nouveau_droit_au_roque) :: releve_plateau)
          end
        in let note, pv = negalphabeta_pv plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
          in let score = - note 
        in if score > !best_score then begin
          best_score := score;
          best_moves := coup :: pv;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        dejoue plateau coup
      done
    end
  end;
  !best_score, !best_moves

let negalphabetime_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_pv plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let runnegalphabeta_pv b1 b2 plateau =
  if b1 then begin
    affiche plateau
  end;
  let (a,b),c = negalphabetime_pv plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_plateau profondeur profondeur evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_float ((float_of_int a)/. 1000.)));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  affiche_liste b plateau (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque);
  print_newline ()

let main b1 plateau =
  print_endline ("\nProfondeur " ^ (string_of_int profondeur));
  affiche plateau;
  if b1 then begin
    print_endline "Negalphabeta";
    runnegalphabeta_pv false true (Array.copy plateau);
  end

let () = main true plateau

(*let bulle liste elt = 
  let rec supprime liste elt = match liste with
    |[] -> []
    |h::t -> if h = elt then t else h :: supprime t elt
  in elt :: supprime liste elt*)