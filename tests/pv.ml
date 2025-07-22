open Libs.Board
open Libs.Generator
open Libs.Zobrist
open Strategie2
open Config

(*Implémentation d'un algorithme de recherche minimax avec élagage alpha-bêta et negamax*)
let rec negalphabeta_pv board white_to_move last_move castling_right board_record depth profondeur_initiale alpha beta evaluation =
  let best_score = ref (-99999) in
  let best_moves = ref [] in
  if repetition board_record 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else if depth = 0 then begin
    best_score := traitement_profondeur_0 evaluation board white_to_move last_move alpha beta
  end
  else begin
    let cp = ref (tab_tri.(depth - 1) board white_to_move last_move castling_right board_record evaluation negalphabeta)
    in if !cp = [] then begin
      if threatened board (index_array board (king white_to_move)) white_to_move then begin
        best_score := (profondeur_initiale - (depth + 99999))
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
        make board coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup castling_right in
        let nouveau_releve = adapte_releve board coup depth white_to_move nouveau_droit_au_roque board_record
        in let note, pv = negalphabeta_pv board (not white_to_move) coup nouveau_droit_au_roque nouveau_releve (depth - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
        in let score = - note in
        if score > !best_score then begin
          best_score := score;
          best_moves := coup :: pv;
          alpha0 := max !alpha0 !best_score;
          if !alpha0 >= beta then begin
            b := false
          end
        end;
        unmake board coup
      done
    end
  end;
  !best_score, !best_moves

let negalphabetime_pv board white_to_move last_move castling_right board_record depth profondeur_initiale evaluation =
  let t = Sys.time () in
  let fx = negalphabeta_pv board white_to_move last_move castling_right board_record depth profondeur_initiale (-99999) 99999 evaluation in
  fx, (Sys.time () -. t)

let runnegalphabeta_pv b1 b2 board =
  if b1 then begin
    print_board board
  end;
  let (a,b),c = negalphabetime_pv board !white_to_move !last_move !castling_right !board_record depth depth evaluation in
  if b2 then begin
    print_endline ("Matériel : " ^ (string_of_float ((float_of_int a)/. 1000.)));
    print_endline ("Temps : " ^ (string_of_float c))
  end; 
  affiche_liste b board (legal_moves board !white_to_move !last_move !castling_right);
  print_newline ()

let main b1 board =
  print_endline ("\nProfondeur " ^ (string_of_int depth));
  print_board board;
  if b1 then begin
    print_endline "Negalphabeta";
    runnegalphabeta_pv false true (Array.copy board);
  end
let () = main false board

(*let bulle liste elt = 
  let rec supprime liste elt = match liste with
    |[] -> []
    |h::t -> if h = elt then t else h :: supprime t elt
  in elt :: supprime liste elt*)