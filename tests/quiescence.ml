open Libs.Board
open Strategie2
open Libs.Quiescence
open Config

let run_tpf0 board =
  let score = traitement_profondeur_0  evaluation board !white_to_move !last_move (-99999) 99999 in
  print_int score;
  print_newline ()

let run_tpfq0 board = 
  let score = quiescence_treatment_depth_0 0 evaluation board !white_to_move !last_move (-99999) 99999 in
  print_int score;
  print_newline ()

let main b1 b2 board =
  print_newline ();
  print_board board;
  if b1 then begin
    print_string "Traitement profondeur 0 : ";
    run_tpf0 board
  end;
  if b2 then begin
    print_string "Traitement quiescent profondeur 0 : ";
  run_tpfq0 board
  end

let () = main true true board