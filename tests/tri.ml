open Config
open Libs.Board
open Libs.Generator
open Libs.Evaluation
open Libs.Zobrist
open Strategie2
open To_algebraic
open Libs.Fen
open Libs.Move_ordering
open Libs.Quiescence

let rec affiche_liste liste board player_legal_moves = match liste with
  |(x, coup) :: t ->
    print_endline ("Score : " ^ (string_of_float ((float_of_int x)/. 1000.)) ^ " " ^ (algebraic_of_move coup board player_legal_moves) ^ " ");
    affiche_liste t board player_legal_moves
  |_ -> ()

let tri_algo_1 board white_to_move last_move castling_right board_record depth evaluation algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        make board coup;
        let nouveau_droit_au_roque = modification_roque coup castling_right in
        let x, _ = algo board (not white_to_move) coup nouveau_droit_au_roque (adapte_releve board coup depth white_to_move nouveau_droit_au_roque board_record) depth depth (-99999) 99999 evaluation in
        unmake board coup;
        (- x, coup) :: association liste_coup
      end
  in merge_sort (association (legal_moves board white_to_move last_move castling_right))

let tri_algo_2 board white_to_move last_move castling_right board_record depth evaluation algo =
  let rec association liste_coups =
    match liste_coups with
    |[] -> []
    |coup :: liste_coup ->
      begin
        make board coup;
        let nouveau_droit_au_roque = modification_roque coup castling_right in
        let nouveau_releve = adapte_releve board coup depth white_to_move nouveau_droit_au_roque board_record in
        let x, _ = algo board (not white_to_move) coup nouveau_droit_au_roque nouveau_releve depth depth (-99999) 99999 evaluation (zobrist board (not white_to_move) coup nouveau_droit_au_roque) in
        unmake board coup;
        (- x, coup) :: association liste_coup
      end
  in merge_sort (association (legal_moves board white_to_move last_move castling_right))

let tri_algo3 board white_to_move last_move castling_right =
let rec association liste_coups =
  match liste_coups with
  |[] -> []
  |Normal {piece; from; to_; capture} :: t ->
    make board (Normal {piece; from; to_; capture});
    let note = see board to_ white_to_move in
    unmake board (Normal {piece; from; to_; capture});
    ((tabvalue.(abs capture) - note), Normal {piece; from; to_; capture}) :: association t
  |Enpassant {from; to_} :: t ->
    make board (Enpassant {from; to_});
    let note = see board to_ white_to_move in
    unmake board (Enpassant {from; to_});
    ((tabvalue.(1) - note), Enpassant {from; to_}) :: association t
  |Promotion {from; to_; promotion; capture} :: t ->
    make board (Promotion {from; to_; promotion; capture});
    let note = see board to_ white_to_move in
    unmake board (Promotion {from; to_; promotion; capture});
    ((tabvalue.(abs promotion) + tabvalue.(abs capture) - note), Promotion {from; to_; promotion; capture}) :: association t
  |h :: t -> (0, h) :: association t
  in List.map (fun (note, coup) -> (100 * note, coup) ) (merge_sort (association (detecte_extension (legal_moves board white_to_move last_move castling_right))))

let tri_negalphabeta board white_to_move last_move castling_right board_record depth evaluation =
  tri_algo_1 board white_to_move last_move castling_right board_record depth evaluation negalphabeta

let tri_negalphabeta_quiescent board white_to_move last_move castling_right board_record depth evaluation =
  tri_algo_1 board white_to_move last_move castling_right board_record depth evaluation negalphabeta_quiescent

let tri_negalphabeta_total board white_to_move last_move castling_right board_record depth evaluation =
  tri_algo_2 board white_to_move last_move castling_right board_record depth evaluation negalphabeta_total

let do_tri_negalphabeta () =
  print_newline ();
  print_endline "Tri negalphabeta";
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta board !white_to_move !last_move !castling_right !board_record (depth - 1) evaluation) board player_legal_moves;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_negalphabeta_quiescent () =
  print_newline ();
  print_endline "Tri negalphabeta quiescent";
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta_quiescent board !white_to_move !last_move !castling_right !board_record (depth - 1) evaluation) board player_legal_moves;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_negalphabeta_total () =
  print_newline ();
  print_endline "Tri negalphabeta total";
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_negalphabeta_total board !white_to_move !last_move !castling_right !board_record (depth - 1) evaluation) board player_legal_moves;
  print_endline (string_of_float (Sys.time () -. t))

let do_tri_see () =
  print_newline ();
  print_endline "Tri static exchange variation";
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (tri_algo3 board !white_to_move !last_move !castling_right) board player_legal_moves;
  print_endline (string_of_float (Sys.time () -. t))

let rec auxmvvlva liste = match liste with
  |[] -> []
  |h :: t -> (0, h) :: auxmvvlva t

let do_tri_mvvlva () =
  print_newline ();
  print_endline "Tri MVV-LVA";
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !move_record !board_record);
  print_newline ();
  let t = Sys.time () in
  affiche_liste (auxmvvlva (tri_mvvlva board !white_to_move !last_move !castling_right board_record evaluation 0)) board player_legal_moves;
  print_endline (string_of_float (Sys.time () -. t))

let main b1 b2 b3 b4 b5 =
  print_endline ("\nProfondeur " ^ (string_of_int depth));
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