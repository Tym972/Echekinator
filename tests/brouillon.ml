open Libs.Bitboards open Libs.Generator
open Libs.Board open Libs.Uci

let r = [|0L;second_row; 66L; 36L; 129L; 0xFF00L; 8L|]
let p = (ref (0, 0)) 

let g n =
  let time = ref (Sys.time ()) in
  let t1 = ref 0. in
  let t2 = ref 0. in
  let _ = ref 0L in
  let _ = ref 0 in
  for i = 0 to n do
    let _ = castling_infos.(i mod 2) in
    let _ = Int64.logand (generate_all_attacks r Int64.max_int 1 ) 0xb37bd32baf73cd5eL in ()
  done;
  t1 := Sys.time () -. !time;
  time := Sys.time ();
  for _ = 0 to n do
    p := 0,0;
    number_of_moves := 0;
    let _ = threatened position.board 10 in ()
  done;
  t2 := Sys.time () -. !time;
  print_endline (Printf.sprintf "t1 : %f \n t2 : %f" !t1 !t2)

let pieces =[|1; 2; 3; 4; 5; 6; -1; -2; -3; -4; -5; -6|]

let b = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L |]

let index_of_bitboard bitboard =
  let index = ref [] in
  if bitboard <> 0L then begin
    for i = 0 to 63 do
      if Int64.logand bitboard (Int64.shift_left 1L i) <> 0L then index := (63 - i) :: !index
    done
  end;
  !index



let bitboard_of_mailbox mailbox =
  let bitboard = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|]
  in for i = 63 downto 0 do
    let piece = mailbox.(i) in
    if piece > 0 then begin
      bitboard.(piece - 1) <- (Int64.logor) bitboard.(piece - 1) (Int64.shift_left 1L (63 - i))
    end
    else if piece < 0 then begin
      bitboard.(5 - piece) <- (Int64.logor) bitboard.(5 - piece) (Int64.shift_left 1L (63 - i))
    end
  done;
  bitboard

let million = 1000000

let () =
  let _ = bitboard_of_mailbox in
  if false then begin
    let aux bitboard =
      let n = (Array.length bitboard) in
      for i = 0 to n - 1 do
        b.(1) <- single_bitboards_tab.(i);
        b.(2) <- (Int64.logor 0L bitboard.(i));
        print_board (mailbox_of_bitboard b);
      done;
      print_endline (string_of_int n)
    in aux white_pawn_attacks_table
  end;
  if false then begin
    let bibi = (bishop_masks, bishop_blockers, Libs.Bitboards.bishop_moves, bishop_shifts, bishop_magics, bishop_table) in
    let roro = (rook_masks, rook_blockers, Libs.Bitboards.rook_moves, rook_shifts, rook_magics, rook_table) in
    let tab = [|bibi; roro|] in
    let aux (masks, blockers, moves, shifts, magics, table) =
      for square = 34 to 34 do
        b.(2) <- masks.(square);
        print_board (mailbox_of_bitboard b);
        b.(2) <- (blockers.(square).(8));
        print_board (mailbox_of_bitboard b);
        b.(2) <- (moves.(square).(8));
        print_board (mailbox_of_bitboard b);
        b.(2) <- table.(square).(index magics.(square) (blockers.(square).(8)) shifts.(square));
        print_board (mailbox_of_bitboard b)
      done;
    in aux tab.(1)
  end;
  if true then begin
    g (1 * million)
  end

(*position fen r3kb1r/ppp1qpp1/2np1n1p/1B2p3/3PP1b1/2N1BN2/PPP2PPP/R2QK2R b KQkq - 4 8 moves e5d4 e3d4 f6e4 c3d5 e7d7 d1e2 g4f5 b5d3 e8c8 e1c1 c6d4 f3d4 d8e8 d4f5 d7f5 h1e1 f5d5 d3e4 d5g5 c1b1 c8b8 e2f3 g5e7 e1e3 e7f6 e3b3 b7b6 e4c6 e8e7 f3f6 g7f6 c6d5 h8g8 b3d3 g8g5 d3d2 g5e5 f2f3 f8g7 h2h3 f6f5 c2c3 e5e1 a2a3 b6b5 b1c2 f7f6 f3f4 a7a6 d1e1 e7e1 d5f3 e1f1 d2d5 f1f2 c2b1 f2f1 b1c2 f1e1 b2b4 e1a1 d5f5 a1h1 f5d5 h1h2 c2b1 h2h1 d5d1 h1d1 f3d1 f6f5 d1c2 g7c3 c2f5 c7c5 b4c5 d6c5 b1c2 c3d4 f5d7 c5c4 d7c6 c4c3 c2d3 d4f6 f4f5 f6e5 d3c2 b8c7 c6d5 a6a5 h3h4 c7d6 d5f7 a5a4 f7e8 d6c5 h4h5 b5b4 a3b4 c5b4 e8f7 a4a3 f7e6 b4c5 e6f7 c5b4

3    4    5   0  1  2  0  1  2  3  4  5       8   - 5          = 3  
m-3  m-2  m-1 m0 m1 m2 m3 m4 m5 m6 m7 m8      ply - half_moves = last_ply

1    0    1   2  3  4  5  6  7  8  9  10      8   - 10         = -2  
m-3  m-2  m-1 m0 m1 m2 m3 m4 m5 m6 m7 m8      ply - half_moves = last_ply

if !index < ply - stack.(ply).half_moves then begin print_endline "AJAX"; print_endline (Printf.sprintf "%i" stack.(ply).half_moves); Array.iter (fun i -> print_string (string_of_int i.zobrist_position ^ " ")) stack; Array.iter (fun i -> print_string (string_of_int i ^ " ")) board_record end
position fen r2qk1nr/pp2ppbp/2np2p1/2p5/2P1P3/1PNB1Q1P/P2P1PP1/R1B1K2R b KQkq - 0 8 moves c6e5 f3g3 e5d3 g3d3 g8f6 e1g1 e8g8 c1b2 a8c8 a1d1 e7e5 c3d5 h7h6 d3c2 f6d5 c4d5 d8c7 f1e1 c8d8 d2d3 a7a5 a2a4 f8e8 b2c3 c7b6 c3b2 b6c7 b2c3 c7b6 c2d2 b6b3 d1b1 b3a4 b1b7 d8b8 b7c7 b8c8 c7b7 c8b8 b7c7 b8c8 c7c8 e8c8 c3a5 c8e8 e1d1 a4b5 a5c3 b5d7 d2e3 d7d8 e3g3 d8g5 g3g4 e8b8 c3a5 b8b2 a5c7 g5f6 g4g3 f6e7 c7a5 e7f6 a5c7 b2e2 d1b1 c5c4 d3c4 e2e4 g3d3 f6f5 b1f1 f5f4 c7d6 e4c4 d3b1 c4d4 b1b3 d4d2 d6c5 f4e4 b3b8 g8h7 d5d6 e4c6 c5b4 d2c2 b8a7 c6e8 a7e7 e8e7 d6e7 c2c8 f1d1 g7f6 d1d7 e5e4 d7c7 c8e8 c7d7 e8c8 b4c5 h7g8 c5d4 f6g5 d4e3 g5h4 e3h6 c8e8 h6f8 h4f6 g2g3 e8a8 g1h2 a8e8 h2h1 g8h7 h1g1 h7g8 g1g2 g8h7 g2h1 h7h8 h1g2 h8g8 g2g1 g8h7 g1h1
go nodes 20000

if tyland then begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt" in
  let ma_chaine =
    Printf.sprintf
      "fen : %s; node_counter : %i; best_score : %i; alpha : %i; beta : %i; depth : %i; hash_move : %s; killer1 : %s; killer2 : %s; bestmove : %s; explored %s\n"
      (Fen.fen position 0) node_counter.(0) !best_score alpha beta depth (Translation.uci_of_mouvement hash_move) (Translation.uci_of_mouvement ordering_tables.killer_moves.(2 * ply + 1)) killer2 (Translation.uci_of_mouvement !best_move) !explored
  in output_string fichier_sortie ma_chaine;
  close_out fichier_sortie
end;

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each tc=10+0.1 -rounds 8000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout file=/home/tym972/Pgn_fastchess.pgn -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each tc=60+0.6 -rounds 8000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout file=/home/tym972/Pgn_fastchess.pgn -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each nodes=20000 -rounds 8000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=-10 elo1=0 -pgnout file=/home/tym972/Pgn_fastchess.pgn -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn -log file=/home/tym972/Echekinator/Results/fastchess.log level=info engine=true
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 1   -each tc=600  option.Threads=16 option.Hash=512  -rounds 8000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=-10 elo1=0 -pgnout file=/home/tym972/Pgn_fastchess.pgn -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn -log file=/home/tym972/Echekinator/Results/fastchess.log level=info engine=true
fastchess -config file=config.json -recover

         (*let gives_check move position =
  let capture = ref 0 in
  make_light position.board move capture;
  let b = threatened position.board (position.king_positions.king_not_to_move) in
  unmake position.board move !capture;
  b

let quiescence_moves position =
  let moves, number_of_moves = legal_moves position in
  let quiescence_moves = Array.make 256 Null in
  let number_of_quiescence_moves = ref 0 in
  for i = 0 to !number_of_moves - 1 do
    let move = moves.(i) in
    if not (isquiet move position.board.(to_ move)) then begin
      quiescence_moves.(!number_of_quiescence_moves) <- move;
      incr number_of_quiescence_moves
    end
    else begin
      if gives_check move position then begin
        quiescence_moves.(!number_of_quiescence_moves) <- move;
        incr number_of_quiescence_moves
      end;
    end
  done;
  quiescence_moves, number_of_quiescence_moves*)

begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt" in
  let ma_chaine =
    Printf.sprintf
      "fen : %s; zobrist : %s; node_counter : %i; best_score : %i; alpha : %i; beta : %i; depth : %i; bestmove : %s; vrai zobrist : %s\n"
      (Fen.fen position 0) (Int64.to_string state.zobrist_position) node_counter.(0) !best_score alpha beta depth
      (Translation.uci_of_mouvement !best_move) (Int64.to_string (zobrist position ))
  in output_string fichier_sortie ma_chaine;
  close_out fichier_sortie
end;

begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt" in
  in output_string fichier_sortie (!display ^"\n");
  close_out fichier_sortie
end;



(*
let new_vector move =
  match move with
    |Normal {piece; from; to_; capture} -> begin
      if piece > 0 then begin
        if capture = 0 then begin
          board_vector.(12 * from + (piece - 1)) <- 0.;
          board_vector.(12 * to_ + (piece - 1)) <- 1.
        end
        else begin
          board_vector.(12 * from + (piece - 1)) <- 0.;
          board_vector.(12 * to_ + (piece - 1)) <- 1.;
          board_vector.(12 * to_ + (5 - capture)) <- 0.
        end
      end
      else begin
        if capture = 0 then begin
          board_vector.(12 * from + (5 - piece)) <- 0.;
          board_vector.(12 * to_ + (5 - piece)) <- 1.
        end
        else begin
          board_vector.(12 * from + (5 - piece)) <- 0.;
          board_vector.(12 * to_ + (5 - piece)) <- 1.;
          board_vector.(12 * to_ + (capture - 1)) <- 0.
        end
      end
    end
    |Castling {sort} -> begin
      match sort with
      |1 ->
        board_vector.(!zobrist_from_white_king) <- 0.;
        board_vector.(749) <- 1.;
        board_vector.(!zobrist_from_short_white_rook) <- 0.;
        board_vector.(735) <- 1.
      |2 ->
        board_vector.(!zobrist_from_white_king) <- 0.;
        board_vector.(701) <- 1.;
        board_vector.(!zobrist_from_long_white_rook) <- 0.;
        board_vector.(711) <- 1.
      |3 ->
        board_vector.(!zobrist_from_black_king) <- 0.;
        board_vector.(83) <- 1.;
        board_vector.(!zobrist_from_short_black_rook) <- 0.;
        board_vector.(69) <- 1.
      |_ ->
        board_vector.(!zobrist_from_black_king) <- 0.;
        board_vector.(35) <- 1.;
        board_vector.(!zobrist_from_long_black_rook) <- 0.;
        board_vector.(45) <- 1.
    end
    |Enpassant {from; to_} -> begin
      if from < 32 then begin
        board_vector.(12 * from) <- 0.;
        board_vector.(12 * to_) <- 1.;
        board_vector.(12 * (to_ + 8) + 6) <- 0.
      end
      else begin
        board_vector.(12 * from + 6) <- 0.;
        board_vector.(12 * to_ + 6) <- 1.;
        board_vector.(12 * (to_ - 8)) <- 0.
      end
    end
    |Promotion {from; to_; promotion; capture} -> begin
      if to_ < 8 then begin
        if capture = 0 then begin
          board_vector.(12 * from) <- 0.;
          board_vector.(12 * to_ + (promotion - 1)) <- 1.
        end
        else begin
          board_vector.(12 * from) <- 0.;
          board_vector.(12 * to_ + (promotion - 1)) <- 1.;
          board_vector.(12 * to_ + (5 - capture)) <- 0.
        end
      end
      else begin
        if capture = 0 then begin
          board_vector.(12 * from + 6) <- 0.;
          board_vector.(12 * to_ + (5 - promotion)) <- 0.
        end
        else begin
          board_vector.(12 * from + 6) <- 0.;
          board_vector.(12 * to_ + (5 - promotion)) <- 1.;
          board_vector.(12 * to_ + (capture - 1)) <- 0.
        end
      end
    end
    |Null -> ()

(*let f tb tn board =
  let material = ref 0 in
  let position = ref 0 in
  let white_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  let black_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i);
      white_pieces.(square) <- white_pieces.(square) + 1
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i);
      black_pieces.(- square) <- black_pieces.(- square) + 1
    end
  done;
  let only_white_king = white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
  in let only_black_king = black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0
  in let score_draw =
    let func () =
      let white_minor = white_pieces.(2) + white_pieces.(3) in
      let black_minor = black_pieces.(2) + black_pieces.(3) in
      white_minor < 3 && black_minor < 3 && begin
        (white_minor < 2 && black_minor < 2) || (*K vs K, K + Minor vs K + Minor*)
        ((white_pieces.(3) = 1 && (white_minor = 1 || black_minor > 0)) || (black_pieces.(3) = 1 && (black_minor = 1 || white_minor > 0))) || (*K + B + B vs K + B, K + B vs K + Minor, K + B vs K*)
        ((white_pieces.(2) = 2 && black_minor < 2) || black_pieces.(2) = 2 && white_minor < 2) (*K + N + N vs K + Minor, K + N + N vs K*)
      end
    in white_pieces.(1) = 0 && black_pieces.(1) = 0
      &&
      (only_white_king && only_black_king ||
      (white_pieces.(4) = 0 && black_pieces.(4) = 0 && white_pieces.(5) = 0 && black_pieces.(5) = 0 && func ()))
  in if score_draw then
    0, 0
  else
    !material, !position*)

(*let eval_materiel3 board (tb, tn) white_to_move =
  let material = ref 0 in
  let position = ref 0 in
  let white_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  let black_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i);
      white_pieces.(square) <- white_pieces.(square) + 1
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i);
      black_pieces.(- square) <- black_pieces.(- square) + 1
    end
  done;
  (*let only_white_king () = white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
  in let only_black_king () = black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0
  in let score_draw =
    let func () =
      let white_minor = white_pieces.(2) + white_pieces.(3) in
      let black_minor = black_pieces.(2) + black_pieces.(3) in
      white_minor < 3 && black_minor < 3 && begin
        (white_minor < 2 && black_minor < 2) || (*K vs K, K + Minor vs K + Minor*)
        ((white_pieces.(3) = 1 && (white_minor = 1 || black_minor > 0)) || (black_pieces.(3) = 1 && (black_minor = 1 || white_minor > 0))) || (*K + B + B vs K + B*)
        ((white_pieces.(2) = 2 && black_minor < 2) || black_pieces.(2) = 2 && white_minor < 2) (*K + N + N vs K + Minor, K + N + N vs K*)
      end
    in white_pieces.(1) = 0 && black_pieces.(1) = 0
      &&
      (only_white_king && only_black_king ||
      (white_pieces.(4) = 0 && black_pieces.(4) = 0 && white_pieces.(5) = 0 && black_pieces.(5) = 0 && func ()))*)
  zugzwang :=
    if white_to_move then
      white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
    else
      black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0;
  (*in let draw =
    if interior then

    else
      false*)
  (*if score_draw then
    0, 0
  else*)
    !material, !position*)
(*Draw Fide
- K vs K
- K + Minor vs K
- K + B vs K + B (de même couleur)

Draw (si les joueurs sont pas d'énormes abrutis)
- K + Minor vs K + Minor
- K + Minor vs K + N + N
- K + Minor vs K + B + N
- K vs K + N + N
- K + B vs K + B + B

Pas Nulle































if nb_pions > 0 || nb_tours > 0 || nb_dames > 0 then
  false  (* mat possible *)
else if nb_cavaliers > 1 || nb_fous > 1 then
  false  (* mat possible *)
else if nb_fous = 1 && nb_cavaliers = 1 then
  false  (* mat possible *)
else if nb_cavaliers = 1 then
  (* Roi + cavalier contre roi seul *)
  nb_fous = 0
else if nb_fous = 1 then
  (* Roi + fou contre roi seul *)
  nb_cavaliers = 0
else if nb_fous = 2 then
  (* Roi + fou contre roi + fou : vérifier couleur *)
  (* ex: fou_case_white, fou_case_black *)
  couleur_case fou_case_white = couleur_case fou_case_black
else
  true  (* roi contre roi *)*)


(*let coup = tolerance plateau "0-0-0" !trait_aux_blancs (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque)
let bitboard = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|]
let main bitboard =
  affiche (mailbox_of_bitboard bitboard);
  joue plateau coup;
  affiche plateau;
  update_bitboard coup bitboard;
  affiche (mailbox_of_bitboard bitboard)

let () = main bitboard*)

(*

let tab_mvvlva = [|
  6; 12; 18; 24; 30;
  5; 11; 17; 23; 29;
  4; 10; 16; 22; 28;
  3;  9; 15; 21; 27;
  2;  8; 14; 20; 26;
  1;  7; 13; 19; 25
  |]

let bmvvlva2 liste =
  let rec association liste_coups = match liste_coups with
    |[] -> []
    |Classique {piece; depart; arrivee; prise} :: t when prise <> 0 ->
      (tab_mvvlva.(5 * (abs piece - 1) + (abs prise - 1)), Classique {piece; depart; arrivee; prise}) :: association t
    |Promotion {depart; arrivee; prise; promotion} :: t ->
      (tab_mvvlva.((if prise <> 0 then (abs prise - 1) else 0)) + tabvalue.(abs promotion), Promotion {depart; arrivee; prise; promotion}) :: association t
    |h :: t -> (0, h) :: association t
  in List.map snd (tri_fusion (association liste))



let main plateau =
  if false then begin
    let b = ref true in
    for i = 0 to 959 do
      fischer i plateau releve_plateau;
      if not (est_960 chaine_fen) then begin
        b := false;
        print_endline "Ce n'est pas une position 960";
        affiche plateau
      end
    done;
    print_endline (if !b then  "Fonction correcte" else "Les problèmes")
  end
  else if true then begin
    affiche plateau;
    print_endline (uci_of_san liste_coup !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart)
  end
  else begin
    affiche plateau;
    print_endline (san_of_uci (uci_of_san liste_coup !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart) !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart)
  end*)


  (* fastchess   -openings order=random file=/home/tym972/openbench-books/UHO_Lichess_4852_v1.epd   -engine name=new cmd=/home/tym972/Echekinator/_build/install/default/bin/main_new   -engine name=base cmd=/home/tym972/Echekinator/_build/install/default/bin/main   -concurrency 4   -each tc=8+0.08 -rounds 4000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout file=/home/tym972/Pgn_fastchess.pgn notation=san seldepth=true -pgnout notation=san file=/home/tym972/Echekinator/Résultats/Pgn_fastchess.pgn
      
      if hash_node_type = All && node_type <> All && hash_depth = profondeur && !best_score > hash_value then begin
        print_newline ();
        print_endline (Printf.sprintf "Défaillance All profondeur %i, alpha : %i, beta : %i" profondeur alpha beta);
        print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] []);
        print_endline (Printf.sprintf "best_score : %i et hash_value : %i" !best_score hash_value);
        if node_type = Pv then print_endline "Pv" else print_endline "Cut";
        (*affiche plateau*)
      end
      else if hash_node_type = Cut && node_type <> Cut && hash_depth = profondeur && !best_score < hash_value then begin
        print_newline ();
        print_endline (Printf.sprintf "Défaillance Cut profondeur %i, alpha ; %i, beta : %i" profondeur alpha beta);
        print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] []);
        print_endline (Printf.sprintf "best_score : %i et hash_value : %i" !best_score hash_value);
        if node_type = Pv then print_endline "Pv" else print_endline "All";
        (*affiche plateau*)
      end;
  
  affiche position_de_depart;
  print_endline (coord.(!depart_roi_blanc) ^ " " ^ coord.(!depart_tour_blanche_pr) ^ " " ^ coord.(!depart_tour_blanche_gr));
  print_endline (coord.(!depart_roi_noir) ^ " " ^ coord.(!depart_tour_noire_pr) ^ " " ^ coord.(!depart_tour_noire_gr));
  print_endline (string_of_bool !roi_blanc_clouable ^ " " ^ string_of_bool !roi_noir_clouable);
  print_endline (coord.(!clouage_roi_blanc_1) ^ " " ^ coord.(!clouage_roi_blanc_2) ^ " " ^ coord.(!clouage_roi_noir_1) ^ " " ^ coord.(!clouage_roi_noir_2));
  print_endline ((string_of_int !longueur_chemin_roi_blanc_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_blanc_gr));
  print_endline ((string_of_int !longueur_chemin_roi_noir_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_noir_gr));
  print_endline (string_of_bool !tour_blanche_gr_en_a ^ " " ^ string_of_bool !tour_blanche_gr_en_b ^ " " ^ string_of_bool !tour_blanche_pr_en_h);
  print_endline (string_of_bool !tour_noire_gr_en_a ^ " " ^ string_of_bool !tour_noire_gr_en_b ^ " " ^ string_of_bool !tour_noire_pr_en_h);
  print_newline ();
  List.iter (fun tab -> Array.iter (fun c -> if c <> 0 then print_string (coord.(c) ^ " ")) tab; print_newline ()) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr]; print_newline ();
  List.iter (fun list -> List.iter (fun c -> print_string (coord.(c) ^ " ")) list; print_newline ()) [!vides_blanc_pr; !vides_blanc_gr; !vides_noir_pr; !vides_noir_gr]
*)*)