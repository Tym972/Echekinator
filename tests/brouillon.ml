(*
https://lichess.org/game/Hnn0dIrC/white#35
position fen 1k6/8/P4R2/8/8/8/7K/8 w - - 97 123

position fen r3kb1r/ppp1qpp1/2np1n1p/1B2p3/3PP1b1/2N1BN2/PPP2PPP/R2QK2R b KQkq - 4 8 moves e5d4 e3d4 f6e4 c3d5 e7d7 d1e2 g4f5 b5d3 e8c8 e1c1 c6d4 f3d4 d8e8 d4f5 d7f5 h1e1 f5d5 d3e4 d5g5 c1b1 c8b8 e2f3 g5e7 e1e3 e7f6 e3b3 b7b6 e4c6 e8e7 f3f6 g7f6 c6d5 h8g8 b3d3 g8g5 d3d2 g5e5 f2f3 f8g7 h2h3 f6f5 c2c3 e5e1 a2a3 b6b5 b1c2 f7f6 f3f4 a7a6 d1e1 e7e1 d5f3 e1f1 d2d5 f1f2 c2b1 f2f1 b1c2 f1e1 b2b4 e1a1 d5f5 a1h1 f5d5 h1h2 c2b1 h2h1 d5d1 h1d1 f3d1 f6f5 d1c2 g7c3 c2f5 c7c5 b4c5 d6c5 b1c2 c3d4 f5d7 c5c4 d7c6 c4c3 c2d3 d4f6 f4f5 f6e5 d3c2 b8c7 c6d5 a6a5 h3h4 c7d6 d5f7 a5a4 f7e8 d6c5 h4h5 b5b4 a3b4 c5b4 e8f7 a4a3 f7e6 b4c5 e6f7 c5b4

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

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 8  -each nodes=20000 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=-10 elo1=0 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 8  -each tc=10+0.1 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 8  -each tc=60+0.6 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 8  -each nodes=10000 -rounds 8000 -games 1 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn

fastchess -config file=config.json -recover
-log file=/home/tym972/Echekinator/Results/fastchess.log level=info engine=true

let pqst position =
  let mg_score = ref 0 in
  let pieces_bitboard = position.pieces in
  let [@inline] add_score mg sign =
    mg_score := !mg_score + sign * mg;
  in for side = 0 to 1 do
    let sign = (-2 * side + 1) in
    for piece = pawn to king do
      let bitboard = ref (pieces_bitboard.(piece + 6 * side)) in
      while !bitboard <> 0L do
        let from, other_pieces_bitboard = pop_lsb !bitboard in
        bitboard := other_pieces_bitboard;
        let index = (from lxor (56 * side)) + 64 * piece in
        add_score mg_pst.(index) sign;
      done
    done
  done;
  (- 2 * position.white_to_move + 1) * !mg_score



let get_output state =
  let side_accumulator = state.side_accumulator in
  let not_side_accumulator = state.not_side_accumulator in
  let output = ref 0 in
  for i = 0 to n - 1 do
    output := !output
      + screlu side_accumulator.(i) * output_weights.(i)
      + screlu not_side_accumulator.(i) * output_weights.(n + i)
  done;
  ((!output / 255 + !output_bias) * 400) / (255 * 64)

let naive_nnue position =
  get_accumulator position;
  get_output position.state_array.(position.game_ply)*)