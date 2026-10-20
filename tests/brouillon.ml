open Libs.Bitboards
open Libs.Evaluation

(*print_endline (string_of_float base_ms ^ " " ^ string_of_float !soft_bound_ms ^ " " ^ string_of_float !hard_bound_ms);*)

let print_weights weights =
  let w = ref "[| " in
  for i = 0 to Array.length weights - 1 do
    w := !w ^ Printf.sprintf "%i; "  weights.(i)
  done;
  print_endline (!w ^ "|]")

let simple_eval () =
  for i = 0 to Array.length weights - 1 do
    weights.(i) <- 0
  done;
  for i = 0 to 63 do
    weights.(i + 64 * pawn) <- 100;
    weights.(i + 384 + 64 * pawn) <- 100;
    weights.(i + 64 * knight) <- 300;
    weights.(i + 384 + 64 * knight) <- 300;
    weights.(i + 64 * bishop) <- 300;
    weights.(i + 384 + 64 * bishop) <- 300;
    weights.(i + 64 * rook) <- 500;
    weights.(i + 384 + 64 * rook) <- 500;
    weights.(i + 64 * queen) <- 900;
    weights.(i + 384 + 64 * queen) <- 900
  done

let () =
  if true then simple_eval ();
  print_weights weights

(*
[Event "Fastchess Tournament"]
[Site "?"]
[Date "2026.10.18"]
[Round "99971"]
[White "new"]
[Black "base"]
[Result "1/2-1/2"]
[SetUp "1"]
[FEN "rnbqkbnr/1p3ppp/2ppp3/2P5/p2PP3/P7/1P3PPP/RNBQKBNR w KQkq - 0 6"]
[GameDuration "00:00:09"]
[GameStartTime "2026-10-18T22:54:22 +0200"]
[GameEndTime "2026-10-18T22:54:31 +0200"]
[PlyCount "237"]
[Termination "normal"]
[TimeControl "-"]

6. cxd6 {+0.78/8 0.058s} Bxd6 {-0.64/8 0.057s} 7. Nf3 {+0.84/8 0.048s}
Ne7 {-0.70/9 0.052s} 8. Be2 {+0.81/8 0.052s} O-O {-0.80/9 0.052s}
9. Nc3 {+0.76/9 0.045s} b5 {-0.68/8 0.048s} 10. O-O {+0.92/8 0.057s}
Bb7 {-0.81/7 0.083s} 11. e5 {+0.95/9 0.064s} Bc7 {-1.05/9 0.061s}
12. Ne4 {+0.95/9 0.064s} Nd7 {-0.82/9 0.067s} 13. Re1 {+0.96/9 0.073s}
c5 {-0.78/7 0.088s} 14. Nxc5 {+1.35/10 0.088s} Nxc5 {-0.60/9 0.066s}
15. dxc5 {+0.74/9 0.062s} Qb8 {-0.74/9 0.065s} 16. Qd7 {+0.96/9 0.068s}
Ng6 {-1.02/10 0.052s} 17. Qxb5 {+1.02/10 0.048s} Nxe5 {-0.76/11 0.053s}
18. Nxe5 {+1.10/11 0.043s} Bxe5 {-1.08/11 0.036s} 19. c6 {+1.27/10 0.042s}
Bxh2+ {-1.37/10 0.035s} 20. Kf1 {+1.02/11 0.036s} Ba6 {-1.02/10 0.035s}
21. Qxb8 {+1.37/10 0.031s} Bxe2+ {-1.15/10 0.033s} 22. Rxe2 {+1.15/10 0.041s}
Bxb8 {-1.29/10 0.043s} 23. Be3 {+1.26/11 0.035s} Rc8 {-1.23/9 0.040s}
24. Rc1 {+1.27/8 0.048s} Bc7 {-1.22/8 0.050s} 25. g3 {+1.51/9 0.043s}
Ra6 {-1.48/8 0.050s} 26. Rd2 {+2.00/8 0.055s} Bb8 {-1.36/8 0.044s}
27. Rd7 {+2.18/8 0.054s} Bc7 {-2.27/8 0.052s} 28. Ke2 {+2.31/8 0.063s}
h6 {-2.19/8 0.055s} 29. Rc4 {+2.40/8 0.060s} g5 {-2.36/8 0.055s}
30. Bd4 {+2.43/8 0.059s} e5 {-2.65/8 0.069s} 31. Bc3 {+2.65/9 0.070s}
h5 {-2.62/8 0.075s} 32. Rc5 {+2.43/8 0.075s} Rb6 {-2.43/8 0.062s}
33. Kf3 {+2.47/8 0.063s} f5 {-2.40/7 0.063s} 34. Kg2 {+2.67/8 0.073s}
Rbb8 {-2.62/8 0.071s} 35. Rc4 {+2.78/6 0.066s} Ra8 {-2.62/9 0.050s}
36. Kh2 {+2.60/9 0.054s} Ra6 {-2.81/9 0.056s} 37. Rc5 {+2.83/10 0.048s}
Kf8 {-2.85/10 0.048s} 38. Kg1 {+2.75/8 0.055s} f4 {-2.54/8 0.058s}
39. gxf4 {+2.78/9 0.062s} gxf4 {-2.83/9 0.050s} 40. Kg2 {+2.91/9 0.055s}
h4 {-2.45/8 0.051s} 41. Kh3 {+3.47/8 0.046s} Ke8 {-2.60/7 0.056s}
42. Rh7 {+2.54/9 0.049s} Bd6 {-2.72/10 0.051s} 43. Rh8+ {+2.72/9 0.050s}
Ke7 {-2.78/10 0.042s} 44. Rxc8 {+3.39/10 0.040s} Bxc5 {-2.57/10 0.052s}
45. Bxe5 {+2.97/8 0.049s} Bd6 {-2.39/9 0.045s} 46. Bg7 {+2.89/8 0.056s}
Ke6 {-2.60/10 0.054s} 47. Kxh4 {+2.50/10 0.049s} Kd5 {-2.31/11 0.045s}
48. Bf8 {+2.47/11 0.046s} Bxf8 {-2.58/10 0.049s} 49. Rxf8 {+2.43/10 0.046s}
Rxc6 {-2.46/10 0.044s} 50. Rxf4 {+2.51/10 0.043s} Rc2 {-2.53/10 0.046s}
51. Kg3 {+2.55/9 0.052s} Rxb2 {-2.58/9 0.051s} 52. Rxa4 {+2.54/8 0.052s}
Rb3+ {-2.57/8 0.052s} 53. Kg4 {+2.56/8 0.049s} Rc3 {-2.61/7 0.053s}
54. Ra8 {+2.51/7 0.054s} Rc1 {-2.68/6 0.053s} 55. Kf5 {+2.60/7 0.050s}
Rf1 {-2.87/7 0.048s} 56. f4 {+2.91/9 0.049s} Kc6 {-2.86/7 0.059s}
57. a4 {+2.81/8 0.054s} Kb7 {-2.75/7 0.050s} 58. Ra5 {+3.25/8 0.045s}
Kc6 {-3.45/8 0.053s} 59. Re5 {+3.59/8 0.052s} Kb7 {-3.60/7 0.052s}
60. a5 {+3.99/9 0.041s} Kc6 {-3.91/7 0.041s} 61. a6 {+4.08/9 0.050s}
Kb6 {-4.27/9 0.046s} 62. Re6+ {+4.40/9 0.044s} Ka7 {-4.48/9 0.045s}
63. Kg5 {+4.72/9 0.040s} Rg1+ {-4.70/8 0.037s} 64. Kf6 {+4.90/10 0.043s}
Rc1 {-5.03/9 0.039s} 65. f5 {+5.11/9 0.043s} Rf1 {-5.04/9 0.036s}
66. Kg6 {+5.63/9 0.038s} Rg1+ {-5.09/7 0.047s} 67. Kf7 {+5.88/9 0.044s}
Rc1 {-5.93/8 0.037s} 68. f6 {+6.20/8 0.038s} Kb8 {-6.36/8 0.044s}
69. Ke7 {+7.15/10 0.047s} Rc7+ {-6.97/8 0.044s} 70. Kd6 {+8.96/11 0.046s}
Rc2 {-9.10/10 0.031s} 71. f7 {+10.48/10 0.041s} Rf2 {-12.88/12 0.040s}
72. Ke7 {+12.81/12 0.034s} Kc7 {-13.91/13 0.032s} 73. f8=Q {+13.80/12 0.031s}
Rxf8 {-13.85/12 0.035s} 74. Kxf8 {+13.85/11 0.024s} Kb8 {-14.47/11 0.018s}
75. Re7 {+14.47/10 0.020s} Ka8 {-14.27/11 0.027s} 76. Kf7 {+14.27/10 0.028s}
Kb8 {-13.91/9 0.027s} 77. Kg6 {+14.47/10 0.025s} Ka8 {-15.13/10 0.029s}
78. Kf6 {+14.35/9 0.022s} Kb8 {-14.76/10 0.022s} 79. Rg7 {+16.24/11 0.025s}
Ka8 {-16.24/10 0.026s} 80. Kg5 {+15.00/10 0.028s} Kb8 {-16.51/11 0.027s}
81. Kf4 {+15.92/11 0.030s} Ka8 {-16.24/10 0.027s} 82. Kg3 {+15.10/10 0.028s}
Kb8 {-15.40/9 0.026s} 83. Kh2 {+16.18/11 0.026s} Ka8 {-15.92/10 0.020s}
84. Kh1 {+15.90/9 0.020s} Kb8 {-15.90/11 0.022s} 85. Kg2 {+16.24/11 0.026s}
Ka8 {-16.72/10 0.023s} 86. Kf2 {+15.41/10 0.022s} Kb8 {-15.73/10 0.024s}
87. Rb7+ {+15.92/10 0.025s} Ka8 {-15.92/10 0.020s} 88. Rc7 {+15.72/11 0.025s}
Kb8 {-16.51/11 0.019s} 89. Rd7 {+16.36/11 0.022s} Ka8 {-16.52/10 0.020s}
90. Kf1 {+15.41/10 0.021s} Kb8 {-15.62/9 0.021s} 91. Rb7+ {+15.92/10 0.024s}
Ka8 {-16.24/10 0.022s} 92. Rg7 {+15.10/10 0.024s} Kb8 {-15.58/10 0.026s}
93. Kg1 {+15.92/11 0.021s} Ka8 {-15.81/11 0.025s} 94. Kg2 {+15.41/10 0.023s}
Kb8 {-15.81/10 0.021s} 95. Rb7+ {+16.56/10 0.024s} Ka8 {-15.81/11 0.019s}
96. Rc7 {+16.41/12 0.023s} Kb8 {-16.52/12 0.048s} 97. Rd7 {+16.41/10 0.031s}
Ka8 {-15.81/11 0.034s} 98. Kg3 {+16.63/11 0.024s} Kb8 {-16.41/11 0.023s}
99. Rb7+ {+16.52/10 0.019s} Ka8 {-15.81/11 0.016s} 100. Rf7 {+15.58/11 0.023s}
Kb8 {-16.41/8 0.020s} 101. Rg7 {+16.56/11 0.024s} Ka8 {-16.52/10 0.022s}
102. Kh2 {+15.41/10 0.028s} Kb8 {-15.58/10 0.018s} 103. Rb7+ {+15.80/10 0.026s}
Ka8 {-15.86/10 0.020s} 104. Re7 {+15.41/11 0.019s} Kb8 {-15.38/9 0.021s}
105. Kg1 {+15.80/11 0.021s} Ka8 {-16.26/10 0.022s} 106. Kg2 {+15.58/10 0.019s}
Kb8 {-15.80/11 0.022s} 107. Kh1 {+15.80/11 0.023s} Ka8 {-15.80/10 0.019s}
108. Rf7 {+15.41/10 0.020s} Kb8 {-16.03/11 0.019s} 109. Rb7+ {+15.38/11 0.019s}
Ka8 {-15.38/11 0.019s} 110. Rc7 {+16.24/12 0.023s} Kb8 {-15.59/12 0.019s}
111. Rf7 {+15.92/10 0.020s} Ka8 {-15.86/10 0.019s} 112. Re7 {+15.38/10 0.018s}
Kb8 {-15.86/12 0.022s} 113. Kh2 {+16.24/11 0.019s} Ka8 {-15.86/10 0.021s}
114. Kg1 {+15.38/10 0.016s} Kb8 {-15.73/10 0.015s} 115. Rb7+ {+15.92/10 0.024s}
Ka8 {-15.92/10 0.019s} 116. Rf7 {+15.40/10 0.023s} Kb8 {-15.44/12 0.021s}
117. Rd7 {+15.60/10 0.027s} Ka8 {-16.12/10 0.022s} 118. Kg2 {+15.30/10 0.021s}
Kb8 {-15.27/9 0.019s} 119. Rf7 {+15.51/10 0.022s} Ka8 {-15.73/9 0.017s}
120. Kh2 {+14.49/9 0.017s} Kb8 {-14.26/8 0.018s} 121. Rf8+ {+13.94/8 0.018s}
Ka7 {-10.49/9 0.020s} 122. Rf6 {+12.73/10 0.023s} Kb8 {-9.98/10 0.028s}
123. Kg2 {+10.78/9 0.027s} Ka7 {0.00/255 0.012s}
124. Kh2 {0.00/255 0.011s, Draw by fifty moves rule} 1/2-1/2

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

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each nodes=20000 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=-10 elo1=0 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each tc=10+0.1 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn
fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Echekinator/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 16  -each tc=60+0.6 -rounds 8000 -repeat -recover -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn

fastchess  -openings order=random file=/home/tym972/openbench-books-master/UHO_Lichess_4852_v1.epd  -engine name=new cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -engine name=base cmd=/home/tym972/Base/_build/default/bin/echekinator.exe  -concurrency 8  -each nodes=10000 -rounds 8000 -games 1 -pgnout notation=san file=/home/tym972/Echekinator/Results/Pgn_fastchess.pgn

fastchess -config file=config.json -recover
-log file=/home/tym972/Echekinator/Results/fastchess.log level=info engine=true

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


  (* fastchess   -openings order=random file=/home/tym972/openbench-books/UHO_Lichess_4852_v1.epd   -engine name=new cmd=/home/tym972/Echekinator/_build/install/default/bin/main_new   -engine name=base cmd=/home/tym972/Echekinator/_build/install/default/bin/main   -concurrency 4   -each tc=8+0.08 -rounds 4000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout notation=san seldepth=true -pgnout notation=san file=/home/tym972/Echekinator/Résultats/Pgn_fastchess.pgn
      
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
  get_output position.state_array.(position.game_ply)