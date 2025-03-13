(*Position standard*)
let standard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(*Bug 12/02/2025 100 parties pf4 vs quiescence4*)
let bug_0 = "8/8/8/3pk3/4p1RP/3p2P1/5P2/2B3K1 b - - 0 52"
let bug_0_1 = "8/8/8/3p4/3kp1RP/3p2P1/5P2/2B3K1 w - - 1 53"
let bug_0_2 = "8/8/8/3p4/3kpPRP/3p2P1/8/2B3K1 b - - 0 53"

(*Kiwipete*)
let pre_kiwipete = "r1b1k2r/p1ppqpb1/Qn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1"
let kiwipete = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

(*Lasker-Reichhelm*)
let lasker = "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - - 0 1"

(*Test Mat*)
let tour_et_roi = "8/7K/8/8/8/8/R7/7k w - - 0 1"
let deux_fous_et_roi = "8/8/8/3k4/8/3K4/3BB3/8 w - - 0 1"
let etouffed_1 = "r2qr1k1/pp1bb1pp/2n2n2/6N1/8/2N5/PP1P1PPP/R1BQ1RK1 w - - 0 1"
let etouffed_2 = "rq2r1k1/pp1bb1pp/2n2n2/6N1/8/2N5/PP1P1PPP/R1BQ1RK1 w - - 0 1"
let etouffed_3 = "rq2r1k1/pp1bb1pp/2n2n2/6N1/8/2N5/PP1PbPPP/R1BQ1RK1 w - - 0 1"

(*Puzzles Chess.com*)
let puzzle_0 = "5k2/6P1/5P1P/8/8/p7/8/6K1 b - - 0 1"

(*Test du roque*)
let test_roque = "rnbq1bnr/pppppppp/8/8/8/8/Pk2PPPP/R3KBNR w KQ - 0 1"

(*Erreurs fâcheuses*)
let discorde_0 = "r2qr1k1/ppp2ppp/2nb4/3n2NQ/8/P2pP1PP/1P1N1P2/R1B2RK1 b - - 1 14"
let discorde_1 = "r3k2r/pBpp1pb1/1n3np1/3pq3/4PB2/2p2Q1p/PPP2PPP/R3K2R w KQkq - 0 1"
let discorde_2 = "r3k2r/pBpp1pb1/1n3np1/3p4/5B2/2p2q1p/PPP2PPP/3R1K1R b kq - 0 1"
let discorde_3 = "2b1k2r/1pp2ppp/2nqpn2/3p4/3P4/2PBPN2/1B3PPP/Q3K2R b Kk - 0 1"
let discorde_4 = "Q1bk3r/1pp2ppp/2nq1n2/1B1p4/3P4/4PN2/1B3PPP/4K2R b K - 0 1"
let discorde_5 = "Q1b1k2r/1pp2ppp/2nq1n2/1B1p4/3p4/2P1PN2/1B3PPP/4K2R b Kk - 1 14"
let discorde_6 = "r3k2r/p1ppqpb1/Bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R3K2R b KQkq - 0 1"
let discorde_7 = "r3r1k1/2q2p1p/p5p1/2pbQ1P1/2p1p2P/P3P3/1P1p1PB1/R3RK2 b - - 0 1"
let discorde_8 = "r4rk1/2q2p2/6pP/p1pbn3/2p1B2P/P1QpP3/1P3P2/R3R1K1 b - - 0 1"
let discorde_9 = "r5k1/2q2p1p/p5p1/2pbr1P1/2p1p2P/P3P3/1P1p1PB1/R3RK2 w - - 0 1"
let discorde_10 = "r5k1/2q2p1p/p5p1/2pbr1P1/2p1p2P/P3P3/1P3PB1/4RK2 w - - 0 1"
let discorde_11 = "r4rk1/2q2p2/p1b3pP/2p1n3/2p1B2P/P1QpP3/1P3P2/R3R1K1 b - - 0 1"
let discorde_12 = "r4rk1/2q2p1p/p5p1/2pbn1P1/2p1p2P/P1QpP3/1P3PB1/R3R1K1 b - - 0 1"
let discorde_13 = "r3r1k1/2q2p1p/p5p1/2pbQ1P1/2p1p2P/P3P3/1P1p1PBK/R3R3 b - - 0 1"
let discorde_14 = "r3r1k1/2q2p1p/p1n5/2pb2Pp/2p1p3/P2QP3/1P3PB1/3RRK2 b - - 0 1"
let discorde_15 = "r3r1k1/2q2p1p/p1n5/2pb2Pp/4p3/P2pP3/1P3PB1/3RRK2 w - - 0 1"
let discorde_16 = "5k2/p5p1/P6p/8/1r1p1Q2/1P6/2P2PPK/8 b - - 0 1"
let discorde_17 = "6k1/5pp1/4p1bp/4P3/3P2PP/1r3P2/5r2/2Q4K b - - 0 1"
let discorde_18 = "4r1k1/2p4p/6b1/1p2pp2/8/5K1P/2q5/3R3R b - - 0 1"
let discorde_19 = "4r1k1/2p4p/6b1/1p2pp2/8/3q3P/5K2/3R3R b - - 0 1"
let discorde_20 = "r3k1nr/p1ppqpb1/Bn2p3/3PN1p1/4P3/2B2Q2/PPP2PpP/RR4K1 w - - 0 1"
let discorde_21 = "rn2kbnr/1ppq1ppp/p2p4/4p3/3PP3/1PN2N2/1PP2PPP/R1BQ1RK1 b kq - 0 8"
let discordieux_21 = "8... exd4 9. Nxd4 c5 10. Nd5 Qd8 11. Nb5 Kd7 12. Ndc7 axb5 13. Nxa8 Na6 14. Rxa6 bxa6 15. Qd5 Qb8 16. Qf5+ Ke8 17. Qd5 Ne7 18. Qd1 Qxa8 19. Qxd6 Qxe4 20. Rd1 f5 21. Qxc5 Nc6 22. Qc3 b4 23. Qc4 Be7 24. f3 Qxc4 25. bxc4 Bc5+ 26. Kh1 Bd4 27. g4 fxg4 28. fxg4 Kf7 29. b3 Re8 30. Bd2 Re4 31. g5 Kg8 32. h3 a5 33. Kh2 Re2+ 34. Kh1 g6 35. h4 Rf2 36. Bc1 Rxc2 37. Re1 Bf2 38. Rd1 Bxh4 39. Bf4 Bf2 40. Rd7 Bb6 41. Rb7 Bd4 42. Rc7 Ne5 43. Rc8+ Kg7 44. Rd8 Bc3 45. Rd5 Nc6 46. Rc5 Rf2 47. Bd6 Nd4 48. Be5+ Kg8 49. Bxd4 Bxd4 50. Rxa5 Rf1+ 51. Kh2 Rf2+ 52. Kh1 Rf1+ 53. Kh2 Rf3 54. Rd5 Bb6 55. c5 Bc7+ 56. Kg1 Rxb3 57. c6 Bb6+ 58. Kh1 Rb1+ 59. Kg2 Rc1 60. Rb5 Rxc6 61. Rxb4 Rc2+ 62. Kh1 Rc1+ 63. Kh2 Bc7+ 64. Kh3 Rc2 65. Rb7 Be5 66. Rd7 Rh2+ 67. Kg4 Rg2+ 68. Kh4 Rh2+ 69. Kg4 Rg2+ 70. Kh4"
let discorde_22 = "r1bqkbnr/ppp3pp/5p2/4p3/3np3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 0 7"
let discordieux_22 = "7. Nxd4 Qxd4 8. Qxd4 exd4 9. Nxe4 f5 10. Ng3 Bb4+ 11. Kd1 Nf6 12. Bf4 Ng4 13. Ke2 Be6 14. Bxc7 Rc8 15. h3 Rxc7 16. hxg4 Rxc2+ 17. Kd1 Rd2+ 18. Kc1 Rxf2 19. gxf5 Bxf5 20. Nxf5 Rxf5 21. Bd3 Rf2 22. Be4 b5 23. Kb1 Kf8 24. Rc1 Kg8 25. Bd5+ Kf8 26. a3 Bxa3 27. Rxa3 b4 28. Rc8+ Ke7 29. Rxa7+ Kd6 30. Rxh8 Kxd5 31. Rxg7 b3 32. Rc7 Rxg2 33. Rc1 Ke6 34. Rxh7 Rd2 35. Rb7 Rd3 36. Rc6+ Kf5 37. Rb5+ Kg4 38. Rc1 Kh3 39. Rg1 Kh2 40. Rc1 Kh3 41. Rg1 Kh2 42. Re1 Kg3 43. Rb7 Kg4 44. Rb5 Kg3 45. Rh1 Kg2 46. Rbh5 Rf3 47. Rh7 d3 48. Rd1 Kf2 49. Rd7 Ke2 50. Kc1 Re3 51. Rd2+ Kf1 52. R2xd3 Re1+ 53. Kd2 Re2+ 54. Kc1 Re1+ 55. Kd2 Re2+ 56. Kc3 Re8 57. Kxb3 Ra8 58. Rd1+ Kg2 59. Kc2 Kh3 60. Kb1 Re8 61. Rh1+ Kg4 62. Rhh7 Kg5 63. Rhg7+ Kh6 64. Rh7+ Kg6 65. Rhg7+ Kh6 66. Rh7+ Kg6 67. Rdg7+ Kf5 68. Rf7+ Kg6 69. Rhg7+ Kh5 70. Rh7+ Kg6 71. Rhg7+ Kh6 72. Rc7 Re1+ 73. Ka2 Rf1 74. Rh7+ Kg6 75. Rcg7+ Kf6 76. Rf7+ Kg6 77. Rfg7+ Kf6 78. Rf7+ Kg6 79. Rhg7+ Kh6 80. Re7 Kh5 81. Rh7+ Kg6 82. Rhg7+ Kh6 83. Rc7 Kh5 84. Rh7+ Kg6"
let discorde_23 = "r2qkb1r/pp2ppp1/1np2n1p/3p3b/3P1B2/2P1PN1P/PP1NBPP1/R2QK2R w KQkq - 1 9"
let discordieux_23 = "9. g4 Bg6 10. Ne5 Bh7 11. O-O e6 12. c4 c5 13. cxd5 cxd4 14. exd4 Nbxd5 15. Qa4+ Nd7 16. Nxd7 Nxf4 17. Nb6+ Ke7 18. Qa3+ Kf6 19. Qf3 Qxb6 20. Qxf4+ Ke7 21. Nc4 Qb5 22. Qc7+ Qd7 23. Qxd7+ Kxd7 24. Ne5+ Ke7 25. Rfc1 f6 26. Nd3 Be4 27. Rc7+ Kd8 28. Rf7 Ke8 29. Rc7 Rd8 30. Nc5 Bxc5 31. dxc5 Kf8 32. Bc4 Re8 33. Re1 f5 34. gxf5 Bxf5 35. Rxb7 Bxh3 36. Rxa7 e5 37. Rf7+ Kg8 38. Rf3+ Be6 39. Rxe5 Bxc4 40. Rxe8+ Kh7 41. Rxh8+ Kxh8 42. Rf8+ Kh7 43. c6 Bxa2 44. c7 Be6 45. c8=Q Bxc8 46. Rxc8 g5 47. Rf8 g4 48. Rf5 Kg6 49. Rf8 Kg7 50. Rf5 Kg6 51. Rf8 Kh7 "
let discorde_24 = "rn2kbnr/1bqp1ppp/pp2p3/2p5/2PP4/2N1PN2/PP3PPP/R1BQKB1R w KQkq - 1 7"
let discordieux_24 = "e3e4 g8f6 d4d5 d7d6 f1d3 b8d7 e1g1 f8e7 c1e3 e8g8 d1b3 e6e5 a1e1 f8e8 h2h3 a8d8 e1d1 f6h5 c3e2 h5f6 e2g3 h7h6 g3f5 e7f8 f1e1 f6h5 g2g3 h5f6 f3e5 d7e5 d3e2 f6e4 b3c2 e4g5 g1g2 g5e6 g2h2 e6g5 h2g2 g5e6 f2f3 e6d4 f5d4 c5d4 d1d4 b6b5 a2a3 b5c4 f3f4 c7a5 c2d1 e5d3 e2d3 b7d5 g2g1 c4d3 d4a4 d5f3 a4a5 f3d1 a5a6 d1e2 e3d2 d6d5 a6a5 f8e7 d2c3 e2f3 f4f5 e7d6 e1e8 d8e8 g1f2 f3e4 a5b5 h6h5 b5a5 e8d8 g3g4 d6c7 a5b5 h5g4 h3g4 c7f4 b5b4 d8e8 b4a4 e8d8 a4b4 d8e8 "
let discorde_25 = "rn2kb1r/pp1qnpp1/4p3/2ppP2p/5P2/2N2N2/PPPP2PP/R1BQ1RK1 b kq - 1 8"
let discordieux_25 = "b8c6 d2d4 e7f5 c3e2 c5d4 e2d4 c6d4 f3d4 f8c5 c2c3 e8g8 d1d3 h5h4 c1d2 f8e8 f1e1 a8d8 e1d1 a7a6 g1h1 c5b6 d4f5 e6f5 d2e1 h4h3 d3h3 d7b5 e1d2 b5b2 h3f5 b2b5 d1e1 b5c4 f5c2 b6d4 a1b1 c4c6 c2b3 d4f2 e1f1 f2c5 b3b7 c6a4 b7b3 a4b3 b1b3 e8e6 f1b1 e6e8 b3b7 e8e6 d2e1 c5e3 e1g3 e6c6 b1b3 e3d2 b7b8 c6c8 b8c8 d8c8 b3b6 c8c3 b6b8 g8h7 b8b1 c3c2 a2a3 d2e3 f4f5 c2a2 e5e6 f7e6 f5e6 a2e2 g3d6 e3g5 b1d1 e2e6 d6c5 e6e5 c5d4 e5e8 d4b6 e8e5 b6c5 h7g8 h1g1 g5f4 h2h3 e5g5 d1f1 f4e5 f1f8 g8h7 f8a8 e5f6 a8a6 d5d4 a6c6 d4d3 c5b4 g5d5 b4d2 f6b2 c6a6 d5d8 g1h1 b2d4 a6c6 h7g8 d2g5 d8e8 c6c7 e8e1 h1h2 d4e5 g2g3 e5c7 h2g2 d3d2 g5d2 e1e2 g2g1 e2d2 g3g4 c7b6 g1f1 d2h2 a3a4 h2h3 a4a5 b6a5 f1g2 h3h4 g2f3 h4h2 g4g5 a5c3 f3g3 c3e5 g3f3 h2h5 f3g2 h5g5 g2f1 e5d4 f1e1 g5g1 e1e2 g1g2 e2f1 g2d2 f1e1 d2f2 e1d1 d4e3 d1e1 e3d4 e1d1 d4e3 d1e1 e3c5 e1d1 c5d4 d1c1 f2b2 c1d1 d4e3 d1e1 b2f2 e1d1 f2g2 d1e1 g2f2 e1d1 e3f4 d1e1 f2d2 e1f1 f4e3 f1e1 d2g2 e1f1 g2d2 f1e1 d2g2 e1f1 g2g1 f1e2 e3d4 e2d2 d4e5 d2e2 e5d4 e2d2 d4e5 d2e2 e5f6 e2f2 f6d4 f2e2 d4c5 e2d2 c5d6 d2e2 d6c5 e2d2 c5d6 d2e2 d6b4 e2f2 b4c5 f2e2 c5d6 e2f2 d6c5 f2e2 c5b6 e2d2 g1f1 d2c2 b6d4 c2d2 d4e5 d2e2 f1f8 e2e1 e5d4 e1d2 f8f1 d2e2 f1b1 e2d2 d4e5 d2e2 b1b2 e2e1 e5d4 e1d1 d4e5 "
let discorde_26 = "r3kbnr/pp1n1ppp/2p2q2/4p3/2B1P3/2PQ4/PP3PPP/RNB1K2R w KQkq - 4 9"
let discordieux_26 = "9. Nd2 b5 10. Bb3 Nc5 11. Qe3 Nxb3 12. Nxb3 Bd6 13. O-O Ne7 14. Qd3 O-O 15. Be3 Rfe8 16. Nc5 h6 17. Rfd1 Bc7 18. c4 bxc4 19. Qxc4 Ng6 20. Rac1 Ne7 21. Rd7 Bb6 22. Rcd1 Nc8 23. Qc2 Qg6 24. Qd3 Ne7 25. Qc3 Qh5 26. Qc2 Rab8 27. Qd3 Rbd8 28. Qc2 Rxd7 29. Rxd7 Qg6 30. Qd3 f5 31. Qb3+ Kh7 32. Qd3 f4 33. Na4 fxe3 34. fxe3 Nd5 35. Nxb6 axb6 36. h4 Kg8 37. h5 Qe6 38. Rb7 Nf6 39. Qa6 Nxh5 40. Qxb6 Qxa2 41. Qxc6 Nf6 42. Qc7 Qb1+ 43. Kh2 Ng4+ 44. Kg3 Qe1+ 45. Kxg4 Qe2+ 46. Kh3 Qh5+ 47. Kg3 Qg5+ 48. Kh2 Qh5+ 49. Kg1 Qd1+ 50. Kh2 Qh5+ 51. Kg1 Qd1+ 52. Kf2 Qd2+ 53. Kf1 Rf8+ 54. Kg1 Qxe3+ 55. Kh2 Qf4+ 56. Kg1 Qe3+ 57. Kh2 Qf4+ 58. Kh1 Qh4+ 59. Kg1 Qf2+ 60. Kh1 Qe1+ 61. Kh2 Qh4+ 62. Kg1 Qe1+ 63. Kh2 Qh4+ 64. Kg1"
let discorde_27 = "r3kbnr/1b1ppppp/2n5/q1p5/8/2N2NP1/PP1PPPBP/R1BQK2R b KQkq - 4 8"
let discordieux_27 = "c6e5 e1g1 e5f3 g2f3 b7f3 e2f3 g8f6 c3e4 d7d5 e4f6 e7f6 d1c2 a5a4 f1e1 e8d8 c2f5 a4d4 f3f4 g7g6 f5c2 f6f5 d2d3 f8g7 c2b3 c5c4 d3c4 d4c4 b3b7 a8a4 e1e7 c4c8 e7f7 c8b7 f7b7 g7f8 b7b8 d8c7 b8b3 f8c5 b3c3 c7b6 c3d3 h8d8 b2b3 a4e4 c1b2 b6b7 a1c1 c5b6 b2f6 d8d7 c1d1 e4e2 f6d4 b6d4 d3d4 e2a2 d4d5 d7d5 d1d5 a2a1 g1g2 a1b1 d5d7 b7b8 d7h7 b1b3 h7h8 b8c7 h8h7 c7b8 h7g7 b3b6 g2g1 b6b1 g1g2 b1b6 g7e7 b6f6 g2g1 f6d6 e7e8 b8c7 e8e7 c7b8 e7e8 b8b7 e8g8 d6d1 g1g2 d1d6 g2g1 d6d1 g1g2 d1d6 g8g7 b7b8 g2g1 d6d1 g1g2 d1d6 h2h3 d6e6 g2g1 e6e1 g1h2 e1e2 g7g8 b8b7 h2g2 e2e6 g2g1 e6e1 g1h2 e1e2 h2g2 e2e6 g3g4 f5g4 h3g4 e6e4 g8g7 b7b8 g7g6 e4f4 g6g8 b8c7 g8g7 c7b8 g7g8 b8c7 g4g5 f4g4 g2h2 c7b7 g5g6 g4h4 h2g3 h4h1 g3g2 h1h4 g6g7 h4g4 g2h1 g4g6 h1h2 g6g4 h2h1 g4g6 f2f4 g6h6"


(*Test 960*)
let test_fisher0 = "bqnb1rkr/pp3ppp/3ppn2/2p5/5P2/P2P4/NPP1P1PP/BQ1BNRKR w HFhf - 2 9"
let test_fisher1 = "2nnrbkr/p1qppppp/8/1ppb4/6PP/3PP3/PPP2P2/BQNNRBKR w HEhe - 1 9"
let test_fisher2 = "b1q1rrkb/pppppppp/3nn3/8/P7/1PPP4/4PPPP/BQNNRKRB w GE - 1 9"
let test_fisher3 = "qbbnnrkr/2pp2pp/p7/1p2pp2/8/P3PP2/1PPP1KPP/QBBNNR1R w hf - 0 9"
let test_fisher4 = "1nbbnrkr/p1p1ppp1/3p4/1p3P1p/3Pq2P/8/PPP1P1P1/QNBBNRKR w HFhf - 0 9"
let test_fisher5 = "qnbnr1kr/ppp1b1pp/4p3/3p1p2/8/2NPP3/PPP1BPPP/QNB1R1KR w HEhe - 1 9"
let test_fisher6 = "q1bnrkr1/ppppp2p/2n2p2/4b1p1/2NP4/8/PPP1PPPP/QNB1RRKB w ge - 1 9"
let test_fisher7 = "qbn1brkr/ppp1p1p1/2n4p/3p1p2/P7/6PP/QPPPPP2/1BNNBRKR w HFhf - 0 9"
let test_fisher8 = "qnnbbrkr/1p2ppp1/2pp3p/p7/1P5P/2NP4/P1P1PPP1/Q1NBBRKR w HFhf - 0 9"
let test_fisher9 = "qn1rbbkr/ppp2p1p/1n1pp1p1/8/3P4/P6P/1PP1PPPK/QNNRBB1R w hd - 2 9"
let test_fisher10 = "qnr1bkrb/pppp2pp/3np3/5p2/8/P2P2P1/NPP1PP1P/QN1RBKRB w GDg - 3 9"
let test_fisher11 = "qb1nrkbr/1pppp1p1/1n3p2/p1B4p/8/3P1P1P/PPP1P1P1/QBNNRK1R w HEhe - 0 9"
let test_fisher12 = "qnnbrk1r/1p1ppbpp/2p5/p4p2/2NP3P/8/PPP1PPP1/Q1NBRKBR w HEhe - 0 9"
let test_fisher13 = "1qnrkbbr/1pppppp1/p1n4p/8/P7/1P1N1P2/2PPP1PP/QN1RKBBR w HDhd - 0 9"
let test_fisher14 = "qn1rkrbb/pp1p1ppp/2p1p3/3n4/4P2P/2NP4/PPP2PP1/Q1NRKRBB w FDfd - 1 9"
let test_fisher15 = "bb1qnrkr/pp1p1pp1/1np1p3/4N2p/8/1P4P1/P1PPPP1P/BBNQ1RKR w HFhf - 0 9"
let test_fisher16 = "bnqbnr1r/p1p1ppkp/3p4/1p4p1/P7/3NP2P/1PPP1PP1/BNQB1RKR w HF - 0 9"
let test_fisher17 = "bnqnrbkr/1pp2pp1/p7/3pP2p/4P1P1/8/PPPP3P/BNQNRBKR w HEhe d6 0 9"
let test_fisher18 = "b1qnrrkb/ppp1pp1p/n2p1Pp1/8/8/P7/1PPPP1PP/BNQNRKRB w GE - 0 9"
let test_fisher19 = "n1bqnrkr/pp1ppp1p/2p5/6p1/2P2b2/PN6/1PNPPPPP/1BBQ1RKR w HFhf - 2 9"
let test_fisher20 = "n1bb1rkr/qpnppppp/2p5/p7/P1P5/5P2/1P1PPRPP/NQBBN1KR w Hhf - 1 9"