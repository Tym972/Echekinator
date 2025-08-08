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
let discorde_28 = "r1b1kb1r/pp3ppp/2n1p3/2qn4/8/P1NB1N2/1PP2PPP/R1BQK2R w KQkq - 0 9"
let discordieux_28 = "9. Ne4 Qb6 10. c4 Nc7 11. Bf4 Bd7 12. c5 Qxb2 13. Bxc7 Bxc5 14. Rb1 Qxa3 15. Nxc5 Qxc5 16. Rxb7 Nb4 17. Be4 Bc6 18. Bxc6+ Qxc6 19. Rb8+ Rxb8 20. Bxb8 Qe4+ 21. Kf1 a6 22. Bd6 Nc6 23. Kg1 Rg8 24. Ng5 Qf5 25. Nf3 Qe4 26. Bg3 Qc4 27. Qd2 Qe4 28. Qc1 h6 29. Ne5 Nxe5 30. Bxe5 Qxe5 31. Qc6+ Kf8 32. Qc8+ Ke7 33. Qb7+ Kf6 34. Qf3+ Qf5 35. Qxf5+ Kxf5 36. h4 Kg6 37. Rh3 Kh7 38. Rf3 Rf8 39. h5 e5 40. Ra3 Ra8 41. Ra5 f6 42. f4 exf4 43. Rf5 Re8 44. Rxf4 Kg8 45. Rf1 Re5 46. Ra1 a5 47. Rd1 Rxh5 48. Rd8+ Kh7 49. Rd7 Re5 50. Rf7 f5 51. Rd7 Re1+ 52. Kh2 f4 53. Rd4 g5 54. Rd7+ Kg8 55. Rd8+ Kh7 56. Rd7+ Kg8 57. Rd8+ Kg7 58. Rd7+ Kh8 59. Ra7 Ra1 60. Re7 g4 61. Re8+ Kg7 62. Re7+ Kf8 63. Re4 Rf1 64. g3 Rf2+ 65. Kg1 fxg3 66. Rxg4 Rf3 67. Rg6 h5 68. Rg5 a4 69. Rxh5 a3 70. Kg2 Rc3 71. Rf5+ Kg7 72. Rg5+ Kh7 73. Rg4 Re3 74. Rh4+ Kg7 75. Rg4+ Kh7 76. Rh4+ Kg7 77. Rg4+ Kh8 78. Rh4+ Kg8 79. Rg4+ Kf7 80. Rf4+ Kg8 81. Rg4+ Kf7 82. Rf4+ Ke7 83. Rg4 Rd3 84. Rg7+ Kf8 85. Rc7 Kg8 86. Rc8+ Kh7 87. Rc4 Rb3 88. Rc7+ Kg8 89. Kh3 Rf3 90. Rc8+ Rf8 91. Rc2 Rf3 92. Rc8+ Kg7 93. Rc2 Re3 94. Rg2 Kg8 95. Rd2 Rf3 96. Rd8+ Kh7 97. Rd2 Rf2 98. Rd7+ Kg8 99. Rd8+ Kh7 100. Kxg3 a2 101. Rd1 Re2 102. Rd7+ Kg8 103. Rd8+ Kh7 104. Rd7+ Kg8 105. Rd8+ Kg7 106. Rd1 Rc2 107. Rd7+ Kg8 108. Rd8+ Kh7 109. Rd1 Kg8 110. Rd8+ Kh7 111. Ra8 Rc3+ 112. Kh2 Rc2+ 113. Kh3 Rc3+ 114. Kh2 Rc2+ 115. Kh3 Rf2 116. Kg3 Re2 117. Ra7+ Kg8 118. Ra8+ Kh7 119. Kh3 Rf2 120. Kg3 Rd2 121. Ra7+ Kg8 122. Ra8+ Kh7 123. Kh3 Rd3+ 124. Kh2 Rd2+ 125. Kh3 Re2 126. Ra7+ Kg8 127. Ra8+ Kh7 128. Ra7+ Kg8 129. Ra8+ Kg7 130. Ra7+ Kh8 131. Ra8+ Kg7 132. Ra7+ Kf8 133. Ra8+ Kf7 134. Ra7+ Kf8 135. Ra8+ Ke7 136. Ra7+ Kd8 137. Ra8+ Kc7 138. Ra4 Kb8 139. Rb4+ Ka7 140. Ra4+ Kb6 141. Ra8 Kb7 142. Ra4 Rf2 143. Rb4+ Ka7 144. Ra4+ Kb6 145. Ra8 Kb7 146. Ra4 Kb6 147. Ra8 Kb7 148. Ra4 Kb8 149. Rb4+ Ka7 150. Ra4+"
let discorde_29 = "rnbq1rk1/ppp1b1pp/3p1n2/5p2/3P4/1P1B1N2/P1P2PPP/RNBQR1K1 b - - 0 8"
let discordieux_29 = "8... Nc6 9. Nc3 d5 10. Bf4 Bb4 11. Bd2 Ne4 12. Nxe4 fxe4 13. Bxb4 Nxb4 14. Bxe4 dxe4 15. Rxe4 Bf5 16. Re2 Bg4 17. Re3 Bxf3 18. Rxf3 Rxf3 19. gxf3 Qg5+ 20. Kh1 Rd8 21. c3 Nd5 22. Qd3 Qf6 23. Kg1 Nf4 24. Qc4+ Nd5 25. Qd3 Qg5+ 26. Kh1 Qf4 27. Rd1 Qf6 28. Kg1 Rf8 29. c4 Nb4 30. Qe4 Qxf3 31. Qxf3 Rxf3 32. c5 Rc3 33. Rd2 Rc1+ 34. Kg2 Ra1 35. Re2 Rxa2 36. Re8+ Kf7 37. Rb8 b6 38. cxb6 axb6 39. Rh8 Nc2 40. Rc8 Nxd4 41. Rxc7+ Kg8 42. Rc8+ Kf7 43. Rc7+ Kf8 44. Rc8+ Ke7 45. Rc7+ Kf8 46. Rc8+ Ke7 47. Rc7+ Kf6 48. b4 Rb2 49. Rc4 Ne6 50. Rh4 Kf7 51. Re4 Nf8 52. Rf4+ Kg8 53. Re4 b5 54. Kg1 h6 55. h3 Ng6 56. Re8+ Kh7 57. Re4 Kg8 58. Re8+ Kh7 59. Re4 Rd2 60. Kg2 Kg8 61. Re8+ Kh7 62. Re4 Rd5 63. Kg1 Ne5 64. Kh1 Rd1+ 65. Kh2 Nc4 66. Re7 Rb1 67. Kg2 Rxb4 68. Kg1 Rb1+ 69. Kh2 Rb2 70. Kg1 Nd6 71. Re6 Nf5 72. Re1 Kg8 73. Re8+ Kh7 74. Re1 b4 75. Rd1 Kg8 76. Rd8+ Kh7 77. Rd1 b3 78. Rd5 Nh4 79. Rd3 Rb1+ 80. Kh2 b2 81. Rb3 Nf3+ 82. Rxf3 Rf1 83. Rb3 b1=R 84. Rxb1 Rxb1 85. Kg2 Kg8 86. Kh2 Rf1 87. Kg2 Re1 88. Kh2 Rf1 89. Kg2 Re1 90. Kh2 Rd1 91. Kg2 Rd2 92. Kf1 h5 93. Kg2 Rd1 94. Kh2 Rf1 95. Kg2 Re1 96. Kh2 Rf1 97. Kg2 Re1 98. Kh2 Re2 99. Kg2 h4 100. Kf1 Rd2 101. Kg2 Re2 102. Kf1 Rd2 103. Kg2 Rc2 104. Kf1 Rc3 105. Kg2 Rc2 106. Kf1 Rc3 107. Kg2 Rd3 108. Kh2 Rf3 109. Kg2 Rf4 110. Kf1 Rf3 111. Kg2 Rf4 112. Kf1 Re4 113. Kg2 Re1 114. Kh2 Rf1 115. Kg2 Re1 116. Kh2 Rf1 117. Kg2 Rd1 118. Kh2 Rd3 119. Kg2 Rd2 120. Kf1 Rd3 121. Kg2 Rd2 122. Kf1 Rc2 123. Kg2 Rb2 124. Kf1 Rb3 125. Kg2 Rd3 126. Kh2 Rd1 127. Kg2 Rc1 128. Kh2 Re1 129. Kg2 Rd1 130. Kh2 Re1 131. Kg2 Rc1 132. Kh2 Rd1 133. Kg2 Rb1 134. Kh2 Rc1 135. Kg2 Rb1 136. Kh2 Rc1 137. Kg2 Ra1 138. Kh2 Rb1 139. Kg2 Ra1 140. Kh2 Rb1 141. Kg2 Rb2 142. Kf1 Rc2 143. Kg2 Ra2 144. Kg1 Ra8 145. Kh1 Ra2 146. Kg2 Ra8 147. Kh1 Ra2 148. Kg2 Ra3 149. Kh2 Ra1"
let discorde_30 = "rnbqk2r/pp3ppp/2p2n2/3p4/1bP5/1B3N2/PP1P1PPP/RNBQK2R b KQkq - 0 7"
let discordieux_30 = "7... Nbd7 8. Qe2+ Qe7 9. Qxe7+ Kxe7 10. cxd5 cxd5 11. Nc3 Nb6 12. O-O Bf5 13. d4 Kf8 14. Bg5 Bd3 15. Rfe1 Ne4 16. Ne5 Bc2 17. Bxc2 Nxg5 18. Nd3 Bxc3 19. bxc3 Ne4 20. Nc5 Nxc5 21. dxc5 Nd7 22. Rad1 Nxc5 23. Rxd5 b6 24. c4 Kg8 25. Re7 Kf8 26. Rxa7 Re8 27. Rd1 Kg8 28. Bf5 h6 29. h3 h5 30. Rb1 Rh6 31. Rd1 Rf6 32. Re7 Ra8 33. Re5 Ne6 34. Ra1 Nc5 35. Bc2 h4 36. Rae1 Ne6 37. Bb3 Nd4 38. Re8+ Rxe8 39. Rxe8+ Kh7 40. Re4 Rf4 41. Rxf4 Ne2+ 42. Kh1 Nxf4 43. c5 bxc5 44. Bxf7 Kh8 45. Bb3 Nd3 46. Kg1 g5 47. Bd5 Ne5 48. Be4 c4 49. a4 Nd7 50. a5 Nc5 51. Bf3 c3 52. Bd1 Na6 53. g3 Nb4 54. a6 Nxa6 55. gxh4 gxh4 56. f4 Nb4 57. f5 c2 58. Bxc2 Nxc2 59. Kf2 Nd4 60. f6 Kg8 61. Ke3 Ne6 62. Kf3 Ng5+ 63. Kg4 Ne4 64. Kxh4 Nxf6 65. Kg3 Ne4+ 66. Kh2 Nf2 67. Kg2 Nd3 68. Kh1 Nf2+ 69. Kh2 Kg7 70. Kg2 Nd3 71. Kh1 Nf2+ 72. Kh2 Kg8 73. Kg2 Nd3 74. Kh1 Nf4 75. Kh2 Ne2 76. Kg2 Nf4+ 77. Kh2 Ne2 78. Kh1 Ng3+ 79. Kh2 Nf5 80. Kh1 Ng3+ 81. Kh2 Nf5 82. Kh1 Ne3 83. Kh2 Nc2 84. Kh1 Ne3 85. Kh2 Nc2 86. Kh1 Nb4 87. Kh2 Nc6 88. Kh1 Nb4 89. Kh2 Nc6 90. Kh1 Nd4 91. Kg2 Ne2 92. Kh1 Nc3 93. Kh2 Nb5 94. Kh1 Nc3 95. Kh2 Ne4 96. Kh1 Ng5 97. Kh2 Nf3+ 98. Kh1 Ng5 99. Kh2 Kh7 100. Kg2 Kg7 101. Kh2 Kg8 102. Kg2 Kh7 103. Kh2 Kg8 104. Kg2 Kh7 105. Kh2 Kg7 106. Kg2 Kh8 107. Kh2 Kh7 108. Kg2 Kg7 109. Kh2 Kh8 110. Kg2 Ne6 111. Kf2 Nf4 112. Kg3 Nd5 113. Kh2 Kh7 114. Kh1 Nf4"
let discorde_31 = "r1b1k1nr/ppppqppp/2n5/4P3/1b6/2N1BN2/PP3PPP/R2QKB1R b KQkq - 2 7"
let discordieux_31 = "7... Nxe5 8. Nxe5 Qxe5 9. Qd4 Qxd4 10. Bxd4 Kf8 11. a3 Be7 12. Nd5 Bd6 13. Bc4 Ne7 14. Nxe7 Bxe7 15. O-O c6 16. Rfe1 d5 17. Bd3 Be6 18. Rad1 c5 19. Bc3 h6 20. Be5 Kg8 21. Be2 c4 22. Bf3 Rd8 23. Bd4 a6 24. h3 Rd7 25. Rf1 Bd6 26. Rfe1 h5 27. Bc3 Bc7 28. a4 h4 29. a5 b6 30. axb6 Bxb6 31. Ra1 a5 32. Bxa5 Bd4 33. Bc3 Bxc3 34. Ra8+ Kh7 35. Rxh8+ Kxh8 36. bxc3 Rb7 37. Rd1 Rb5 38. Rd4 g5 39. Rd1 Kg8 40. Kh1 Rc5 41. Kg1 Rb5 42. Re1 Rb6 43. Re5 Rb1+ 44. Kh2 Rb2 45. Rxg5+ Kh7 46. Bxd5 Kh6 47. f4 f6 48. Bxe6 fxg5 49. fxg5+ Kxg5 50. Bxc4 Rc2 51. Kh1 Rxc3 52. Bd5 Kh6 53. Kg1 Kg7 54. Kh1 Rc2 55. Kg1 Re2 56. Kh1 Rd2 57. Bb3 Rb2 58. Bc4 Rc2 59. Bd3 Rd2 60. Bc4 Rc2 61. Bd3 Rd2 62. Bc4 Rf2 63. Kg1 Rc2 64. Bd3 Rd2 65. Bc4 Rc2 66. Bd3 Rd2 67. Bc4 Rb2 68. Kh1 Kh8 69. Kg1 Rd2 70. Kh1 Rc2 71. Bd3 Rd2 72. Bc4 Rc2 73. Bd3 Rf2 74. Kg1 Rd2 75. Bc4 Rc2 76. Bd3 Rd2 77. Bc4 Rc2 78. Bd3 Rb2 79. Bc4 Rb1+ 80. Kh2 Re1 81. Bb3 Re2 82. Kh1 Rb2 83. Bc4 Rf2 84. Kg1 Rb2 85. Kh1 Rf2 86. Kg1 Rf4 87. Ba2 Re4 88. Bb3 Re2 89. Kh1 Kg7 90. Kg1 Rb2 91. Bc4 Kh6 92. Kh1 Kg7 93. Kg1 Rb1+ 94. Kh2 Re1 95. Bb3 Re2 96. Kh1 Rf2 97. Kg1 Re2 98. Kh1 Re1+ 99. Kh2 Re3 100. Bc4 Rg3 101. Kh1 Rg5"
let discorde_32 = "r2qkb1r/pppb2pp/2n1pn2/1B3p2/3P4/2P2N2/PP1N1PPP/R1BQK2R b KQkq - 2 8"
let discordieux_32 = "8... a6 9. Bd3 Nd5 10. O-O Nf4 11. Bc2 Be7 12. Nc4 Nd5 13. Nfe5 Nxe5 14. Nxe5 O-O 15. Nxd7 Qxd7 16. c4 Nb4 17. Bb3 Bf6 18. Be3 f4 19. Bxf4 Qxd4 20. Qxd4 Bxd4 21. Bxc7 Nd3 22. Bg3 Bxb2 23. Rad1 Nc5 24. Bc2 Bf6 25. Bd6 Rfc8 26. Rfe1 Bc3 27. Re2 Bf6 28. Ree1 Rc6 29. Bg3 Re8 30. Be5 Bxe5 31. Rxe5 Rcc8 32. Rde1 Red8 33. R1e2 Rd4 34. Bb3 a5 35. Rxc5 Rxc5 36. Rxe6 Rd8 37. Re7 Rb8 38. h3 h6 39. Rd7 Ra8 40. Rxb7 a4 41. Bc2 Rxc4 42. Bd3 Rc1+ 43. Kh2 Rd8 44. Be4 Rf1 45. Rb2 a3 46. Rc2 Rfd1 47. Rc3 R1d2 48. Rxa3 Rxf2 49. Ra7 Rb2 50. a3 Rf2 51. Kg1 Re2 52. Bf3 Re1+ 53. Kh2 Ree8 54. Bh5 Re4 55. Bf7+ Kh7 56. Ba2 Re2 57. Bc4 Rb2 58. Ra5 Rdd2 59. Bd5 Re2 60. Ra7 Rbd2 61. Bf3 Re8 62. Kg1 Re1+ 63. Kh2 Re8 64. Kg1 Re1+ 65. Kh2 Rf2 66. Rf7 Ra2 67. Rd7 Rxa3 68. Bd1 Rc3 69. Bg4 Re8 70. Bh5 Re1 71. Bg4 Re8 72. Bf5+ Kg8 73. Bg6 Rf8 74. Be4 Rc1 75. Bd5+ Kh7 76. Re7 Rd8 77. Bb3 Rd2 78. Be6 Rf1 79. Rd7 Rxd7 80. Bxd7 Rf2 81. Kg1 Re2 82. Bb5 Re1+ 83. Kh2 Rc1 84. Be2 Kg8 85. Bf3 Rc5 86. Kg1 Rc1+ 87. Kh2 Rc5 88. Be4 Re5 89. Bf3 Rg5 90. Kh1 h5 91. h4 Rf5 92. Kg1 Re5 93. Kh2 Rf5 94. Kg1 Rb5 95. Kh2 Rb4 96. Bd5+ Kh7 97. Kh3 Rf4 98. Bb3 Rf1 99. Bc2+ g6 100. Kh2 Rf4 101. Kh3 Rd4 102. Bb1 Rd2 103. Kh2 Kg7 104. Kh3 Rf2 105. Kh2 Rd2 106. Kh3 Kh7 107. Kh2 Rd8 108. Ba2 Rd4 109. Kh3 Rd2 110. Bc4 Rf2 111. Bd3 Rd2 112. Bb1 Kg7 113. Kh2 Kh7 114. Kh3 Rb2 115. Be4 Rb4 116. Bf3 Rd4 117. g3 Rd2 118. Bg2 Kg8 119. Be4 Kh7 120. Bf3 Kg8 121. Bg2 Rf2 122. Be4 Kh7 123. Bd3 Rf3 124. Bb1 Rb3 125. Be4 Re3 126. Bg2 Re2 127. Bf3 Rb2 128. Be4 Rf2 129. Bd3 Rd2 130. Be4 Re2 131. Bd3 Rf2 132. Bb1 Re2 133. Bd3 Rf2 134. Bb1 Kg7 135. Bd3 Rd2 136. Be4 Rf2 137. Bd3 Rf3 138. Be4 Re3 139. Bg2 Re2 140. Bf3 Rf2 141. Be4 Rd2 142. Bf3 Rd3 143. Bg2 Rd2 144. Kh2 Kg8 145. Kh1 Rd1+ 146. Kh2 Rd2 147. Kh1 Rd3 148. Kh2 Rd1 149. Bf3 Rd4 150. Kg1 Rd3 151. Kg2 Rd2+ 152. Kg1 Rd8 153. Be4 Rd1+ 154. Kh2 Rd2+ 155. Kh1 Rd1+ 156. Kh2 Rd2+ 157. Kg1 Kh7 158. Bf3 Rd3 159. Kg2 Rd2+ 160. Kh1 Rf2 161. Be4 Re2 162. Bd5 Rd2 163. Bf3 Rd3 164. Kg2 Kg8 165. Kf2 Rd2+ 166. Ke1 Rb2 167. Be4"
let discorde_33 = "rn3rk1/ppp2ppp/4bn2/3q4/1b1P4/2N2NP1/PP3P1P/R1BQKB1R w KQ - 3 9"
let discordieux_33 = "9. Bd2 Qh5 10. a3 Be7 11. Bf4 Rc8 12. Bg2 Nc6 13. O-O Nd5 14. Nxd5 Bxd5 15. Ne5 Qxd1 16. Rfxd1 Bxg2 17. Kxg2 Nxe5 18. dxe5 Bc5 19. Rd7 h6 20. Rad1 g5 21. Bd2 Bb6 22. Kg1 Re8 23. Bc3 g4 24. R1d2 a6 25. Rd1 Rac8 26. R1d2 Bc5 27. Rd1 Bb6 28. R1d2 Bc5 29. R2d3 Bb6 30. Rd1 Bc5 31. Re1 Red8 32. e6 fxe6 33. Rg7+ Kf8 34. Rxg4 Rd6 35. Bg7+ Kf7 36. Bxh6 Bd4 37. Rf4+ Kg8 38. Rg4+ Kh7 39. Bf4 Rc6 40. Rh4+ Kg8 41. Rg4+ Kh7 42. Bxc7 Bxb2 43. Rh4+ Kg8 44. Be5 Bxe5 45. Rxe5 Rc1+ 46. Kg2 Re8 47. Rg5+ Kf8 48. Rf4+ Ke7 49. Rg7+ Kd8 50. Rxb7 e5 51. Rff7 Rc6 52. Rb8+ Rc8 53. Rbb7 Rc6 54. Ra7 e4 55. Ra8+ Rc8 56. Rxa6 Rc2 57. Raa7 Rc1 58. g4 Rc5 59. Kg1 Rc1+ 60. Kg2 Rc5 61. Rg7 Rc2 62. g5 Rf8 63. Ra8+ Rc8 64. Raa7 Rc2 65. Ra8+ Rc8 66. Ra4 Rc2 67. Rd4+ Kc8 68. Rxe4 Rfxf2+ 69. Kg1 Rg2+ 70. Kf1 Rcf2+ 71. Ke1 Rf8 72. Rc4+ Kb8 73. Rb4+ Ka8 74. h3 Rg1+ 75. Ke2 Rg2+ 76. Ke1 Rg1+ 77. Ke2 Rg2+ 78. Ke3 Rg3+ 79. Kd2 Rxa3 80. h4 Ra2+ 81. Ke1 Ra1+ 82. Ke2 Re8+ 83. Kf2 Rd8 84. Ke2 Re8+ 85. Kf2 Rd8 86. Re4 Kb8 87. Re2 Rh1 88. Rb2+ Ka8 89. Ra2+ Kb8 90. Rb2+ Ka8 91. Ra2+ Kb8 92. Ra4 Rd2+ 93. Ke3 Rb2 94. Rg8+ Kb7 95. Rg7+ Kb8 96. Rg8+ Kb7 97. Kf3 Rh3+ 98. Kg4 Rhh2 99. Rg7+ Kb8 100. Rg8+ Kb7 101. Rg7+ Kb8 102. Kh5 Rhg2 103. Rg8+ Kb7 104. Rga8 Kc7 105. R4a7+ Rb7 106. Ra1 Rd2 107. Re1 Rb8 108. Ra7+ Kd8 109. Ree7 Rbb2 110. g6 Rd5+ 111. Kh6 Rb4 112. Rf7 Rxh4+ 113. Kg7 Rd7 114. Raxd7+ Kc8 115. Kf6 Rh1 116. g7 Rf1+ 117. Ke7 Rxf7+ 118. Kxf7 Kxd7 119. g8=Q Kc7 120. Qa8 Kb6 121. Qb8+ Kc6 122. Ke6 Kc5 123. Qd6+ Kb5 124. Kf5 Kc4 125. Kg4 Kb5 126. Kh3 Ka4 127. Kh2 Kb5 128. Kg1 Kc4 129. Qe5 Kb3 130. Qa1 Kb4 131. Qa2 Kb5 132. Qe6 Kb4 133. Qe1+ Kb5 134. Qe6 Kb4 135. Qf5 Kb3 136. Qa5 Kb2 137. Qa4 Kc3 138. Qd1 Kc4 139. Qh5 Kb3 140. Qa5 Kb2 141. Qa4 Kc3 142. Qd1 Kb2 143. Qf1 Ka2 144. Qc1 Kb3 145. Qa1 Kb4 146. Qe5 Kb3 147. Kh1 Ka3 148. Kh2 Kb4 149. Kh1 Ka3 150. Kh2 Ka2 151. Qc3 Kb1 152. Qa3 Kc2 153. Qb4 Kd3 154. Qc5 Ke4 155. Qg5 Kd4 156. Kg1 Kc3 157. Qc1+ Kb4 158. Qa1 Kb5 159. Qf6 Kc5 160. Qa6 Kb4 161. Qa1 Kb5 162. Qf6 Kc5 163. Qa6 Kb4 164. Qb6+ Ka3 165. Qa7+ Kb4 166. Qb6+ Ka3 167. Qd4 Kb3 168. Qa7 Kb4 169. Qa2"

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

(*Tests tri*)
let tri1 = "r1b1r1k1/p1p2ppp/2n5/1pbqp3/Q7/3P1NP1/PP2PPBP/R1B2RK1 w - - 0 11"
let tri2 = "rnbqkb1r/1ppp1ppp/p4n2/3Pp3/5B2/8/PPP1PPPP/RN1QKBNR w KQkq e6 0 4"

(*Tests SEE*)
let see1 = "3r3k/1b2n3/2b5/3N4/2Q1B3/8/8/3R2K1 b - - 0 1" (* *)
let see2 = "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1" (*e5*)
let see3 = "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1" (*e5*)
let see4 = "6k1/1pp4p/p1pb4/6q1/3P1pRr/2P4P/PP1Br1P1/5RKN w - - 0 1" (*f4*)
let see5 = "5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1" (*f4*)
let see6 = "4R3/2r3p1/5bk1/1p1r3p/p2PR1P1/P1BK1P2/1P6/8 b - - 0 1" (*g4*)
let see7 = "4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - - 0 1"
let see8 = "4r1k1/5pp1/nbp4p/1p2p2q/1P2P1b1/1BP2N1P/1B2QPPK/3R4 b - - 0 1"
let see9 = "2r1r1k1/pp1bppbp/3p1np1/q3P3/2P2P2/1P2B3/P1N1B1PP/2RQ1RK1 b - - 0 1"
let see10 = "7r/5qpk/p1Qp1b1p/3r3n/BB3p2/5p2/P1P2P2/4RK1R w - - 0 1"
let see11 = "6rr/6pk/p1Qp1b1p/2n5/1B3p2/5p2/P1P2P2/4RK1R w - - 0 1"
let see12 = "7r/5qpk/2Qp1b1p/1N1r3n/BB3p2/5p2/P1P2P2/4RK1R w - - 0 1"
let see13 = "6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - - 0 1"
let see14 = "6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - - 0 1"
let see15 = "7R/5P2/8/8/6r1/3K4/5p2/4k3 w - - 0 1"
let see16 = "7R/5P2/8/8/6r1/3K4/5p2/4k3 w - - 0 1"
let see17 = "7R/4bP2/8/8/1q6/3K4/5p2/4k3 w - - 0 1"
let see18 = "8/4kp2/2npp3/1Nn5/1p2PQP1/7q/1PP1B3/4KR1r b - - 0 1"
let see19 = "8/4kp2/2npp3/1Nn5/1p2P1P1/7q/1PP1B3/4KR1r b - - 0 1"
let see20 = "rnbq1b1r/pppppkPp/8/3n4/3P4/8/PPP2PPP/RNBQKBNR w KQ - 0 1"
let see21 = "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 0 1"
let see22 = "rnbqkb1r/pppp1ppp/4pn2/7P/8/8/PPPPPPP1/RNBQKBNR w KQkq - 0 1"
let see23 = "rnb1kbnr/ppppqppp/4p3/8/3P4/2N2N2/PPP1PPP1/R1BQKB1R w KQkq - 0 1"
let see24 = "rn1qkbnr/Qpp1pppp/3pb3/8/4P3/5N2/PPP2PPP/RNB1KB1R b KQkq - 0 1"
let see25 = "r1bqkbnr/pppN1ppp/8/3p4/8/2N5/PPPPPPPP/R1BQKB1R w KQkq - 0 1"

(*Perft*)
let position3 = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
let position4 = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
let position5 = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
let position6 = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
let position7 = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1"
let position8 = "4k3/8/8/8/8/8/8/4K2R w K - 0 1"
let position9 = "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1"
let position10 = "r3k1r1/8/8/8/8/8/8/R3K2R b KQq - 0 1"
let position11 = "R6r/8/8/2K5/5k2/8/8/r6R b - - 0 1"
let position12 = "8/PPPk4/8/8/8/8/4Kppp/8 w - - 0 1"
let position13 = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N w - - 0 1"
let position14 = "8/Pk6/8/8/8/8/6Kp/8 b - - 0 1"
let position15 = "n1n5/1Pk5/8/8/8/8/5Kp1/5N1N b - - 0 1"
let position16 = "8/PPPk4/8/8/8/8/4Kppp/8 b - - 0 1"
let position17 = "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1"
let perft_test = [standard; kiwipete; position3; position4; position5; position6; position7; position8; position9; position10; position11; position12; position13; position14; position15; position16; position17]