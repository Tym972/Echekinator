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
let lasker = "8/k7/3p4/p2P1p2/P2P1P2/8/8/K7 w - -"

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