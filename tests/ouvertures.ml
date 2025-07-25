(*Module contenant des répertoires d'ouvertures*)

open Libs.Of_algebraic
open Libs.Board

let chess_openings_exhaustif =
  "1 a3 a5 2 b4
  1 a3 e5 2 b3 d5 3 c3 Nf6 4 d3 Nc6 5 e3 Bd6 6 f3 0-0 7 g3
  1 a3 e5 2 g3 d5 3 Bg2 Nf6 4 d3 Nc6 5 Nd2 Bd6 6 e3 0-0 7 h3
  1 a3 e5 2 h3 d5
  1 a3 g6 2 g4
  1 a4 b5 (2 axb5 Bb7)
  1 a4 b6 2 d4 d5 3 Nc3 Nd7
  1 a4 d5 2 Nf3
  1 a4 e5 2 a5 d5 3 e3 f5 4 a6
  1 a4 e5 2 e4
  1 a4 e5 2 h4
  1 a4 e5 2 Ra3
  1 b3 b5
  1 b3 b6
  1 b3 c5
  1 b3 d5 2 Ba3
  1 b3 d5 2 Nf3
  1 b3 e5 2 Bb2 Nc6 3 e3
  1 b3 e5 2 Bb2 Nc6 3 f4
  1 b3 e6 2 Bb2 f5 3 e4
  1 b3 f5 2 Bb2 e6 3 e4
  1 b3 Nf6 2 Bb2 g6 3 g4
  1 b4 b5
  1 b4 c5
  1 b4 c6 2 Bb2 a5 3 b5 cxb5 4 e4
  1 b4 c6 2 Bb2 Qb6 3 a3 a5 4 c4 d6
  1 b4 d5 2 Bb2 Bf5
  1 b4 d5 2 Bb2 c6 3 a4
  1 b4 d5 2 Bb2 d4 3 Nf3 c5
  1 b4 d5 2 Bb2 Qd6 3 a3 e5 4 e4 dxe4 5 f3
  1 b4 e5 2 a3
  1 b4 e5 2 Bb2 Bd6
  1 b4 e5 2 Bb2 c5
  1 b4 e5 2 Bb2 d6
  1 b4 e5 2 Bb2 f6 3 b5
  1 b4 e5 2 Bb2 f6 3 e4 Bxb4 4 Bc4 Nc6 5 f4 Qe7 6 f5 g6
  1 b4 e5 2 e4 Bxb4 3 f4 exf4
  1 b4 e6 2 Bb2 f5 3 e4
  1 b4 f5
  1 b4 Nc6
  1 b4 Nf6 2 Bb2 d5 3 e3 e6 4 b5
  1 b4 Nf6 2 Bb2 e6 3 a3 c6 4 d3 a5 5 bxa5 d5 6 e4
  1 b4 Nf6 2 Bb2 e6 3 b5 a6 4 a4 axb5 5 axb5 Rxa1 6 Bxa1
  1 b4 Nf6 2 Bb2 e6 3 b5 b6
  1 b4 Nf6 2 Bb2 g6 3 c4 Bg7 4 e3 d6 5 Nf3 0-0 6 d4
  1 b4 Nf6 2 Bb2 g6 3 e4
  1 b4 Nf6 2 Bb2 g6 3 g4
  1 b4 Nf6 2 Nf3
  1 b4 Nh6
  1 c3 e5 2 a3 d5 3 b3 Nf6 4 Bb2 Nc6 5 a4 Bd6 6 g3 0-0 7 e3
  1 c3 e5 2 e4 Nc6 3 f4
  1 c3 Nf6 2 Qa4 e6 3 f3 Bc5 4 Qh4 Be7 5 Kd1 Nc6 6 Qe1 b6 7 e4 Bb7 8 d4 0-0 9 Bd3 a6
  1 c4 b5
  1 c4 b6 2 d4 e6
  1 c4 c5 2 b4
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 e3 e5
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 e4
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 Nf3 e5
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 Nf3 Nf6 6 0-0 0-0 7 b3
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 Nf3 Nf6 6 0-0 0-0 7 d3
  1 c4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 Nf3 Nf6 6 0-0 0-0 7 d4
  1 c4 c5 2 Nc3 Nc6 3 Nf3 Nf6
  1 c4 c5 2 Nc3 Nf6 3 g3 d5 4 cxd5 Nxd5 (5 Bg2 Nc7)
  1 c4 c5 2 Nc3 Nf6 3 Nf3 d5 4 cxd5 Nxd5 5 g3 Nc6 6 Bg2 e6 7 0-0 Be7
  1 c4 c5 2 Nf3 Nf6 3 b4
  1 c4 c5 2 Nf3 Nf6 3 d4 cxd4 4 Nxd4 e6 5 Nc3 Nc6 6 g3 Qb6
  1 c4 c5 2 Nf3 Nf6 3 g3 b6 4 Bg2 Bb7 5 0-0 e6 6 Nc3 Be7 7 d4 cxd4 8 Qxd4 d6 9 Rd1 a6 10 b3 Nbd7
  1 c4 c6 2 Nf3 d5 3 b3 Bg4
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 Bb2 Bf5
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 Bb2 Bg4
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 Bb2 g6
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 g3 Bf5 5 Bg2 e6 6 Bb2 Nbd7
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 g3 Bg4
  1 c4 c6 2 Nf3 d5 3 b3 Nf6 4 g3 g6
  1 c4 c6 2 Nf3 d5 3 g3 Nf6 4 Bg2 g6
  1 c4 d5 2 cxd5 c6
  1 c4 d5 2 cxd5 e6
  1 c4 d5 2 cxd5 Nf6
  1 c4 d5 2 cxd5 Qxd5 3 Nc3 Qa5
  1 c4 e5 2 e3 Nf6 3 f4 exf4 4 Nf3
  1 c4 e5 2 f4 exf4 3 Nf3 g5 4 g3
  1 c4 e5 2 g3 h5
  1 c4 e5 2 Nc3 Bb4
  1 c4 e5 2 Nc3 d6 3 g3 Be6 4 Bg2 Nc6
  1 c4 e5 2 Nc3 d6 3 g3 c6
  1 c4 e5 2 Nc3 d6 3 Nf3 Bg4
  1 c4 e5 2 Nc3 Nc6 3 e3 Nf6 4 b3 d6
  1 c4 e5 2 Nc3 Nc6 3 f4
  1 c4 e5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Be7
  1 c4 e5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 e4
  1 c4 e5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 e3 d6 6 Nge2 Be6
  1 c4 e5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 e3 d6 6 Nge2 Nh6
  1 c4 e5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 Rb1 Nh6
  1 c4 e5 2 Nc3 Nc6 3 Nf3 f5
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 a3
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 d3
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 d4 e4
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Bg5 h6 (7 Bh4 Bxc3+ 8 bxc3 Ne5)
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 e3 Bb4 5 Qc2 0-0 6 Nd5 Re8 7 Qf5
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 e3 Bb4 5 Qc2 Bxc3
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 e4
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 g3
  1 c4 e5 2 Nc3 Nf6 3 f4
  1 c4 e5 2 Nc3 Nf6 3 g3 Bb4
  1 c4 e5 2 Nc3 Nf6 3 g3 c6
  1 c4 e5 2 Nc3 Nf6 3 g3 d5
  1 c4 e5 2 Nc3 Nf6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 e4 0-0 7 Nge2 c6 8 0-0 a6
  1 c4 e5 2 Nc3 Nf6 3 g3 g6 4 Bg2 Bg7 5 e3 d6 6 Nge2 Be6
  1 c4 e5 2 Nc3 Nf6 3 Nf3 e4 4 Ng5 b5
  1 c4 e5 2 Nc3 Nf6 3 Nf3 e4 4 Ng5 Ng4
  1 c4 e5 2 Nf3 e4
  1 c4 e5 2 Qa4
  1 c4 e6 2 d4 b6 3 e4 Bb7 4 Bd3 Nc6
  1 c4 e6 2 d4 b6 3 e4 Bb7 4 f3 f5 5 exf5 Nh6
  1 c4 e6 2 d4 b6 3 Nc3 Bb7 4 e4 f5 5 d5
  1 c4 e6 2 d4 b6 3 Nc3 Bb7 4 e4 f5 5 exf5 Nf6
  1 c4 e6 2 d4 e5
  1 c4 e6 2 Nf3 d5 3 b3 Nf6 4 Bb2 c5 5 e3
  1 c4 e6 2 Nf3 d5 3 g3 b6 4 Bg2 Bb7 5 0-0
  1 c4 e6 2 Nf3 d5 3 g3 c5
  1 c4 e6 2 Nf3 d5 3 g3 c6
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 Bd6
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 Be7 5 0-0 c5 6 cxd5 Nxd5 7 Nc3 Nc6
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 Be7 5 b3 0-0 6 Bb2 a5
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 c5 5 b3 Nc6 6 0-0 Be7
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 c6
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 dxc4
  1 c4 e6 2 Nf3 Nf6 3 g3 a6 (4 Bg2 b5)
  1 c4 f5 2 b4
  1 c4 f5 2 e4 fxe4 3 Nc3 Nf6 4 g4
  1 c4 f5 2 g4
  1 c4 f5 2 Nc3 Nf6 3 e4
  1 c4 f5 2 Nf3 d6 3 e4
  1 c4 g5 2 d4 Bg7
  1 c4 g5 2 d4 e5
  1 c4 g6 2 e4 e5
  1 c4 Nc6
  1 c4 Nf6 2 b4
  1 c4 Nf6 2 e4
  1 c4 Nf6 2 g4
  1 c4 Nf6 2 Nc3 d5 3 cxd5 Nxd5 4 g3 c5 5 Bg2 Nc7
  1 c4 Nf6 2 Nc3 d5 3 cxd5 Nxd5 4 g3 g6 5 Bg2 Nb6
  1 c4 Nf6 2 Nc3 d5 3 cxd5 Nxd5 4 g3 g6 5 Bg2 Nxc3
  1 c4 Nf6 2 Nc3 d5 3 cxd5 Nxd5 4 Nf3 g6 5 g3 Bg7 6 Bg2 e5
  1 c4 Nf6 2 Nc3 e6 3 e4 c5 4 e5 Ng8
  1 c4 Nf6 2 Nc3 e6 3 e4 d5 4 e5
  1 c4 Nf6 2 Nc3 e6 3 e4 e5
  1 c4 Nf6 2 Nc3 e6 3 e4 Nc6
  1 c4 Nf6 2 Nc3 e6 3 Nf3 b6 4 e4 Bb7 5 Bd3
  1 c4 Nf6 2 Nc3 e6 3 Nf3 Bb4 4 g4
  1 c4 Nf6 2 Nc3 g6 3 Nf3 Bg7 4 e4
  1 c4 Nf6 2 Nf3 b6
  1 c4 Nf6 2 Nf3 d5 3 cxd5 Nxd5
  1 c4 Nf6 2 Nf3 d6
  1 c4 Nf6 2 Nf3 e6 3 g3 a6 4 Bg2 b5
  1 c4 Nf6 2 Nf3 e6 3 g3 b6 4 Bg2 Bb7
  1 c4 Nf6 2 Nf3 g6 3 g3 b6 4 Bg2 Bb7
  1 c4 Nf6 2 Nf3 g6 3 g3 Bg7 4 Bg2 0-0
  1 c4 Nf6 2 Nf3 g6 3 g3 c6
  1 c4 Nf6 2 Nf3 g6 3 g3 d5
  1 d3 c5 2 Nc3 Nc6 3 g3
  1 d3 d5 2 Nc3 Nc6 3 g3
  1 d3 e5 2 Bd2
  1 d3 e5 2 Nd2
  1 d3 g6 2 g4
  1 d4 a6 2 c4 b5 3 e4 e6 4 cxb5 axb5
  1 d4 b5 2 e4 Bb7 3 Bxb5
  1 d4 b6 2 c4 Bb7 3 Nc3 e5
  1 d4 b6 2 c4 c5 3 d5 e6 4 e4 b5 5 cxb5 f5
  1 d4 c5 2 b4
  1 d4 c5 2 c4 cxd4 3 e3
  1 d4 c5 2 c4 e6
  1 d4 c5 2 d5 d6 3 Nc3 g6
  1 d4 c5 2 d5 e5 3 e4 d6
  1 d4 c5 2 d5 e6 3 e4
  1 d4 c5 2 d5 f5 3 e4
  1 d4 c5 2 d5 Na6
  1 d4 c5 2 d5 Nf6 3 c4 Ne4
  1 d4 c5 2 d5 Nf6 3 Nc3 Qa5
  1 d4 c5 2 d5 Nf6 3 Nf3 c4
  1 d4 c5 2 dxc5 b6
  1 d4 c5 2 dxc5 Na6
  1 d4 c5 2 Nf3 cxd4 3 b4 e5
  1 d4 c6 2 c4 d6
  1 d4 d5 2 Bf4 c5 3 e4
  1 d4 d5 2 Bg5 Bg4
  1 d4 d5 2 Bg5 h6 3 Bh4 c6 4 Nf3 Qb6
  1 d4 d5 2 c4 b5
  1 d4 d5 2 c4 Bf5 3 cxd5 Bxb1 4 Qa4+ c6 5 dxc6 Nxc6 6 Rxb1
  1 d4 d5 2 c4 Bf5 3 Nc3 e6 4 Nf3 c6
  1 d4 d5 2 c4 Bf5 3 Nc3 e6 4 Nf3 Nc6
  1 d4 d5 2 c4 Bf5 3 Nc3 e6 4 Qb3
  1 d4 d5 2 c4 Bf5 3 Qb3
  1 d4 d5 2 c4 c5 3 cxd5 Nf6 4 e4 Nxe4 5 dxc5 Qa5+
  1 d4 d5 2 c4 c5 3 dxc5 d4
  1 d4 d5 2 c4 c6 3 cxd5
  1 d4 d5 2 c4 c6 3 e4
  1 d4 d5 2 c4 c6 3 Nc3 dxc4 4 e4
  1 d4 d5 2 c4 c6 3 Nc3 e5 4 e4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Bg5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 cxd5 cxd5 5 Nc3 Nc6 6 Bf4 Bf5 7 e3 e6 8 Qb3 Bb4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bf5 5 cxd5 cxd5 6 Nc3 e6 7 Ne5 Nfd7
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bf5 5 cxd5 cxd5 6 Qb3 Qc8 7 Bd2 e6 8 Na3
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bf5 5 Nbd2 e6 6 Be2 Bd6 7 c5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bg4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nbd2
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 a6 5 c5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 e3 e6 7 Bxc4 Bb4 8 0-0 0-0 9 Qe2 Ne4 10 g4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 e3 Na6
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Ne5 e6 7 f3 Bb4 8 e4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Ne5 Na6 7 e4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Ne5 Nbd7 7 Nxc4 Qc7 8 g3 e5 9 dxe5 Nxe5 10 Bf4 Nfd7 11 Bg2 g5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Nh4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bg4 6 e3 b5 7 Bd2 a5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 e6
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Na6 (6 e4 Bg4)
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 e3
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 e4 b5 6 e5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 g6
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 Qb6
  1 d4 d5 2 c4 dxc4 3 e3 e5 4 Bxc4 exd4 5 Qb3 Qe7 6 a3
  1 d4 d5 2 c4 dxc4 3 e3 e5 4 Bxc4 exd4 5 Qb3 Qe7 6 Kf1
  1 d4 d5 2 c4 dxc4 3 e3 e5 4 Bxc4 exd4 5 Qb3 Qe7 6 Nd2
  1 d4 d5 2 c4 dxc4 3 e3 e5 4 Bxc4 exd4 5 Qb3 Qe7 6 Nf3
  1 d4 d5 2 c4 dxc4 3 e4 b5
  1 d4 d5 2 c4 dxc4 3 e4 c5 4 d5 b5
  1 d4 d5 2 c4 dxc4 3 e4 c5 4 d5 Nf6 5 Nc3 b5
  1 d4 d5 2 c4 dxc4 3 e4 e5 4 Bxc4
  1 d4 d5 2 c4 dxc4 3 e4 f5
  1 d4 d5 2 c4 dxc4 3 e4 Nc6
  1 d4 d5 2 c4 dxc4 3 e4 Nf6
  1 d4 d5 2 c4 dxc4 3 Nc3 c5 4 d5 Nf6 5 Nf3 e6 6 e4 exd5 7 e5
  1 d4 d5 2 c4 dxc4 3 Nf3 a6 4 e3 b5
  1 d4 d5 2 c4 dxc4 3 Nf3 a6 4 e3 Bg4 5 Bxc4 e6 6 d5
  1 d4 d5 2 c4 dxc4 3 Nf3 a6 4 e4
  1 d4 d5 2 c4 dxc4 3 Nf3 b5
  1 d4 d5 2 c4 dxc4 3 Nf3 c5 4 d5 Nf6 5 Nc3 e6 6 e4 exd5 7 e5
  1 d4 d5 2 c4 dxc4 3 Nf3 e6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nd7
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 Be6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 Bg4
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 a4
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 dxc5 Bxc5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 e4
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 b5 8 Bb3 Bb7 9 Rd1 Nbd7 10 Nc3 Bd6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 b5 8 Bb3 Nc6 9 Rd1 c4 10 Bc2 Nb4 11 Nc3 Nxc2 12 Qxc2 Bb7 13 d5 Qc7
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 Nc6 8 dxc5 Bxc5 9 e4 b5 10 e5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 Nc6 8 Rd1 b5 9 Bb3 c4 10 Bc2 Nb4 11 Nc3 Nxc2 12 Qxc2 Bb7 13 d5 Qc7
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 cxd4
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 Nc6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 Qe2 a6 7 dxc5 Bxc5 8 0-0 Nc6 9 e4 b5 10 e5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 g6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 Nc3 a6 5 e4
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 Qa4+
  1 d4 d5 2 c4 dxc4 3 Qa4+
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 e3 Bb4+ 5 Bd2 dxe3
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 c5
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 Nc6 5 g3
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 Nc6 5 Nbd2 Bg4 5 h3 Bxf3 7 Nxf3 Bb4+ 8 Bd2 Qe7
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 Nc6 5 Nbd2 f6
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 Nc6 5 Nbd2 Qe7
  1 d4 d5 2 c4 e6 3 Nc3 a6
  1 d4 d5 2 c4 e6 3 Nc3 b6
  1 d4 d5 2 c4 e6 3 Nc3 Be7 4 e4 dxe4 5 f3
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 cxd4 5 Qxd4 Nc6 6 Qd1 exd5 7 Qxd5 Be6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 dxc5 d4 6 Na4 b5
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 e4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 dxc5 d4 7 Na4 b5
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 c4 7 e4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 Be6 10 Rc1 b6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 Be6 10 Rc1 c4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 c4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 h6 11 Be3 Bg4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 h6 11 Be3 Re8 12 Rc1 Be6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 Re8
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 dxc5 Bxc5 10 Na4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 dxc5 d4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Bg4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 e3 Nf6 5 Nf3 Nc6
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 e3 f5 5 g4
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 e4 dxe4 5 f3
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 e4 dxe4 5 Nxe4 Bb4+ 6 Bd2 Qxd4 7 Bxb4 Qxe4+ 8 Be2 c5 9 Bxc5 Qxg2
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 e4 dxe4 5 Nxe4 Bb4+ 6 Nc3
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 (7 Bd2 a5) (8 axb5 Bxc3 9 Bxc3 cxb5 10 b3 Bb7)
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 7 Bd2 Qb6
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 7 Bd2 Qe7
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 Bg5 f6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bf4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 Bxf6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 b6 7 Bd3 Bb7 8 cxd5 exd5 9 Ne5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 b6 8 cxd5 exd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 b6 8 cxd5 Nxd5 9 Bxe7 Qxe7 10 Nxd5 exd5 11 Rc1 Be6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 cxd5 Nxc3 10 bxc3 exd5 11 Qb3 Qd6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 cxd5 Nxc3 10 bxc3 exd5 11 Qb3 Rd8 12 c4 Be6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 Qc2 Nf6 10 Bd3 dxc4 11 Bxc4 c5 12 0-0 Nc6 13 Rfd1 Bd7
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 Rc1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bxf6 Bxf6 8 Rc1 c6 9 Bd3 Nd7 10 0-0 dxc4 11 Bxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Bd3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Qb3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Qc2 c5 8 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 a6 8 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Bb5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Bd3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Qa4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 b5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 dxe5 Nxe5 14 Nxe5 Qxe5 15 f4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 Qb1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 Qc2
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 Ne4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 h4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 a6 9 a3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 a6 9 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 Ne4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Rc1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 Ne4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 Nf3 0-0 6 Qc2
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 Nf3 c6 6 e4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 Nf3 h6 6 Bh4 0-0 7 Rc1 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 c5 5 cxd5 Qb6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 c5 5 Nf3 cxd4 6 Nxd4 e5 7 Ndb5 a6 8 Qa4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 c5 5 Nf3 cxd4 6 Qxd4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 Bb4 6 Nf3 c5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 a3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 Bxf6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 cxd5 Nxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 Nd2 Bb4 8 Qc2 0-0 9 Bh4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 Nd2 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 Nf3 Bb4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 Nf3 c6 6 e3 Qa5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 Nf3 c6 6 e4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 Nf3 c6 6 Rc1 Qa5 7 Bd2
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 Nf3 h6 6 Bh4 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 cxd5 exd5 5 Bg5 Be7 6 e3 0-0 7 Bd3 Nbd7 8 Qc2 Re8 9 Nge2 Nf8 10 0-0-0
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 cxd5 exd5 5 Bg5 c6 6 Qc2
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 cxd5 exd5 5 Nf3 Nbd7 6 Bf4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Bb4 5 Bg5 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Bb4 5 Bg5 h6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Bb4 5 Qa4+
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 b6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 c5 7 dxc5 Bxc5 8 Qc2 Nc6 9 a3 Qa5 10 0-0-0
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 c5 7 dxc5 Bxc5 8 Qc2 Nc6 9 a3 Qa5 10 Rd1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 c6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 Nbd7 7 c5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 Bg5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 cxd5 Nxd5 6 e3 Nc6 7 Bd3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 cxd5 Nxd5 6 e4 Nxc3 7 bxc3 cxd4 8 cxd4 Bb4+ 9 Bd2 Bxd2+ 10 Qxd2 0-0 11 Bb5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 cxd5 Nxd5 6 e4 Nxc3 7 bxc3 cxd4 8 cxd4 Bb4+ 9 Bd2 Qa5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 e3 Nc6 6 Bd3 Bd6 7 0-0 0-0 8 Qe2 Qe7 9 dxc5 Bxc5 10 e4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 exf6 gxh4 10 Ne5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 hxg5 10 Bxg5 Nbd7 11 g3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 hxg5 10 Bxg5 Nbd7 11 Qf3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 Nd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 h6 6 Bh4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 h6 6 Bxf6 Qxf6 7 Qb3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 a6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 Bb4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 Bd6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 Be7
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 b4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 d5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Ng4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Nxe5 12 Nxe5 axb5 13 0-0 Qd5 14 Qe2 Ba6 (15 Bg5)
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Nxe5 12 Nxe5 axb5 13 Qf3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 b4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 Bb7 9 e4 b4 10 Na4 c5 11 e5 Nd5 12 0-0 cxd4 13 Nxd4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Ne5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Qc2 Bd6 7 e4 dxe4 8 Nxe4 Nxe4 9 Qxe4 e5 10 dxe5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Qc2 Bd6 7 g4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Ne4 6 Bd3 f5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 dxc4 5 e3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Nbd7 5 cxd5
  1 d4 d5 2 c4 e6 3 Nf3 c5 4 cxd5 exd5 5 Bg5
  1 d4 d5 2 c4 e6 3 Nf3 c6 4 Nc3 dxc4 5 g3
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 Bg5 Bb4+
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 Bg5 h6 5 Bxf6 Qxf6 6 Nc3 c6 7 Qb3
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nbd2
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 e3 c6 5 Nbd2 g6
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 e3 c6 5 Nbd2 Nbd7 6 Bd3 c5
  1 d4 d5 2 c4 e6 3 Nf3 Nf6 4 e3 c6 5 Nbd2 Ne4 6 Bd3 f5
  1 d4 d5 2 c4 g6
  1 d4 d5 2 c4 Nc6 3 cxd5 Qxd5 4 e3 e5 5 Nc3 Bb4 6 Bd2 Bxc3 7 Bxc3 exd4 8 Ne2
  1 d4 d5 2 c4 Nc6 3 Nc3 dxc4 4 Nf3
  1 d4 d5 2 c4 Nc6 3 Nc3 e5
  1 d4 d5 2 c4 Nc6 3 Nc3 Nf6 4 Nf3 dxc4
  1 d4 d5 2 c4 Nc6 3 Nf3 Bg4 4 Qa4
  1 d4 d5 2 c4 Nc6 3 Nf3 e5
  1 d4 d5 2 c4 Nf6 3 cxd5 c6
  1 d4 d5 2 e3 c6 3 Bd3
  1 d4 d5 2 e3 e6 3 Bd3 c5
  1 d4 d5 2 e3 Nf6 3 Bd3
  1 d4 d5 2 e4 dxe4 3 Bc4
  1 d4 d5 2 e4 dxe4 3 Be3
  1 d4 d5 2 e4 dxe4 3 f3 exf3 4 Nxf3
  1 d4 d5 2 e4 dxe4 3 Nc3 Bd7
  1 d4 d5 2 e4 dxe4 3 Nc3 Bf5 4 f3 Nf6 5 Bc4
  1 d4 d5 2 e4 dxe4 3 Nc3 e5 4 Be3
  1 d4 d5 2 e4 dxe4 3 Nc3 e5 4 dxe5
  1 d4 d5 2 e4 dxe4 3 Nc3 e5 4 Nge2
  1 d4 d5 2 e4 dxe4 3 Nc3 e5 4 Nxe4
  1 d4 d5 2 e4 dxe4 3 Nc3 e5 4 Qh5
  1 d4 d5 2 e4 dxe4 3 Nc3 f5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 Be3
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 Bg5 Bf5 5 Bxf6 exf6 6 g4 Bg6 7 Qe2 Bb4 8 Qb5+
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 Bf5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 c5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 c6
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 e3
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 e5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 e6
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 b6
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 Bf5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 Bg4
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 c5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 c6
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 e6 6 Bg5 Be7 7 Bd3 Nc6 8 0-0 Nxd4 9 Kh1
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Kh1
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Qe1
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 h4
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 Ne5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 h5
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 Nbd7
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 Nc6
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Qxf3
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 Nc6
  1 d4 d5 2 f4
  1 d4 d5 2 g3
  1 d4 d5 2 g4
  1 d4 d5 2 Nc3 b6 3 a4 Nd7
  1 d4 d5 2 Nc3 Bf5
  1 d4 d5 2 Nc3 Bg4
  1 d4 d5 2 Nc3 c5
  1 d4 d5 2 Nc3 e5
  1 d4 d5 2 Nc3 f5
  1 d4 d5 2 Nc3 g6 3 Nf3 Bg7
  1 d4 d5 2 Nc3 h5
  1 d4 d5 2 Nc3 Nf6 3 Bg5 Bf5 4 Bxf6
  1 d4 d5 2 Nc3 Nf6 3 Bg5 Bf5 4 f3
  1 d4 d5 2 Nc3 Nf6 3 Bg5 e6 4 Nf3
  1 d4 d5 2 Nc3 Nf6 3 Bg5 Nbd7 4 Nf3 g6
  1 d4 d5 2 Nc3 Nf6 3 Bg5 Ne4
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 Bg5
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 Bf5
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Nxf3 Bg4
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Nxf3 c6
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Nxf3 e6
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Qe1
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Qxf3
  1 d4 d5 2 Nc3 Nf6 3 e4 e5
  1 d4 d5 2 Nc3 Nf6 3 e4 Nxe4
  1 d4 d5 2 Nc3 Nf6 3 f3
  1 d4 d5 2 Nf3 Bg4
  1 d4 d5 2 Nf3 c5 3 g3 cxd4 4 Bg2
  1 d4 d5 2 Nf3 c6 3 c4 e6 4 Nc3 dxc4 5 g3
  1 d4 d5 2 Nf3 Nc6
  1 d4 d5 2 Nf3 Nf6 3 Bf4 c5 4 e3 Qb6 5 Nc3
  1 d4 d5 2 Nf3 Nf6 3 Bg5 e6 4 e3 c5 5 c3 Qb6
  1 d4 d5 2 Nf3 Nf6 3 Bg5 g6 4 e3 Bg7 5 Nbd2 0-0
  1 d4 d5 2 Nf3 Nf6 3 Bg5 Ne4
  1 d4 d5 2 Nf3 Nf6 3 c4 b5
  1 d4 d5 2 Nf3 Nf6 3 c4 dxc4
  1 d4 d5 2 Nf3 Nf6 3 e3 Bf5 4 Bd3 e6
  1 d4 d5 2 Nf3 Nf6 3 e3 c5 4 c3
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 c5 5 b3 Nc6 6 Bb2 Bd6 7 0-0 0-0
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 c5 5 b3 Nc6 6 Bb2 Be7 7 0-0 0-0
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 c5 5 c3
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 Nbd7 5 0-0 Bd6 6 b3 0-0 7 Bb2
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Nbd2 c5 5 b3
  1 d4 d5 2 Nf3 Nf6 3 g3
  1 d4 d5 2 Qd3 Nf6 3 Nc3
  1 d4 d6 2 c4 c6
  1 d4 d6 2 c4 e5 3 dxe5 Be6
  1 d4 d6 2 c4 e5 3 dxe5 Nc6
  1 d4 d6 2 c4 g6 3 Nc3 Bg7 4 e4 c5 5 Nf3 Qa5
  1 d4 d6 2 c4 g6 3 Nc3 Bg7 4 e4 f5
  1 d4 d6 2 c4 g6 3 Nc3 Bg7 4 e4 Nc6
  1 d4 d6 2 c4 g6 3 Nf3 Bg7 4 e4 Bg4
  1 d4 d6 2 Nf3 Bg4 3 c4 Nd7 4 Qb3 Rb8
  1 d4 e5 2 d5 Bc5 3 e4 Qh4
  1 d4 e5 2 dxe5 d6 3 Bf4
  1 d4 e5 2 dxe5 f6
  1 d4 e5 2 dxe5 Nc6 3 Nf3 Bc5
  1 d4 e5 2 dxe5 Nc6 3 Nf3 f6
  1 d4 e5 2 dxe5 Nc6 3 Nf3 h6
  1 d4 e5 2 dxe5 Nc6 3 Nf3 Nge7
  1 d4 e5 2 dxe5 Nc6 3 Nf3 Qe7 4 Qd5
  1 d4 e5 2 dxe5 Qh4
  1 d4 e5 2 e3
  1 d4 e5 2 Nf3 e4 3 Ne5
  1 d4 e5 2 Nf3 e4 3 Ng1
  1 d4 e6 2 Bf4 f5 3 g4
  1 d4 e6 2 c4 b6 3 e4 Bb7 4 Bd3 Nc6
  1 d4 e6 2 c4 b6 3 e4 Bb7 4 f3 f5
  1 d4 e6 2 c4 b6 3 e4 Bb7 4 Nc3 f5 5 exf5 Nf6
  1 d4 e6 2 c4 Bb4+ 3 Nc3
  1 d4 e6 2 c4 c5 3 d5 exd5 4 cxd5 d6 5 Nc3 g6 6 e4 Bg7 7 Nf3 Ne7
  1 d4 e6 2 c4 f5 3 e4
  1 d4 e6 2 e4 c5
  1 d4 f5 2 Bf4 e6 3 g4
  1 d4 f5 2 Bg5
  1 d4 f5 2 c4 e6 3 e4
  1 d4 f5 2 c4 e6 3 Nc3
  1 d4 f5 2 c4 e6 3 Nf3 Nf6 4 g3 c6 5 Bg2 d5 6 0-0 Bd6
  1 d4 f5 2 c4 g6 3 Nc3 Nh6
  1 d4 f5 2 c4 Nf6 3 g3 d6 4 Bg2 c6 5 Nc3 Qc7
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Bb4+ 5 Bd2 Be7
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d5 7 b3 c6 8 Ba3
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d5 7 Nc3 c6 8 Qc2 Qe8 9 Bg5
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 a5
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Ne4
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 b3
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 Qc2
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 Re1
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Ne4
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nh3
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 d5 5 Nf3 c6 6 0-0 Bd6
  1 d4 f5 2 c4 Nf6 3 g3 g6 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 d6 7 Nc3 c6
  1 d4 f5 2 c4 Nf6 3 g3 g6 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Nc6
  1 d4 f5 2 c4 Nf6 3 g3 g6 4 Bg2 Bg7 5 Nh3
  1 d4 f5 2 c4 Nf6 3 Nc3 d6 (4 Nf3 Nc6)
  1 d4 f5 2 c4 Nf6 3 Nc3 e6 4 Nf3 d5 5 e3 c6 6 Bd3 Ne4
  1 d4 f5 2 c4 Nf6 3 Nf3 g6 4 g3
  1 d4 f5 2 e4 c6
  1 d4 f5 2 e4 d6
  1 d4 f5 2 e4 fxe4 3 f3 exf3 4 Nxf3 Nf6 5 Bd3
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 Bg5 b6
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 Bg5 c6
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 Bg5 g6 5 f3
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 Bg5 g6 5 h4
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 f3
  1 d4 f5 2 e4 fxe4 3 Nc3 Nf6 4 g4
  1 d4 f5 2 e4 fxe4 3 Nd2
  1 d4 f5 2 g3 g6 3 Bg2 Bg7 4 Nf3 c6 5 0-0 Nh6
  1 d4 f5 2 g3 g6 3 Bg2 Bg7 4 Nh3
  1 d4 f5 2 g3 Nf6 3 Bg2 e6 4 Nh3
  1 d4 f5 2 g3 Nf6 3 Bg2 g6
  1 d4 f5 2 g4 e5
  1 d4 f5 2 g4 fxg4 3 e4 d5 4 Nc3
  1 d4 f5 2 h3 Nf6 3 g4
  1 d4 f5 2 Nc3 d5 3 e4
  1 d4 f5 2 Nc3 d5 3 g4 fxg4 4 e4
  1 d4 f5 2 Nc3 Nf6 3 g4
  1 d4 f5 2 Nf3 e5
  1 d4 f5 2 Nf3 e6 3 e4
  1 d4 f5 2 Qd3 d5 3 g4
  1 d4 f5 2 Qd3 d6 3 g4
  1 d4 f5 2 Qd3 e6 3 g4
  1 d4 f5 2 Qd3 g6 3 g4
  1 d4 g5
  1 d4 g6 2 c4 Bg7 3 e4 d5 4 exd5 c6 5 dxc6 Bxd4
  1 d4 g6 2 c4 Bg7 3 e4 d6 4 Be3 Nf6 5 f3
  1 d4 g6 2 c4 Bg7 3 e4 e5
  1 d4 g6 2 c4 Bg7 3 Nc3 c5 4 d5 Bxc3+ 5 bxc3 f5
  1 d4 g6 2 c4 Bg7 3 Nc3 c5 4 d5 Bxc3+ 5 bxc3 Qa5
  1 d4 g6 2 c4 Bg7 3 Nc3 c5 4 d5 Qa5
  1 d4 g6 2 c4 Bg7 3 Nc3 c5 4 e3
  1 d4 g6 2 c4 Bg7 3 Nc3 d6 4 e4 c5 5 Nf3 Qa5
  1 d4 g6 2 c4 Bg7 3 Nc3 d6 4 e4 f5
  1 d4 g6 2 c4 Bg7 3 Nc3 d6 4 e4 Nc6
  1 d4 g6 2 h4 Nf6 3 h5
  1 d4 g6 2 Nf3 Bg7 3 Bf4 c5 4 c3 cxd4 5 cxd4 Qa5+
  1 d4 g6 2 Nf3 Bg7 3 e3 c5 4 Bd3 cxd4 5 Nxd4 Qa5+
  1 d4 g6 2 Nf3 Bg7 3 e3 c5 4 Bd3 Qa5+
  1 d4 g6 2 Nf3 Bg7 3 e3 c5 4 dxc5 Qa5+
  1 d4 g6 2 Nf3 Bg7 3 g3 c5 4 Bg2 Qa5+
  1 d4 Na6
  1 d4 Nc6 2 c4 e5 3 d5 Nce7
  1 d4 Nc6 2 c4 e5 3 d5 Nd4
  1 d4 Nc6 2 c4 e5 3 dxe5 Nxe5 4 Nc3 Nxc4
  1 d4 Nc6 2 d5 Nb8 3 e4 Nf6 4 e5 Ng8
  1 d4 Nc6 2 d5 Ne5 3 e4 e6 4 f4 exd5 5 fxe5 Qh4+
  1 d4 Nf6 2 Bg5 c5 3 d5 Qb6 4 Nc3
  1 d4 Nf6 2 Bg5 e6 3 e4
  1 d4 Nf6 2 Bg5 Ne4 3 Bf4 g5
  1 d4 Nf6 2 Bg5 Ne4 3 Bh4 c6 4 Nd2 Qa5 5 c3 Nxd2 6 Qxd2 d5 7 e4
  1 d4 Nf6 2 Bg5 Ne4 3 Bh4 d5 4 f3 Nf6 5 Nc3 Bf5 6 e4
  1 d4 Nf6 2 Bg5 Ne4 3 h4 Nxg5 4 hxg5 e5
  1 d4 Nf6 2 c4 b5
  1 d4 Nf6 2 c4 b6 3 Nc3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 a4
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 Bg5
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 b6
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 d6 7 e4 Bxf1 8 Kxf1 g6 9 g3 Bg7 10 Kg2 0-0 11 Nf3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 d6 7 Nf3 g6 8 g3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 g6 7 Nf3 d6 8 g3 (Bg7 9 Bg2)
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 g6 6 Nc3 Bxa6 7 f4
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 e3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 f3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 Nc3 axb5 6 e4 b4 7 Nb5 (d6 8 Bc4)
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 e4
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 f3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 g4
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 Nd2
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 Nf3
  1 d4 Nf6 2 c4 c5 3 d5 d6
  1 d4 Nf6 2 c4 c5 3 d5 e5 4 Nc3 d6 5 e4 g6
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 Bd6
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 Bb5+
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 e5
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 Nf3 0-0 9 Be2 Re8
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Be2 0-0 9 0-0 a6 10 a4 Bg4
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Be2 0-0 9 0-0 Re8 10 Nd2 Na6 11 f3
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Bg5
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 h3
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 Be7
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 Bd2
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 Bg5
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 g3 Bg7 8 Bg2 0-0 9 0-0 a6 10 a4 Nbd7 11 Nd2 Re8
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 g3 Bg7 8 Bg2 0-0 9 0-0 Nbd7 10 Nd2 a6 11 a4 Re8
  1 d4 Nf6 2 c4 c5 3 d5 g6
  1 d4 Nf6 2 c4 c5 3 d5 Ne4
  1 d4 Nf6 2 c4 c5 3 dxc5 e6
  1 d4 Nf6 2 c4 c6 3 Nf3 b5
  1 d4 Nf6 2 c4 d6 3 g4
  1 d4 Nf6 2 c4 d6 3 Nc3 Bf5 4 e4
  1 d4 Nf6 2 c4 d6 3 Nc3 Bf5 4 f3
  1 d4 Nf6 2 c4 d6 3 Nc3 Bf5 4 g3
  1 d4 Nf6 2 c4 d6 3 Nc3 Bf5 4 Nf3
  1 d4 Nf6 2 c4 d6 3 Nc3 c6
  1 d4 Nf6 2 c4 d6 3 Nc3 e5 4 e3 Nbd7 5 Bd3
  1 d4 Nf6 2 c4 d6 3 Nc3 e5 4 Nf3 Nbd7 5 e4
  1 d4 Nf6 2 c4 d6 3 Nf3 Bg4
  1 d4 Nf6 2 c4 d6 3 Nf3 c6
  1 d4 Nf6 2 c4 e5 3 dxe5 Ne4 4 a3 b6
  1 d4 Nf6 2 c4 e5 3 dxe5 Ne4 4 Qc2
  1 d4 Nf6 2 c4 e5 3 dxe5 Ng4 4 Bf4
  1 d4 Nf6 2 c4 e5 3 dxe5 Ng4 4 e4 d6
  1 d4 Nf6 2 c4 e5 3 dxe5 Ng4 4 e4 Nxe5 5 f4 Nec6
  1 d4 Nf6 2 c4 e5 3 dxe5 Ng4 4 Nf3
  1 d4 Nf6 2 c4 e6 3 Bg5
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Nc3 c6 8 Qd3
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 b3 b6 9 Rd1 Bb7 10 Nc3 b5
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Nbd2 b5
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Nbd2 b6 9 b3 a5 10 Bb2 Ba6
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Rd1 b6 9 a4
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 c5 5 Nf3 Nc6
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 dxc4 5 Nf3 Be7
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 dxc4 5 Nf3 Nc6 6 Qa4 Bb4+
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 dxc4 5 Qa4+ Nbd7 6 Qxc4 a6 7 Qc2
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Nf3
  1 d4 Nf6 2 c4 e6 3 g3 e5
  1 d4 Nf6 2 c4 e6 3 g4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 0-0 6 e3 c5 7 Bd3 Nc6 8 Ne2 b6 9 e4 Ne8
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 c5 6 e3 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 c5 6 f3 d5 7 cxd5 Nxd5 8 dxc5 f5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 c5 6 f3 d5 7 e3 0-0 8 cxd5 Nxd5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Bg5 h6 5 Bh4 c5 6 d5 a6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Bg5 h6 5 Bh4 c5 6 d5 b5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Bg5 h6 5 Bh4 c5 6 d5 d6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Bd3 d5 6 a3 Bxc3+ 7 bxc3 c5 8 cxd5 exd5 9 Ne2 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 c5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 (6 Bd3 Nc6 7 0-0 dxc4)
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 a3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 b6 9 Qe2 Bb7 10 Rd1 Qc8
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Bd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Nbd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Qe7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nbd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nc6 8 a3 Bxc3 9 bxc3 dxc4 10 Bxc4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nc6 8 a3 dxc4 9 Bxc4 cxd4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nge2 d5 6 a3 Bd6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 b6 5 Nge2 Ba6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 c5 5 Bd3 Nc6 6 Nf3 Bxc3+ 7 bxc3 d6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 c5 5 Nf3 cxd4 6 exd4 d5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 c5 5 Nge2 cxd4 6 exd4 0-0 7 a3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 c5 5 Nge2 cxd4 6 exd4 0-0 7 c5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 Nc6 5 Nf3 0-0 6 Bd3 d5 7 0-0
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 f3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 g3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 b6 5 Bg5 Bb7 6 Nd2
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 c5 5 d5 b5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 c5 5 d5 Ne4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 c5 5 g3 0-0 6 Bg2 cxd4 7 Nxd4 d5 8 cxd5 Nxd5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 d5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qb3 c5 5 dxc5 Nc6 6 Nf3 Ne4 7 Bd2 Nxc5 8 Qc2 f5 9 g3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qb3 c5 5 dxc5 Nc6 6 Nf3 Ne4 7 Bd2 Nxd2
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 0-0 5 a3 Bxc3+ 6 Qxc3 b5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 0-0 5 a3 Bxc3+ 6 Qxc3 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 c5 5 dxc5 0-0 6 a3 Bxc5 7 Nf3 b6 8 Bf4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 c5 5 dxc5 Bxc3+
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 a3 Bxc3+ 6 Qxc3 Nc6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 a3 Bxc3+ 6 Qxc3 Ne4 7 Qc2 Nc6 8 e3 e5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 cxd5 exd5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 cxd5 Qxd5 6 Nf3 Qf5 7 Qd1 e5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 Nc6 5 Nf3 d6 6 a3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qd3
  1 d4 Nf6 2 c4 e6 3 Nf3 a6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 a3 Ba6 5 Qc2 Bb7
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 a3 Bb7 5 Nc3 d5 6 cxd5 Nxd5 7 Qc2 c5 8 e4 Nxc3 9 bxc3 Nc6 10 Bb2 cxd4 11 cxd4 Rc8 12 Rd1 Bd6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Bf4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 e3 Bb7 5 Bd3 c5 6 0-0 Be7 7 b3 0-0 8 Bb2 cxd4 9 Nxd4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Ba6 5 b3 Bb4+ 6 Bd2 Be7
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Ba6 5 b3 Bb4+ 6 Bd2 Qe7
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Ba6 5 Qa4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Ba6 5 Qb3
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Bb4+ 6 Bd2 a5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Bb4+ 6 Bd2 Be7
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Bb4+ 6 Bd2 Bxd2+ 7 Qxd2 0-0 8 Nc3 Ne4 9 Qc2 Nxc3 10 Ng5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 b3
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 d5 exd5 8 Nd4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 d5 exd5 8 Nh4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 d5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 Na6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 Ne4 8 Qc2 Nxc3 9 Qxc3
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Re1
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 Nc3 Ne4 7 Bd2
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 c5 6 d5 exd5 7 Ng5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 c5 6 d5 exd5 7 Nh4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Qc8 6 0-0 c5 7 d5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 Be7
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 exd5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 Nxd5 7 Bd2
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 Nxd5 7 e3
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 Nxd5 7 e4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 Nxd5 7 Qa4+
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 d5 6 cxd5 Nxd5 7 Qc2
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 g6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 a3 Ne4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 Bg5 h6 6 Bh4 Bb4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 Bg5 h6 6 Bh4 g5 7 Bg3 Nh5
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 a5
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 Be7
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 Bxd2+ 5 Qxd2 b6 6 g3 Bb7 7 Bg2 0-0 8 Nc3 Ne4 9 Qc2 Nxc3 10 Ng5
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 c5
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 Nc6
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 Qe7
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Nbd2
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Nfd2
  1 d4 Nf6 2 c4 e6 3 Nf3 c5 4 d5 b5 5 Bg5 exd5 6 cxd5 h6
  1 d4 Nf6 2 c4 e6 3 Nf3 c5 4 d5 b5 5 dxe6 fxe6 6 cxb5 d5
  1 d4 Nf6 2 c4 e6 3 Nf3 c5 4 d5 b5 5 e4
  1 d4 Nf6 2 c4 e6 3 Nf3 d6
  1 d4 Nf6 2 c4 e6 3 Nf3 Ne4
  1 d4 Nf6 2 c4 g5
  1 d4 Nf6 2 c4 g6 3 d5 b5
  1 d4 Nf6 2 c4 g6 3 f3 d5
  1 d4 Nf6 2 c4 g6 3 f3 e5
  1 d4 Nf6 2 c4 g6 3 g3 Bg7 4 Bg2 d5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 cxd5 Nxd5 6 e4 Nb6 7 Ne2
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 c6 7 cxd5 cxd5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 dxc4
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 Nc6
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 c5 8 dxc5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 c5 8 Nc3
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 Nb6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Be3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 a6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 c5 7 d5 e6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 c5 7 dxc5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 h6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 Na6 7 Qd2 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 Nbd7
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 b6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 c6 7 Bd3 a6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 d5 c6 8 Nge2 (cxd5)
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 d5 Nh5 8 Qd2 Qh4+ 9 g3 Nxg3 10 Qf2 Nxf1 11 Qxh4 Nxe3 12 Ke2 (Nxc4)
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 Nge2 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 Nc6 7 Nge2 a6 8 Qd2 Rb8
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 Nc6 7 Nge2 Rb8
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Nge2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 e5 6 d5 Nh5 7 Be3 Na6 8 Qd2 Qh4+ 9 g3 Nxg3 10 Qf2 Nxf1 11 Qxh4 Nxe3 12 Kf2 Nxc4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 d5 e6 8 dxe6 fxe6 9 g4 Nc6 10 h4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 d5 e6 8 Nf3 exd5 9 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 Nf3 cxd4 8 Nxd4 Nc6 9 Be3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Nf3 c5 7 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 g3 0-0 6 Bg2 e5 7 Nge2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 g3 0-0 6 Bg2 Nc6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 h3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nbd7 8 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nbd7 8 Re1 c6 9 Bf1 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 b4 Nh5 10 Qc2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 b4 Nh5 10 Re1
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Bd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Nd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Ne1 Nd7 10 Be3 f5 11 f3 f4 12 Bf2 g5 13 Rc1 Ng6 14 c5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Ne1 Nd7 10 f3 f5 11 g4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 Be3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 d5 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 d5 Nbd7 8 Bg5 h6 9 Bh4 g5 10 Bg3 Nh5 11 h4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 dxe5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nge2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c5 7 0-0 Nc6 8 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c5 7 0-0 Nc6 8 dxc5 dxc5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Qa5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Qb6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nbd7 7 0-0 e5 8 e4 c6 9 h3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nbd7 7 0-0 e5 8 e4 Re8 9 h3 exd4 10 Nxd4 Nc5 11 Re1 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 d5 Na5 9 Nd2 c5 10 Qc2 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 d5 Na5 9 Nd2 c5 10 Qc2 Rb8 11 b3 b5 12 Bb2 bxc4 13 bxc4 Bh6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 h3 Rb8 9 Be3 b5 10 Nd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 Bg4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 cxd5 Nxd5 (7 Nxd5 Qxd5 8 Bxc7)
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 Rc1 c5 7 dxc5 Be6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 Rc1 c5 7 dxc5 Qa5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bg5 Ne4 5 Nxe4 dxe4 6 Qd2 c5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 b6 9 h4 Ba6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 Bg4 11 f3 Na5 12 Bxf7+
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 cxd4 11 cxd4 Bg4 12 f3 Na5 13 Bd3 Be6 14 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 cxd4 11 cxd4 Bg4 12 f3 Na5 13 Rc1
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 Nc6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 Qd7 (9 0-0 b6)
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Nf3 c5 8 h3
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Nf3 c5 8 Rb1 0-0 9 Be2 Nc6 10 d5 Bxc3+
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 g3
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 Na4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 e3 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 f3 c5 5 cxd5 Nxd5 6 Na4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 g3
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 g4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 h4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Bf4 0-0 6 e3
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Bg5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 cxd5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 b4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Bd2
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Bd3 c6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Bd3 c6 7 0-0 Bg4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 cxd5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Qb3 dxc4 7 Bxc4 Nbd7 8 Ng5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Qb3 e6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 c5 6 Be2 cxd4 7 exd4 Nc6 8 0-0 0-0
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qa4+
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 a6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 b6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Bg4 8 Be3 Nfd7 9 Be2 Nb6 10 Qd3 Nc6 11 0-0-0
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Bg4 8 Be3 Nfd7 9 Qb3 c5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Nc6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Qb3
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 b4
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 0-0 5 Bg2 d6 6 0-0 Nc6 7 d5 Na5
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 c5 5 Bg2 Qa5+
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 d6 5 Bg2 0-0 6 0-0 c5
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 d6 5 Bg2 0-0 6 0-0 Nbd7 7 Nc3 a6
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 d6 5 Bg2 0-0 6 0-0 Nbd7 7 Nc3 e5 8 b3
  1 d4 Nf6 2 c4 g6 3 Nf3 d5 4 e3 c6
  1 d4 Nf6 2 c4 g6 3 Qc2
  1 d4 Nf6 2 c4 Nc6 3 d5 Ne5 4 f4
  1 d4 Nf6 2 d5
  1 d4 Nf6 2 e4 Nxe4 3 Bd3 Nf6 4 Bg5
  1 d4 Nf6 2 f3 d5 3 e4
  1 d4 Nf6 2 f3 d5 3 g4
  1 d4 Nf6 2 f4
  1 d4 Nf6 2 g3 e5
  1 d4 Nf6 2 g4 e5
  1 d4 Nf6 2 g4 Nxg4 3 e4 d6 4 Be2 Nf6 5 Nc3
  1 d4 Nf6 2 g4 Nxg4 3 f3 Nf6 4 e4
  1 d4 Nf6 2 Nc3 c5 3 dxc5
  1 d4 Nf6 2 Nc3 d5 3 Bg5 c5 4 Bxf6 gxf6 5 e4 dxe4 6 d5
  1 d4 Nf6 2 Nc3 d5 3 e4 Nxe4
  1 d4 Nf6 2 Nc3 e5
  1 d4 Nf6 2 Nd2 e5
  1 d4 Nf6 2 Nf3 a6
  1 d4 Nf6 2 Nf3 b5
  1 d4 Nf6 2 Nf3 b6 3 Bg5
  1 d4 Nf6 2 Nf3 b6 3 c3 e5
  1 d4 Nf6 2 Nf3 b6 3 g3 Bb7 4 Bg2 c5 5 c4 cxd4 6 Qxd4
  1 d4 Nf6 2 Nf3 c5 3 d5 b5
  1 d4 Nf6 2 Nf3 c6
  1 d4 Nf6 2 Nf3 d6
  1 d4 Nf6 2 Nf3 e6 3 Bf4
  1 d4 Nf6 2 Nf3 e6 3 Bg5 c5 4 e3 b6 5 d5
  1 d4 Nf6 2 Nf3 e6 3 Bg5 c5 4 e4
  1 d4 Nf6 2 Nf3 e6 3 Bg5 h6
  1 d4 Nf6 2 Nf3 e6 3 e3
  1 d4 Nf6 2 Nf3 g6 3 Bf4
  1 d4 Nf6 2 Nf3 g6 3 Bg5 Bg7 4 Nbd2 c5
  1 d4 Nf6 2 Nf3 g6 3 e3 Bg7 4 Bd3 d5
  1 d4 Nf6 2 Nf3 g6 3 e3 Bg7 4 Bd3 d6
  1 d4 Nf6 2 Nf3 g6 3 g3
  1 d4 Nf6 2 Nf3 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 Be2
  1 d4 Nf6 2 Nf3 Ne4
  1 e3 d5 2 Nc3 Nf6 3 a3 e5 4 f4 exf4 5 Nf3
  1 e3 e5 2 Bc4 b5 3 Bb3
  1 e3 e5 2 c4 d6 3 Nc3 Nc6 4 b3 Nf6
  1 e3 e5 2 Nc3 d5 3 f4 exf4 4 Nf3
  1 e3 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3
  1 e3 e5 2 Nc3 Nf6 3 f4 exf4 4 Nf3
  1 e4 a5 2 d4 Nc6
  1 e4 a6 2 d4 b5 3 Nf3 Bb7 4 Bd3 d6 5 0-0 g6 6 c3 Bg7
  1 e4 a6 2 d4 b5 3 Nf3 Bb7 4 Bd3 e6
  1 e4 a6 2 d4 b5 3 Nf3 Bb7 4 Bd3 d6 5 0-0 Nd7
  1 e4 a6 2 d4 e5
  1 e4 a6 2 d4 e6 3 c4 b5
  1 e4 a6 2 d4 e6 3 Nf3 b5 4 Bd3 c5 5 c3 Bb7 6 0-0 Nf6
  1 e4 a6 2 d4 Nc6
  1 e4 b5
  1 e4 b6 2 d4 Ba6
  1 e4 b6 2 d4 Bb7 3 Bd3 f5 4 exf5 Bxg2 5 Qh5+ g6
  1 e4 b6 2 d4 Bb7 3 Bg5
  1 e4 b6 2 d4 Bb7 3 f3 e5
  1 e4 b6 2 d4 Bb7 3 Nf3 (Bxe4)
  1 e4 b6 2 d4 c5 3 dxc5 Nc6
  1 e4 b6 2 d4 Na6
  1 e4 c5 2 a3
  1 e4 c5 2 a4
  1 e4 c5 2 b3 b6
  1 e4 c5 2 b4 cxb4 3 a3 bxa3
  1 e4 c5 2 b4 cxb4 3 a3 d5 4 exd5 Qxd5 5 Bb2
  1 e4 c5 2 b4 cxb4 3 a3 d5 4 exd5 Qxd5 5 Nf3 e5 6 Bb2 Nc6 7 c4 Qe6
  1 e4 c5 2 b4 cxb4 3 a3 d5 4 exd5 Qxd5 5 Nf3 e5 6 c4 Qe6 7 Bd3
  1 e4 c5 2 b4 cxb4 3 Bb2
  1 e4 c5 2 b4 cxb4 3 c4
  1 e4 c5 2 Bc4
  1 e4 c5 2 Be2
  1 e4 c5 2 c3 d5 3 exd5 Qxd5 4 d4 cxd4 5 cxd4 Nc6 6 Nf3 Bg4 7 Nc3 Bxf3 8 gxf3 Qxd4 9 Qxd4 Nxd4
  1 e4 c5 2 c3 d5 3 exd5 Qxd5 4 d4 cxd4 5 cxd4 Nc6 6 Nf3 e5 7 Nc3 Bb4 8 Be2
  1 e4 c5 2 c3 d5 3 exd5 Qxd5 4 d4 Nf6 5 Nf3 Bg4
  1 e4 c5 2 c3 Nf6 3 e5 Nd5 4 d4 cxd4
  1 e4 c5 2 c3 Nf6 3 e5 Nd5 4 d4 e6 5 Nf3 Nc6
  1 e4 c5 2 c3 Nf6 3 e5 Nd5 4 Nf3 Nc6 5 Bc4 Nb6 6 Bb3 c4 7 Bc2 Qc7 8 Qe2 g5
  1 e4 c5 2 c3 Nf6 3 e5 Nd5 4 Nf3 Nc6 5 Na3
  1 e4 c5 2 c4 d6 3 Nc3 Nc6 4 g3 h5
  1 e4 c5 2 d3 Nc6 3 c3 d6 4 f4
  1 e4 c5 2 d4 cxd4 3 c3 d3 4 c4
  1 e4 c5 2 d4 cxd4 3 c3 d5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nf3
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 d6 5 Bc4 e6 6 Nf3 Nf6 7 0-0 a6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 e6 5 Bc4 a6 6 Nf3 Ne7
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 e6 5 Nf3 a6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 d6 6 Bc4 a6 7 0-0 Nf6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 a6 7 0-0 b5 8 Bb3 Bc5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 a6 7 0-0 Qc7 8 Qe2 Bd6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 Bb4
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 Bc5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 d6 7 0-0 a6 8 Qe2 b5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 g6
  1 e4 c5 2 d4 cxd4 3 c3 e5
  1 e4 c5 2 d4 cxd4 3 c3 Nf6
  1 e4 c5 2 d4 cxd4 3 c3 Qa5
  1 e4 c5 2 d4 cxd4 3 f4
  1 e4 c5 2 d4 cxd4 3 Nf3 e5 4 c3
  1 e4 c5 2 d4 cxd4 3 Qxd4 Nc6 4 Qd1 Nf6 5 Bc4
  1 e4 c5 2 f4 d5 3 exd5 Nf6
  1 e4 c5 2 f4 d5 3 Nc3
  1 e4 c5 2 g3
  1 e4 c5 2 g4
  1 e4 c5 2 h4
  1 e4 c5 2 Ke2
  1 e4 c5 2 Na3 Nc6 3 d4 cxd4 4 Bc4
  1 e4 c5 2 Nc3 e6 3 d4 d5
  1 e4 c5 2 Nc3 e6 3 g3 d5
  1 e4 c5 2 Nc3 Nc6 3 f4 g6 4 Nf3 Bg7 5 Bc4 e6 6 f5
  1 e4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 Be3
  1 e4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 f4 e5 7 Nh3 Nge7
  1 e4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 Nge2 e5
  1 e4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 e6 6 Be3 Nd4 7 Nce2
  1 e4 c5 2 Nc3 Nc6 3 g4
  1 e4 c5 2 Nc3 Nc6 3 Nf3 e5 4 Bc4 d6 5 d3 Be7 6 Nd2 Bg5
  1 e4 c5 2 Nc3 Nc6 3 Nge2
  1 e4 c5 2 Ne2
  1 e4 c5 2 Nf3 a6 3 b3
  1 e4 c5 2 Nf3 a6 3 b4
  1 e4 c5 2 Nf3 a6 3 Be2
  1 e4 c5 2 Nf3 a6 3 c3 b5
  1 e4 c5 2 Nf3 a6 3 c3 d5 4 exd5 Nf6
  1 e4 c5 2 Nf3 a6 3 c3 d6
  1 e4 c5 2 Nf3 a6 3 c3 Nf6
  1 e4 c5 2 Nf3 a6 3 c4 d6
  1 e4 c5 2 Nf3 a6 3 c4 e6
  1 e4 c5 2 Nf3 a6 3 c4 Nc6 4 d4 cxd4 5 Nxd4 e5
  1 e4 c5 2 Nf3 a6 3 d3
  1 e4 c5 2 Nf3 a6 3 d4 cxd4 4 Bc4
  1 e4 c5 2 Nf3 a6 3 d4 cxd4 4 c3
  1 e4 c5 2 Nf3 a6 3 d4 cxd4 4 Nxd4 e5
  1 e4 c5 2 Nf3 a6 3 d4 cxd4 4 Nxd4 e6
  1 e4 c5 2 Nf3 a6 3 d4 cxd4 4 Qxd4
  1 e4 c5 2 Nf3 a6 3 g3
  1 e4 c5 2 Nf3 a6 3 Nc3
  1 e4 c5 2 Nf3 b5
  1 e4 c5 2 Nf3 b6
  1 e4 c5 2 Nf3 d6 3 b4
  1 e4 c5 2 Nf3 d6 3 Bb5+ Bd7 4 Bxd7+ Qxd7 5 0-0 Nc6 6 c3 Nf6 7 d4
  1 e4 c5 2 Nf3 d6 3 Bb5+ Bd7 4 Bxd7+ Qxd7 5 c4
  1 e4 c5 2 Nf3 d6 3 Bb5+ Nc6 4 0-0 Bd7 5 c3 a6 6 Bxc6 Bxc6 7 Re1 Nf6 8 d4 Bxe4 9 Bg5
  1 e4 c5 2 Nf3 d6 3 Bb5+ Nc6 4 0-0 Bd7 5 Qe2 g6 6 e5
  1 e4 c5 2 Nf3 d6 3 c3 Nf6 4 Be2 Nc6 5 d4 cxd4 6 cxd4 Nxe4 7 d5 Qa5+ 8 Nc3 Nxc3 9 bxc3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 (4 c3)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 g6 5 c4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 g6 5 Nc3 Bg7 6 Be3 Nf6 7 Bc4 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 f3 e5 6 Bb5+
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 f3 e5 6 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 f3 e5 6 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bc4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Be2 e5 7 Nb3 Be7 8 0-0 0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Be2 e5 7 Nb3 Be7 8 0-0 Be6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Be3 Ng4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 b5 8 e5 dxe5 9 fxe5 Qc7 10 Qe2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 h6 9 Bh4 g5 10 fxg5 Nfd7 11 Nxe6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 h6 9 Bh4 Qc7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 Qc7 9 0-0-0 Nbd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Qb6 8 Qd2 Qxb2 9 Rb1 Qa3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 Nbd7 7 Bc4 Qa5 8 Qd2 e6 9 0-0-0 b5 10 Bb3 Bb7 11 Rhe1 Nc5 12 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 f4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 g3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 h3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Rg1
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Bd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Bb5+
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bb5+
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 a6 7 Bb3 b5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 a3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Bb3 (Be7 8 Be3 0-0 9 f4)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Be3 Be7 8 Bb3 0-0 9 0-0 Na5 10 f4 b6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Be3 Be7 8 Qe2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Be7 8 f4 0-0 9 Be3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Nbd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Qc7 8 f4 Nc6 9 Be3 Be7 10 Qe1 0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Qc7 8 f4 Nc6 9 Kh1 Be7 10 a4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 Nc6 7 0-0 Be7 8 Be3 0-0 9 f4 Bd7 10 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 Nc6 7 0-0 Be7 8 Kh1
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be3 a6 7 f3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be3 a6 7 g4 e5 8 Nf5 g6 9 g5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be3 a6 7 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 f4 Nc6 7 Be3 Be7 8 Qf3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 g3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 0-0 0-0 8 Nb3 Nc6 9 Kh1
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 Bc4 12 Nxa5 Bxe2 13 Qxe2 Qxa5 14 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Qc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 f4 Qb6 10 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 (Bc4 12 Bd3 Bxd3 13 cxd3 d5)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 Bc4 12 Nxa5 Bxe2 13 Qxe2 Qxa5 14 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Qc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 Qd2 0-0 9 0-0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 0-0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Bd7 10 0-0-0 Qa5 11 Bb3 Rfc8 12 h4 h5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Bd7 10 0-0-0 Rc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Nd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Nxd4 10 Bxd4 Be6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 g4 d5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 f4 Nbd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 g3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bc4 g6 7 Nxc6 bxc6 8 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bc4 Qb6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bd3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Be2 e5 7 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Be2 e5 7 Nxc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Be2 Nxd4 7 Qxd4 g6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 Bd7 7 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Bb5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Nxc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 a6 8 0-0-0 Bd7 9 f4 Be7 10 Nf3 b5 11 Bxf6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 Nxd4 (10 Qxd4)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 Nxd4 9 Qxd4 a6 10 f4 b5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Qb6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 g6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 g3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Nde2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Qxd4 Nc6 5 Bb5 Qd7
  1 e4 c5 2 Nf3 d6 3 d4 Nf6 4 dxc5 Nxe4
  1 e4 c5 2 Nf3 d6 3 g3 b5
  1 e4 c5 2 Nf3 e5
  1 e4 c5 2 Nf3 e6 3 b3
  1 e4 c5 2 Nf3 e6 3 b4
  1 e4 c5 2 Nf3 e6 3 c3
  1 e4 c5 2 Nf3 e6 4 c4
  1 e4 c5 2 Nf3 e6 3 d4 a6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 c3
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Bd3 Bc5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Bd3 g6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Bd3 Nf6 6 0-0 d6 7 c4 g6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 c4 g6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 c4 Nf6 6 Nc3 Bb4 7 Bd3 Nc6 (8 Bc2)
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Nc3 b5 6 Bd3 Qb6 7 Be3 Bc5 8 Qg4
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Nc3 b5 6 Bd3 Qb6 7 Nf3
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Nc3 b5 6 g3
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Bc5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nb5 d6 6 c4 Nf6 7 N1c3 a6 8 Na3 Be7 9 Be2 0-0 10 0-0 b6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nb5 d6 6 c4 Nf6 7 N1c3 a6 8 Na3 d5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 a6 6 Be2 Nge7
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Nf6 6 Be2 Bb4 7 0-0
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Nf6 6 Ndb5 Bb4 7 Nd6+
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Be3 a6 7 Be2 b5 8 Nxc6 Qxc6 9 a3 Bb7 10 Qd4
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Be3 a6 7 f3
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Be3 a6 7 Qd2
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Ndb5 Qb8 7 Be3 a6 8 Bb6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Bb4 6 Bd3 e5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Bb4 6 e5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Ndb5 Bc5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Nxc6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Qb6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Qb6
  1 e4 c5 2 Nf3 e6 3 d4 d5
  1 e4 c5 2 Nf3 f5 3 exf5 Nh6
  1 e4 c5 2 Nf3 g6 3 c4 Bh6
  1 e4 c5 2 Nf3 g6 3 d4 Bg7 4 dxc5 Qa5+ 5 Nc3 Bxc3+ 6 bxc3 Qxc3+
  1 e4 c5 2 Nf3 g6 3 d4 f5
  1 e4 c5 2 Nf3 h6
  1 e4 c5 2 Nf3 Nc6 3 Bb5 e6 4 Nc3 Nd4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 g6 4 0-0 Bg7 5 c3 e5 6 d4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 g6 4 0-0 Bg7 5 c3 Nf6 6 d4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 g6 4 0-0 Bg7 5 c3 Nf6 6 Qa4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 g6 4 0-0 Bg7 5 Re1 e5 6 b4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 Na5 4 b4
  1 e4 c5 2 Nf3 Nc6 3 Bb5 Nb8
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 c3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 d5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 e5 5 Nb5 d6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Bg7 6 Be3 Nf6 7 Nc3 Ng4
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Bg7 6 Nc2 d6 7 Be2 Nh6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Nf6 6 Nc3 Nxd4 7 Qxd4 d6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 Nc3 Bg7 6 Be3 Nf6 7 Bc4
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 Nxc6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Be2 e5 7 Nb3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Be2 e5 7 Nxc6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 Bd7 7 Qd2
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Bb5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Nb3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Nxc6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 a6 8 0-0-0 Bd7 9 f4 Be7 10 Nf3 b5 11 Bxf6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 e5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 Nxd4 10 Qxd4
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 h6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 g6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Nb3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 (9 Nd5)
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 9 Bxf6 gxf6 10 Nd5 Bg7
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 9 Bxf6 gxf6 10 Nd5 f5 11 Bxb5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 Be6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Nf3
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Nxc6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Qb6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Qc7
  1 e4 c5 2 Nf3 Nc6 3 d4 e6
  1 e4 c5 2 Nf3 Nf6 3 e5 Nd5 4 Nc3 e6 5 Nxd5 exd5 6 d4 Nc6
  1 e4 c5 2 Nf3 Nf6 3 e5 Nd5 4 Nc3 Nxc3
  1 e4 c5 2 Nf3 Nf6 3 Nc3
  1 e4 c5 2 Nf3 Qa5
  1 e4 c5 2 Nf3 Qc7
  1 e4 c5 2 Nh3
  1 e4 c5 2 Qg4
  1 e4 c6 2 b3
  1 e4 c6 2 b4 d5 3 b5
  1 e4 c6 2 b4 e5 3 Bb2
  1 e4 c6 2 Bc4 d5 3 Bb3 dxe4 4 Qh5
  1 e4 c6 2 c4 d5 3 cxd5 cxd5 4 Qb3
  1 e4 c6 2 c4 d5 3 e5
  1 e4 c6 2 c4 d5 3 exd5 cxd5 4 cxd5 Nf6
  1 e4 c6 2 c4 d5 3 exd5 Qxd5
  1 e4 c6 2 c4 e5
  1 e4 c6 2 d3 d5 3 Nd2 g6 4 g3 Bg7 5 Bg2 e5 6 Ngf3 Ne7 7 0-0 0-0 8 b4
  1 e4 c6 2 d4 d5 3 Bd3 Nf6 4 e5 Nfd7 5 e6
  1 e4 c6 2 d4 d5 3 Be3 (dxe4)
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 b4
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 c3 e6 5 Be2
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 g4
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 h4
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 Nc3 e6 5 g4 Bg6 6 Nge2 c5 7 h4
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 Nc3 Qb6
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 Ne2
  1 e4 c6 2 d4 d5 3 e5 Bf5 4 Nf3
  1 e4 c6 2 d4 d5 3 e5 c5
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 Bd3 Nc6 5 c3 Nf6 6 Bf4
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 c5
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 e6 6 Bg5 Nc6
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 g6 6 cxd5 Bg7
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Bg5 dxc4 7 d5 Na5
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Bg5 e6
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Bg5 Qa5
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Bg5 Qb6
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Nf3 Bg4
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 g4
  1 e4 c6 2 d4 d5 3 f3 dxe4 4 fxe4 e5 5 Nf3 exd4 6 Bc4
  1 e4 c6 2 d4 d5 3 Nc3 b5
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Bc4
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 f3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 f4
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 h4 h6 7 Nf3 Nd7 8 h5 Bh7 9 Bd3 Bxd3 10 Qxd3 (e6 11 Bd2 Ngf6 12 0-0-0 Bd6)
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 h4 h6 7 Nf3 Nd7 8 h5 Bh7 9 Bd3 Bxd3 10 Qxd3 e6 11 Bd2 Ngf6 12 0-0-0 Be7
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 Nh3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 h6
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Bc4 Ngf6 6 Ng5 e6 7 Qe2 Nb6 8 Bb3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Bc4 Ngf6 6 Nxf6+ Nxf6
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Nf3 Ngf6 6 Ng3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Ng5 Ndf6
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Ng5 Ngf6 6 Bd3 e6 7 N1f3 Bd6 8 Qe2 h6 9 Ne4 Nxe4 10 Qxe4
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nf6 5 Bd3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nf6 5 Nxf6+ exf6 6 Bc4
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nf6 5 Nxf6+ gxf6
  1 e4 c6 2 d4 d5 3 Nc3 g6 4 e5 Bg7 5 f4 h5
  1 e4 c6 2 d4 d5 3 Nc3 Nf6
  1 e4 c6 2 d4 d5 3 Nd2 Qb6
  1 e4 c6 2 d4 d5 3 Nf3 dxe4 4 Ng5
  1 e4 c6 2 d4 f5
  1 e4 c6 2 d4 Na6 (3 Nc3 Nc7)
  1 e4 c6 2 d4 Nf6
  1 e4 c6 2 g4
  1 e4 c6 2 Nc3 d5 3 d3 dxe4 4 Bg5
  1 e4 c6 2 Nc3 d5 3 Nf3 Bg4 4 h3 Bh5
  1 e4 c6 2 Nc3 d5 3 Nf3 Bg4 4 h3 Bxf3 5 Qxf3 e6 6 d4 dxe4 7 Nxe4 Qxd4 8 Bd3
  1 e4 c6 2 Nc3 d5 3 Nf3 dxe4 4 Ng5
  1 e4 c6 2 Nc3 d5 3 Qf3
  1 e4 d5 2 b3
  1 e4 d5 2 b4
  1 e4 d5 2 exd5 c6 3 dxc6 e5
  1 e4 d5 2 exd5 c6 3 dxc6 Nxc6
  1 e4 d5 2 exd5 e5 3 dxe6ep Bxe6
  1 e4 d5 2 exd5 Nf6 3 Bb5+ Bd7 4 Be2
  1 e4 d5 2 exd5 Nf6 3 c4 c6
  1 e4 d5 2 exd5 Nf6 3 c4 e6
  1 e4 d5 2 exd5 Nf6 3 d4 Bg4 4 f3 Bf5 5 Bb5+ Nbd7
  1 e4 d5 2 exd5 Nf6 3 d4 c6 4 dxc6 e5
  1 e4 d5 2 exd5 Nf6 3 d4 g6 4 c4 b5
  1 e4 d5 2 exd5 Nf6 3 d4 Nc6 4 Nc3
  1 e4 d5 2 exd5 Nf6 3 d4 Nxd5 4 c4 Nb4
  1 e4 d5 2 exd5 Nf6 3 d4 Nxd5 4 Nf3 Bg4
  1 e4 d5 2 exd5 Nf6 3 d4 Nxd5 4 Nf3 g6
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qa5 4 b4
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qa5 4 d4 e5 5 dxe5 Bb4 6 Bd2 Nc6 7 Nf3
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qa5 4 d4 e5 5 Nf3 Bg4
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qa5 4 d4 Nf6 5 Nf3 Bf5 6 Ne5 c6 7 g4
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qa5 4 d4 Nf6 5 Nf3 Bg4 (6 h3)
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qd6 4 d4 c6 5 Bc4 Nf6 6 Nge2 Bf5 7 Bf4 Qb4
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qd6 4 d4 Nf6 5 Nf3 a6
  1 e4 d5 2 exd5 Qxd5 3 Nc3 Qd8
  1 e4 d6 2 d4 e5
  1 e4 d6 2 d4 e6
  1 e4 d6 2 d4 f5
  1 e4 d6 2 d4 Nd7
  1 e4 d6 2 d4 Nf6 3 f3
  1 e4 d6 2 d4 Nf6 3 Nc3 c6
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Bc4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Be2 Bg7 5 g4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Be2 Bg7 5 h4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Be3 c6 5 h3
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Be3 c6 5 Qd2 Bg4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Bg5
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 f4 Bg7 5 Bc4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 f4 Bg7 5 Nf3 0-0 6 Bd3
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 f4 Bg7 5 Nf3 0-0 6 Be3
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 f4 Bg7 5 Nf3 0-0 6 e5 Nfd7 7 h4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 f4 Bg7 5 Nf3 c5
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 g3
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Nf3 Bg7 5 Be2 0-0 6 0-0 Bg4
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Nf3 Bg7 5 Be2 0-0 6 0-0 c6
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Nf3 Bg7 5 Be2 0-0 6 0-0 Nc6
  1 e4 d6 2 d4 Nf6 3 Nc3 g6 4 Nf3 Bg7 5 h3
  1 e4 d6 2 d4 Nf6 3 Nc3 Nbd7 4 f4 e5 5 Nf3 exd4 6 Qxd4 c6 7 Bc4 d5
  1 e4 d6 2 d4 Nf6 3 Nc3 Nbd7 4 g4
  1 e4 d6 2 d4 Nf6 3 Nf3
  1 e4 d6 2 f4 d5 3 exd5 Nf6
  1 e4 d6 2 g4
  1 e4 d6 2 h4
  1 e4 e5 2 a3
  1 e4 e5 2 b3
  1 e4 e5 2 Bb5 Bc5 3 b4
  1 e4 e5 2 Bb5 Nf6 3 d4
  1 e4 e5 2 Bc4 b5 3 Bxb5 c6
  1 e4 e5 2 Bc4 b5 3 Bxb5 f5
  1 e4 e5 2 Bc4 Bc5 3 b4 Bxb4 4 c3
  1 e4 e5 2 Bc4 Bc5 3 b4 Bxb4 4 f4 exf4 5 Nf3 Be7 6 d4 Bh4+ 7 g3 fxg3 8 0-0 gxh2+ 9 Kh1
  1 e4 e5 2 Bc4 Bc5 3 c3 d5 4 Bxd5 Nf6 5 d4
  1 e4 e5 2 Bc4 Bc5 3 c3 Nf6 4 d4 exd4 5 e5 d5 6 exf6 dxc4 7 Qh5 0-0
  1 e4 e5 2 Bc4 Bc5 3 c3 Qg5
  1 e4 e5 2 Bc4 Bc5 3 d4
  1 e4 e5 2 Bc4 Bc5 3 f4
  1 e4 e5 2 Bc4 Bc5 3 Nf3 d6 4 c3 Qe7 5 d4
  1 e4 e5 2 Bc4 Bc5 3 Qe2 Nc6 4 c3 Nf6 5 f4
  1 e4 e5 2 Bc4 Bc5 3 Qe2 Nf6 4 f4
  1 e4 e5 2 Bc4 c6 3 d4 d5 4 exd5 cxd5 5 Bb5+ Bd7 6 Bxd7+ Nxd7 7 dxe5 Nxe5 8 Ne2
  1 e4 e5 2 Bc4 d5
  1 e4 e5 2 Bc4 f5 3 d3
  1 e4 e5 2 Bc4 Nf6 3 d3 Bc5 4 Nc3
  1 e4 e5 2 Bc4 Nf6 3 d3 Be7 4 Nf3 0-0
  1 e4 e5 2 Bc4 Nf6 3 d3 Nc6 4 Nc3 Bb4 5 Ne2
  1 e4 e5 2 Bc4 Nf6 3 d4 exd4 4 c3
  1 e4 e5 2 Bc4 Nf6 3 d4 exd4 4 Nf3 d5 5 exd5 Bb4+ 6 c3 Qe7+
  1 e4 e5 2 Bc4 Nf6 3 d4 exd4 4 Nf3 Nxe4 5 Qxd4
  1 e4 e5 2 Bc4 Nf6 3 f3 Bc5 4 Ne2 Nc6 5 b4
  1 e4 e5 2 Bc4 Nf6 3 f4
  1 e4 e5 2 Bc4 Nf6 3 Nc3 b5
  1 e4 e5 2 Bc4 Nf6 3 Nc3 Nxe4
  1 e4 e5 2 Bc4 Nf6 3 Ne2 Nxe4 4 Nec3
  1 e4 e5 2 Bc4 Nf6 3 Nf3 Nxe4 4 Nc3
  1 e4 e5 2 Bd3
  1 e4 e5 2 Be2
  1 e4 e5 2 c3 d5 3 Qh5 Bd6
  1 e4 e5 2 c3 f5
  1 e4 e5 2 c4 d5
  1 e4 e5 2 d3 d5 3 exd5 c6 4 dxc6 Nxc6
  1 e4 e5 2 d3 f5
  1 e4 e5 2 d3 Nf6 3 f4 Bc5
  1 e4 e5 2 d4 d5
  1 e4 e5 2 d4 d6 3 dxe5 Bd7
  1 e4 e5 2 d4 exd4 3 Bc4
  1 e4 e5 2 d4 exd4 3 Bd3
  1 e4 e5 2 d4 exd4 3 c3 d5 4 exd5 Nf6 5 cxd4 Bb4+
  1 e4 e5 2 d4 exd4 3 c3 dxc3 4 Bc4 cxb2 5 Bxb2 Bb4+
  1 e4 e5 2 d4 exd4 3 c3 dxc3 4 Bc4 cxb2 5 Bxb2 d5
  1 e4 e5 2 d4 exd4 3 c3 dxc3 4 Bc4 cxb2 5 Bxb2 Nf6
  1 e4 e5 2 d4 exd4 3 c3 dxc3 4 Bc4 cxb2 5 Bxb2 Qe7
  1 e4 e5 2 d4 exd4 3 c3 dxc3 4 Nxc3 Bb4 5 Bc4
  1 e4 e5 2 d4 exd4 3 c3 Ne7
  1 e4 e5 2 d4 exd4 3 c3 Qe7 4 cxd4 Qxe4+ 5 Be2 Qxg2 6 Bf3 Qg6
  1 e4 e5 2 d4 exd4 3 f4 Bc5 4 Nf3 Nc6 5 c3
  1 e4 e5 2 d4 exd4 3 Nf3 Bc5 4 c3 dxc3 5 Bc4
  1 e4 e5 2 d4 exd4 3 Nf3 c5 4 Bc4 b5
  1 e4 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qc4
  1 e4 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qe3 Bb4+ 5 c3 Be7
  1 e4 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qe3 f5
  1 e4 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qe3 Nf6 5 Nc3 Bb4 6 Bd2 0-0 7 0-0-0 Re8 8 Bc4 d6 9 Nh3
  1 e4 e5 2 f3 (Nf6 3 Nc3)
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 b4
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 c3 Bg4 5 fxe5 dxe5 6 Qa4+
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 c3 f5 5 fxe5 dxe5 6 d4 exd4 7 Bc4
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 c3 f5 5 fxe5 dxe5 6 d4 fxe4 7 Bc4
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 fxe5
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 Nc3 Nd7
  1 e4 e5 2 f4 Bc5 3 Nf3 d6 4 Nc3 Nf6 5 Bc4 Nc6 6 d3 Bg4 7 h3 Bxf3 8 Qxf3 exf4
  1 e4 e5 2 f4 Bc5 3 Nf3 g5
  1 e4 e5 2 f4 c5
  1 e4 e5 2 f4 d5 3 d4
  1 e4 e5 2 f4 d5 3 exd5 Bc5
  1 e4 e5 2 f4 d5 3 exd5 c6 4 dxc6 Bc5
  1 e4 e5 2 f4 d5 3 exd5 e4 4 Bb5+
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 dxe4 Nxe4 6 Nf3 Bc5 7 Qe2 Bf2+ 8 Kd1 Qxd5+ 9 Nfd2
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 dxe4 Nxe4 6 Nf3 Bc5 7 Qe2 Bf5 8 g4 0-0
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 dxe4 Nxe4 6 Qe2 Qxd5 7 Nd2 f5 8 g4
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 Nc3 Bb4 6 Bd2 e3
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 Nd2
  1 e4 e5 2 f4 d5 3 exd5 e4 4 d3 Nf6 5 Qe2
  1 e4 e5 2 f4 d5 3 exd5 e4 4 Nc3 Nf6 5 Qe2
  1 e4 e5 2 f4 d5 3 exd5 exf4
  1 e4 e5 2 f4 d5 3 Nc3
  1 e4 e5 2 f4 d5 3 Nf3
  1 e4 e5 2 f4 d6
  1 e4 e5 2 f4 exf4 3 b3
  1 e4 e5 2 f4 exf4 3 Bb5
  1 e4 e5 2 f4 exf4 3 Bc4 b5
  1 e4 e5 2 f4 exf4 3 Bc4 c6
  1 e4 e5 2 f4 exf4 3 Bc4 d5 4 Bxd5 c6
  1 e4 e5 2 f4 exf4 3 Bc4 d5 4 Bxd5 Nf6
  1 e4 e5 2 f4 exf4 3 Bc4 d5 4 Bxd5 Qh4+ 5 Kf1 Bd6
  1 e4 e5 2 f4 exf4 3 Bc4 d5 4 Bxd5 Qh4+ 5 Kf1 g5 6 g3
  1 e4 e5 2 f4 exf4 3 Bc4 f5 4 Qe2 Qh4+ 5 Kd1 fxe4 6 Nc3 Kd8
  1 e4 e5 2 f4 exf4 3 Bc4 g5
  1 e4 e5 2 f4 exf4 3 Bc4 Nc6
  1 e4 e5 2 f4 exf4 3 Bc4 Ne7
  1 e4 e5 2 f4 exf4 3 Bc4 Nf6 4 Nc3 Bb4 5 e5
  1 e4 e5 2 f4 exf4 3 Bc4 Nf6 4 Nc3 c6
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 b5
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 Bc5
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 d5 5 Bxd5 g5 6 g3
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 d6 5 Qf3
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 g5 5 Nc3 Bg7 6 d4 d6 7 e5
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 g5 5 Nc3 Bg7 6 d4 Ne7 7 g3
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 g5 5 Nc3 Bg7 6 g3 fxg3 7 Qf3
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 g5 5 Qf3
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 Nc6
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 Nf6
  1 e4 e5 2 f4 exf4 3 Bc4 Qh4+ 4 Kf1 Qf6
  1 e4 e5 2 f4 exf4 3 Bd3
  1 e4 e5 2 f4 exf4 3 Be2 f5 4 exf5 d6
  1 e4 e5 2 f4 exf4 3 d4
  1 e4 e5 2 f4 exf4 3 g3
  1 e4 e5 2 f4 exf4 3 h4
  1 e4 e5 2 f4 exf4 3 Kf2
  1 e4 e5 2 f4 exf4 3 Nc3
  1 e4 e5 2 f4 exf4 3 Ne2
  1 e4 e5 2 f4 exf4 3 Nf3 Be7 4 Bc4 Bh4+ 5 g3 fxg3 6 0-0 gxh2+ 7 Kh1
  1 e4 e5 2 f4 exf4 3 Nf3 Be7 4 Bc4 Nf6
  1 e4 e5 2 f4 exf4 3 Nf3 d5 4 exd5 Nf6 5 Bb5+ c6 6 dxc6 bxc6 7 Bc4 Nd5
  1 e4 e5 2 f4 exf4 3 Nf3 d6 4 b4
  1 e4 e5 2 f4 exf4 3 Nf3 d6 4 d4 Nf6 5 Bd3
  1 e4 e5 2 f4 exf4 3 Nf3 f5 4 e5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Bg7 5 0-0
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Bg7 5 d4 d6 6 c3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Bg7 5 h4 h6 6 d4 d6 7 hxg5 hxg5 8 Rxh8 Bxh8 9 Nc3 c6 10 Ne5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Bg7 5 h4 h6 6 d4 d6 7 Nc3 c6 8 hxg5 hxg5 9 Rxh8 Bxh8 10 Ne5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Bg7 5 h4 h6 6 d4 d6 7 Qd3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 d6 5 0-0 Bg4 6 h3 h5 7 hxg4 hxg4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 d5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Nc6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qe7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qf6 7 Bxf7+ Kxf7 8 d4 Qxd4+ 9 Be3 Qf6 10 Nc3 fxe3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qf6 7 e5 Qf5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qf6 7 e5 Qxe5 8 Bxf7+
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qf6 7 e5 Qxe5 8 d3 Bh6 9 Nc3 Ne7 10 Bd2 Nbc6 11 Rae1
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 gxf3 6 Qxf3 Qf6 7 Nc3 Qd4+ 8 Kh1 Qxc4 9 Nd5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 0-0 Qe7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Bxf7+ Kxf7 6 0-0 gxf3 7 Qxf3 Qf6 8 d4 Qxd4+ 9 Be3 Qf6 10 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 d4 (gxf3 6 Qxf3)
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 d4 gxf3 6 Bxf4)
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 h4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Ne5 Qh4+ 6 Kf1 f3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Ne5 Qh4+ 6 Kf1 Nc6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Ne5 Qh4+ 6 Kf1 Nf6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Ne5 Qh4+ 6 Kf1 Nh6 7 d4 d6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 g4 5 Ne5 Qh4+ 6 Kf1 Nh6 7 d4 f3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Bc4 Nc6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 d4 g4 5 Bxf4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 d4 g4 5 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 d4 g4 5 Ne5 Qh4+ 6 g3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 d4 h6 5 h4 Bg7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Be7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Bg7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 d5 6 d4 Nf6 7 Bxf4 Nxe4 8 Nd2
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 d5 6 d4 Nf6 7 exd5 Qxd5 8 Nc3 Bb4 9 Kf2
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 d6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 h5 6 Bc4 Rh7 7 d4 Bh6 8 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 h6 6 Nxf7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nc6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nf6 6 Bc4 d5 7 exd5 Bd6 8 0-0 (Bxe5)
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nf6 6 Bc4 d5 7 exd5 Bd6 8 d4 Nh5 9 Bxf4 Nxf4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nf6 6 Bc4 d5 7 exd5 Bg7
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nf6 6 d4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Nf6 6 Nxg4 d5
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ne5 Qe7 6 d4 f5 7 Bc4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ng5 h6 6 Nxf7 Kxf7 7 Bc4+ d5 8 Bxd5+ Kg7 9 d4
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ng5 h6 6 Nxf7 Kxf7 7 d4 d5 8 Bxf4 dxe4 9 Bc4+ Kg7 10 Be5+
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ng5 h6 6 Nxf7 Kxf7 7 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ng5 h6 6 Nxf7 Kxf7 7 Qxg4 Nf6 8 Qxf4 Bd6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 h4 g4 5 Ng5 Nf6
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Nc3
  1 e4 e5 2 f4 exf4 3 Nf3 g5 4 Ne5
  1 e4 e5 2 f4 exf4 3 Nf3 h6
  1 e4 e5 2 f4 exf4 3 Nf3 Nc6
  1 e4 e5 2 f4 exf4 3 Nf3 Ne7
  1 e4 e5 2 f4 exf4 3 Nf3 Nf6 4 e5 Nh5 5 g4
  1 e4 e5 2 f4 exf4 3 Nf3 Nf6 4 e5 Nh5 5 Qe2
  1 e4 e5 2 f4 exf4 3 Nh3
  1 e4 e5 2 f4 exf4 3 Qe2
  1 e4 e5 2 f4 exf4 3 Qf3
  1 e4 e5 2 f4 exf4 3 Qg4
  1 e4 e5 2 f4 exf4 3 Qh5
  1 e4 e5 2 f4 f5 3 exf5 Bc5
  1 e4 e5 2 f4 f5 3 exf5 exf4 4 Nf3 d5 5 d4 Bd6 6 Bd3
  1 e4 e5 2 f4 f5 3 exf5 exf4 4 Qh5+ g6 5 fxg6 Qe7+ 6 Kd1
  1 e4 e5 2 f4 f5 3 exf5 exf4 4 Qh5+ Ke7
  1 e4 e5 2 f4 f5 3 exf5 Qh4+
  1 e4 e5 2 f4 f6 3 fxe5 Nc6
  1 e4 e5 2 f4 g5
  1 e4 e5 2 f4 Nc6 3 Nf3 f5
  1 e4 e5 2 f4 Nc6 3 Nf3 g5 4 fxg5 h6
  1 e4 e5 2 f4 Nf6
  1 e4 e5 2 f4 Qf6 3 Nc3 Qxf4 4 d4
  1 e4 e5 2 f4 Qf6 3 Nf3 Qxf4 4 Nc3 Bb4 5 Bc4
  1 e4 e5 2 f4 Qh4+ 3 g3 Qe7
  1 e4 e5 2 g3
  1 e4 e5 2 h3
  1 e4 e5 2 Ke2
  1 e4 e5 2 Nc3 Bb4 3 Qg4 (Nf6)
  1 e4 e5 2 Nc3 Bc5 3 b4 Bxb4 4 f4 exf4
  1 e4 e5 2 Nc3 Bc5 3 Na4
  1 e4 e5 2 Nc3 Bc5 3 Qg4
  1 e4 e5 2 Nc3 d6 3 f4
  1 e4 e5 2 Nc3 Nc6 3 Bc4 Bc5 4 Qg4 Qf6 5 Nd5
  1 e4 e5 2 Nc3 Nc6 3 Bc4 Nf6 4 f4 Nxe4 5 Nf3
  1 e4 e5 2 Nc3 Nc6 3 d4 f5
  1 e4 e5 2 Nc3 Nc6 3 f4 Bc5 4 fxe5 d6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 b6 6 Nb5 Ba6 7 a4 g5 8 Nf3 Qh5 9 Ke1 Kd8 10 g4
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 d5 6 exd5 Qe7+ 7 Kf2 Qh4+ 8 g3 fxg3+ 9 hxg3
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 d6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 g5
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 Be7
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 Bc4 g4 6 0-0 gxf3 7 Qxf3 Ne5 8 Qxf4 Qf6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 d4 g4 6 Bc4 gxf3 7 0-0 d5 8 exd5 Bg4 9 dxc6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 h4 g4 6 Ng5 d6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 h4 g4 6 Ng5 h6 7 Nxf7 Kxf7 8 d4
  1 e4 e5 2 Nc3 Nc6 3 g3 Bc5 4 Bg2 h5 5 Nf3 h4
  1 e4 e5 2 Nc3 Nc6 3 g3 Nf6 4 Bg2 Bc5 5 Nge2 d5
  1 e4 e5 2 Nc3 Nf6 3 a3
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Bb4
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Bc5 4 Nf3
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Bc5 4 Nge2 b5
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nc6
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nxe4 4 Nf3 d5
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nxe4 4 Qh5 Nd6 5 Bb3 Be7 6 Nf3 Nc6 7 Nxe5
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nxe4 4 Qh5 Nd6 5 Bb3 Nc6 6 d4
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nxe4 4 Qh5 Nd6 5 Bb3 Nc6 6 Nb5 g6 7 Qf3 f5 8 Qd5 Qe7 9 Nxc7+ Kd8 10 Nxa8 b6
  1 e4 e5 2 Nc3 Nf6 3 f3
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 d3 Bb4 5 fxe5 Nxe4
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 d3 Qh4+ 6 g3 Nxg3 7 Nf3 Qh5 8 Nxd5 Bg4 9 Nf4 Bxf3 10 Nxh5 Bxd1 11 hxg3 Bxc2 12 b3
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 Nf3 Bb4 6 Qe2
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 Nf3 Be7
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 Nf3 Bg4 6 Qe2
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 Qf3 f5 6 d4
  1 e4 e5 2 Nc3 Nf6 3 g3 Bc5 4 Bg2 Nc6 5 Nge2 d5 6 exd5
  1 e4 e5 2 Nc3 Nf6 3 g3 d5 4 exd5 c6
  1 e4 e5 2 Ne2
  1 e4 e5 2 Nf3 a6
  1 e4 e5 2 Nf3 b6
  1 e4 e5 2 Nf3 Bc5 3 Nxe5 Nc6
  1 e4 e5 2 Nf3 Bd6
  1 e4 e5 2 Nf3 c5
  1 e4 e5 2 Nf3 c6
  1 e4 e5 2 Nf3 d5 3 exd5 Bd6
  1 e4 e5 2 Nf3 d5 3 exd5 e4
  1 e4 e5 2 Nf3 d5 3 Nxe5 dxe4 4 Bc4 Qg5
  1 e4 e5 2 Nf3 d6 3 Bc4 Be6 4 c3
  1 e4 e5 2 Nf3 d6 3 Bc4 Be7 4 c3
  1 e4 e5 2 Nf3 d6 3 Bc4 f5 4 d4 exd4 5 Ng5 Nh6 6 Nxh7
  1 e4 e5 2 Nf3 d6 3 d4 Bd7
  1 e4 e5 2 Nf3 d6 3 d4 Bg4 4 dxe5 Bxf3
  1 e4 e5 2 Nf3 d6 3 d4 Bg4 4 dxe5 Nd7
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Bc4
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 c3
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 c5 5 Bb5+
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 c5 5 Nf5 Nc6 6 Ne3
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 c5 5 Nf5 Nf6
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 d5 5 exd5
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 g6
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 Nf6 5 Nc3 Be7 6 Be2 0-0 7 0-0 c5 8 Nf3 Nc6 9 Bg5 Be6 10 Re1
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Qxd4 Bd7
  1 e4 e5 2 Nf3 d6 3 d4 f5 4 dxe5 fxe4 5 Ng5 d5 6 e6 Bc5 7 Nc3
  1 e4 e5 2 Nf3 d6 3 d4 f5 4 Nc3
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 0-0 Be7 6 dxe5
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 a4
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 c3
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 Nc3
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 Ng5 Nh6 6 f4 Be7 7 0-0 0-0 8 c3 d5
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 Ng5 Nh6 6 f4 Be7 7 c3 0-0 8 0-0 d5
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 Nb6
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Nc3 Ngf6 5 Bc4 Be7 6 0-0 h6
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Nc3 Ngf6 5 Be2 Be7 6 0-0 c6
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Nc3 Ngf6 5 g4
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Bc4
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 dxe5 Nxe4 5 Nbd2
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 dxe5 Nxe4 5 Qd5
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 exd4 5 Nxd4 Be7 6 Be2 0-0 7 0-0 c5 8 Nf3 Nc6 9 Bg5 Be6 10 Re1
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 0-0 0-0 7 Qe2 c6 8 a4 exd4
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 Bxf7+
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 dxe5 dxe5 7 Bxf7+
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 Ng5 0-0 7 Bxf7+ Rxf7 8 Ne6
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 h6
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Ng5 h6 5 Nxf7
  1 e4 e5 2 Nf3 f5 3 b4
  1 e4 e5 2 Nf3 f5 3 Bc4 b5
  1 e4 e5 2 Nf3 f5 3 Bc4 fxe4 4 Nxe5 d5
  1 e4 e5 2 Nf3 f5 3 Bc4 fxe4 4 Nxe5 Nf6
  1 e4 e5 2 Nf3 f5 3 Bc4 fxe4 4 Nxe5 Qg5 5 d4 Qxg2
  1 e4 e5 2 Nf3 f5 3 Bc4 fxe4 4 Nxe5 Qg5 5 Nf7 Qxg2 6 Rf1 d5 7 Nxh8 Nf6
  1 e4 e5 2 Nf3 f5 3 Bc4 Nf6
  1 e4 e5 2 Nf3 f5 3 c4
  1 e4 e5 2 Nf3 f5 3 d3 Nc6 4 exf5
  1 e4 e5 2 Nf3 f5 3 d4
  1 e4 e5 2 Nf3 f5 3 exf5
  1 e4 e5 2 Nf3 f5 3 g4
  1 e4 e5 2 Nf3 f5 3 Nc3
  1 e4 e5 2 Nf3 f5 3 Nxe5 Nc6
  1 e4 e5 2 Nf3 f5 3 Nxe5 Nf6 4 Bc4 fxe4 5 Nf7 Qe7 6 Nxh8 d5
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qe7
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 d4 d6 5 Nc4 fxe4 6 Be2
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 d4 d6 5 Nc4 fxe4 6 Nc3 Qg6
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 d4 d6 5 Nc4 fxe4 6 Ne3
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 d4 d6 5 Nc4 fxe4 6 Qh5+ g6 7 Qe2
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 Nc4 fxe4 5 d3
  1 e4 e5 2 Nf3 f5 3 Nxe5 Qf6 4 Nc4 fxe4 5 Nc3
  1 e4 e5 2 Nf3 f6 3 Nxe5 (fxe5 4 Qh5+ g6 5 Qxe5+ Qe7 6 Qxh8)
  1 e4 e5 2 Nf3 f6 3 Nxe5 Qe7 4 Nf3 d5
  1 e4 e5 2 Nf3 g6
  1 e4 e5 2 Nf3 Nc6 3 a3
  1 e4 e5 2 Nf3 Nc6 3 b3
  1 e4 e5 2 Nf3 Nc6 3 b4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 b5 5 Bb3 Bc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 b5 5 Bb3 Na5 6 Bxf7+
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Bb4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Bc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 Bxc6+ bxc6 6 d4 f6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 c3 Bd7 6 d4 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 c3 Bd7 6 d4 Nge7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 c3 f5 6 exf5 Bxf5 7 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 d4 b5 6 Bb3 exd4 7 Nxd4 Nxd4 8 Qxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 d4 b5 6 Bb3 Nxd4 7 Nxd4 exd4 8 Qxd4 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 f5 5 exf5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 b5 6 Bb3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 b5 6 Bb3 Be7 7 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 b5 6 Bb3 d6 7 Ng5 d5 8 exd5 Nd4 9 Re1 Bc5 10 Rxe5+ Kf8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Bc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 d4 b5 7 Bb3 d6 8 c3 Bg4 9 h3 Bxf3 10 Qxf3 exd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 d4 exd4 7 e5 Ne4 8 c3 dxc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Qe2 b5 7 Bb3 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Qe2 b5 7 Bb3 d6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 e4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 c6 12 Bxd5 cxd5 13 d4 Bd6 14 Re3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 c6 12 d4 Bd6 13 Re1 Qh4 14 g3 Qh3 15 Be3 Bg4 16 Qd3 Rae8 17 Nd2 Re6 18 a4 Qh5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 Bc2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 d4 Bg4 10 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 a5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Be6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 h6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Bd7 13 Nf1 Rfe8 14 Ne3 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 cxd4 13 cxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Nc6 13 dxc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c6 11 d4 Qc7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d4 Nbd7 11 Nbd2 Bb7 12 Bc2 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d4 Nbd7 11 Nh4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Re8 10 d4 Bb7 11 Nbd2 Bf8 12 a3 h6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 Bc2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 a5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Be6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 h6 10 d4 Re8 11 Nbd2 Bf8 12 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Bb7 12 Nbd2 cxd4 13 cxd4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Bd7 13 Nf1 Rfe8 14 Ne3 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 cxd4 13 cxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c6 11 d4 Qc7 12 Nbd2 Nc4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d4 Nbd7 11 Nbd2 Bb7 12 Bc2 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d4 Nbd7 11 Nh4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Qd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 h3 Nc6 12 d5 Nb8 13 Nbd2 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 h3 Nc6 12 d5 Nd8 13 Nbd2 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 Nbd2 0-0 12 Nf1 Bg4 13 Ne3 Bxf3 14 Qxf3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 d4 Nxd4 9 Nxd4 exd4 10 Qxd4 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 d6 7 c3 0-0 8 d4 Bd7 9 Nbd2 Be8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 d6 7 c3 Bd7 8 d4 0-0 9 Nbd2 Be8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 d6 6 Bxc6+ bxc6 7 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 d6 6 Bxc6+ bxc6 7 d4 Nxe4 8 Re1 f5 9 dxe5 d5 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 a4 Nxd4 9 Nxd4 exd4 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Bc5 10 Nbd2 0-0 11 Bc2 Nxf2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Bc5 10 Qd3 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 a4 b4 11 Nd4 Qd7 12 Nxe6 fxe6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Nbd2 0-0 11 Qe2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Re1 0-0 11 Nd4 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Re1 0-0 11 Nd4 Qd7 12 Nxe6 fxe6 13 Rxe4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Nc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Nbd2 Bc5 10 Qe1
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Nbd2 Nc5 10 c3 d4 11 Ng5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Qe2 Be7 10 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Qe2 Be7 10 Rd1 0-0 11 c4 bxc4 12 Bxc4 Qd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 exd4 7 Re1 d5 8 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 Qe2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 Re1 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 c3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 d3 d6 6 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 d4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Nc3 Bc5 6 Nxe5 Nxe5 7 d4 Bd6 8 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Qe2 b5 6 Bb3 Be7 7 c3 d6 8 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Qe2 b5 6 Bb3 Be7 7 d4 d6 8 c3 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nge7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 bxc6 5 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 0-0 Bd6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 0-0 Bg4 6 h3 h5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 0-0 f6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 0-0 Qd6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 d4 exd4 6 Qxd4 Qxd4 (7 Nxd4 Bd6)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 d4 exd4 6 Qxd4 Qxd4 (7 Nxd4 Bd7)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 Nc3 f6 6 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 b6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bb4 4 c3 Ba5 5 Bxc6 dxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 0-0 Nd4 5 b4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 0-0 Nf6 5 c3 0-0 6 d4 Bb6 7 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 b4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 c3 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 c3 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 c3 f5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 c3 Nf6 5 0-0 0-0 6 d4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Bc5 4 c3 Qe7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Be7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d5 4 Nxe5 Qg5 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d5 4 Nxe5 Qg5 5 Nxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d6 4 Bxc6+ bxc6 5 d4 f6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d6 4 d4 Bd7 5 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d6 4 d4 Bd7 5 Nc3 Nf6 6 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 d6 4 d4 exd4 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 d4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 exf5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 Be7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 d5 6 Nxe5 dxe4 7 Nxc6 Qd5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 d5 6 Nxe5 dxe4 7 Nxc6 Qg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 Nf6 5 exf5 e4 6 Nh4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 g6 4 c3 f5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Na5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nb8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nd4 4 Nxd4 exd4 5 0-0 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nd4 4 Nxd4 exd4 5 d3 c6 6 Bc4 Nf6 7 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Bc5 5 c3 0-0 6 d4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Bd7 6 Nc3 Be7 7 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Bd7 6 Nc3 Be7 7 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Bd7 6 Nc3 Be7 7 Re1 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Bd7 6 Nc3 exd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Ng4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 a6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 dxe5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 b3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 (6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nc3 0-0 10 Re1 Nc5 11 Nd4 Ne6 12 Be3 Nxd4 13 Bxd4 c5)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nc3 0-0 10 Re1 Re8 11 Qc4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nf5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 6 Ba4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 6 Bxc6 dxc6 7 dxe5 Ne4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 6 Bxc6 dxc6 7 dxe5 Nf5 8 Qxd8+ Kxd8 9 Nc3 Bd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 (6 dxe5)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 d3 Bc5 5 Be3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 d3 d6 5 Bxc6+
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 d3 d6 5 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 d3 Ne7 5 Nxe5 c6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 d4 exd4 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 Nc3 Bb4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nge7 4 d4 exd4 5 Nxd4 g6 6 Nc3 Bg7 7 Be3 0-0 8 Qd2 d5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nge7 4 Nc3 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Qe7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Qf6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 0-0 Nf6 5 d4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 b5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 a4 a6 6 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Nh6 7 d4 d6 8 Bxh6 dxe5 9 Bxg7 Rg8 10 Bxf7+ Kxf7 11 Bxe5 Qg5 12 Nd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Qg5 7 Bxf7+ Ke7 8 Qh5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Qg5 7 Qf3 Qxe5 8 Qxf7+ Kd8 9 Bb2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 Bb2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 0-0 d6 6 d4 Bd7 7 Bb2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bd7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 exd4 8 Qb3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 Nf6 7 d4 0-0 8 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 Qf6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 b5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 d6 7 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 d6 7 Qb3 Qd7 8 dxe5 dxe5 9 0-0 Bb6 10 Ba3 Na5 11 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 b5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 d3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 d6 (8 Qb3)
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 dxc3 8 Qb3 Qf6 9 e5 Qg6 10 Nxc3 Nge7 11 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 dxc3 8 Qb3 Qf6 9 e5 Qg6 10 Nxc3 Nge7 11 Rd1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 Nge7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 Nxd4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 0-0 d6 7 d4 exd4 8 cxd4 Bb6 9 Nc3 Bg4 10 Qa4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 d5 Na5 10 Bb2 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 Nc3 Bg4 10 Qa4 Bd7 11 Qb3 Na5 12 Bxf7+ Kf8 13 Qc2 Kxf7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 Nc3 Na5 10 Bg5 f6 11 Be3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 cxd4 Bb4+ 8 Bd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 cxd4 Bb4+ 8 Kf1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bd6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Be7 6 d4 Na5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bf8
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 d5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 Bxf7+
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 d6 5 d4 exd4 6 cxd4 Bb4+ 7 Kf1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 d6 5 d4 exd4 6 cxd4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 f5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 b4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d3 a6 6 Bb3 Ba7 7 h3 0-0 8 0-0 d6 9 Re1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Bd2 Nxe4 8 Bxb4 Nxb4 9 Bxf7+ Kxf7 10 Qb3+ d5 11 Ne5+ Kf6 12 f3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Kf1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 bxc3 d5 10 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 d5 Bf6 10 Re1 Ne7 11 Rxe4 d6 12 Bg5 Bxg5 13 Nxg5 0-0 14 Nxh7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 d5 Bf6 10 Re1 Ne7 11 Rxe4 d6 12 g4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Nxc3 9 bxc3 Bxc3 10 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Nxc3 9 bxc3 Bxc3 10 Qb3 d5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 e5 d5 7 Bb5 Ne4 8 cxd4 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 e5 Ne4 7 Bd5 Nxf2 8 Kxf2 dxc3+ 9 Kg3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 0-0 Nf6 7 a4 a6 8 Re1 d6 9 h3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 d5 Nb8 7 d6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 d5 Nf6 7 a4 a6 8 0-0 d6 9 h3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 d3 f5 5 Ng5 f4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 d3 Nf6 5 Nc3 d6 6 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 d4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 Nc3 Nf6 5 d3 d6 6 Bg5 h6 7 Bxf6 Qxf6 8 Nd5 Qd8 9 c3 Ne7 10 d4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Be7 4 d4 exd4 5 c3 Nf6 6 e5 Ne4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 d6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 f5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 h6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d3 d6 6 Bg5 h6 7 Bh4 g5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d4 Bxd4 6 Nxd4 Nxd4 7 Bg5 d6 8 f4 Qe7 9 fxe5 dxe5 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d4 Bxd4 6 Nxd4 Nxd4 7 Bg5 h6 8 Bh4 g5 9 f4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 c3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 Bd6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 d6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 c3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 fxg7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 g6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 Qd5 10 Nc3 Qf5 11 g4 Qg6 12 Nce4 Bb6 (13 f4 0-0-0)
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 Qd5 10 Nc3 Qf5 11 Nce4 Bf8
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 Ng4 7 c3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Be7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 d6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Re1 d5 7 Bxd5 Qxd5 8 Nc3 Qa5 9 Nxe4 Be6 10 Bd2 Qd5 11 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Re1 d5 7 Bxd5 Qxd5 8 Nc3 Qa5 9 Nxe4 Be6 10 Bg5 h6 11 Bh4 g5 12 Nf6+ Ke7 13 b4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Re1 d5 7 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 e5 d5 6 Bb5 Ne4 7 Nxd4 Bc5 8 Nxc6 Bxf2+ 9 Kf1 Qh4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 Ng5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Nc3 Nxe4 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 Bc5 5 Bxf7+ Ke7 6 d4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 Bc5 5 Nxf7 Bxf2+ 6 Kxf2 Nxe4+ 7 Ke3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 b5 6 Bf1 h6 7 Nxf7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nf3 e4 10 Ne5 Bd6 11 d4 Qc7 12 Bd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nf3 e4 10 Ne5 Qc7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nh3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 cxb5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 Qc7 9 Bd3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 Rb8
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 d3 h6 7 Nf3 e4 8 Qe2 Nxc4 9 dxc4 Bc5 10 Nfd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 d3 h6 7 Nf3 e4 8 Qe2 Nxc4 9 dxc4 Be7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nb4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nd4 6 c3 b5 7 Bf1 Nxd5 8 Ne4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nxd5 6 d4 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nxd5 6 Nxf7 Kxf7 7 Qf3+ Ke6 8 Nc3 Ncb4 9 Qe4 c6 10 a3 Na6 11 d4 Nac7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nxd5 6 Nxf7 Kxf7 7 Qf3+ Ke6 8 Nc3 Nce7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 Nxe4
  1 e4 e5 2 Nf3 Nc6 3 Be2 Nf6 4 d3 d5 5 Nbd2
  1 e4 e5 2 Nf3 Nc6 3 Be2 Nf6 4 d4 exd4 5 e5
  1 e4 e5 2 Nf3 Nc6 3 c3 Bc5 4 b4
  1 e4 e5 2 Nf3 Nc6 3 c3 Be7
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Bb5 dxe4 5 Nxe5 Qd5 6 Qa4
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Bb5 dxe4 5 Nxe5 Qg5
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 Bd7 5 exd5 Nd4 6 Qd1 Nxf3+ 7 Qxf3 f5 8 Bc4 Bd6 9 d3
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 dxe4
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 f6
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 Nf6
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 Qd6
  1 e4 e5 2 Nf3 Nc6 3 c3 f5 4 d4 d6 5 d5 fxe4 6 Ng5 Nb8 7 Nxe4 Nf6 8 Bd3 Be7
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 Bc4
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 d3
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 d4 d6
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 d4 exd4
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 d4 Nxe4 5 d5 Bc5
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 Qa4
  1 e4 e5 2 Nf3 Nc6 3 c3 Nf6 4 Qc2
  1 e4 e5 2 Nf3 Nc6 3 c3 Nge7
  1 e4 e5 2 Nf3 Nc6 3 c4 Nf6 4 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 d3
  1 e4 e5 2 Nf3 Nc6 3 d4 a6
  1 e4 e5 2 Nf3 Nc6 3 d4 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 d4 Bd6
  1 e4 e5 2 Nf3 Nc6 3 d4 d5
  1 e4 e5 2 Nf3 Nc6 3 d4 d6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bb5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bb4+ 5 c3 dxc3 6 0-0 cxb2 7 Bxb2 Nf6 8 Ng5 0-0 9 e5 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bb4+ 5 c3 dxc3 6 bxc3 (Ba5 7 e5)
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 0-0 d6 6 c3 Bg4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 Ng5 Nh6 6 Nxf7 Nxf7 7 Bxf7+ Kxf7 8 Qh5+ g6 9 Qxc5 d5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 Ng5 Nh6 6 Nxf7 Nxf7 7 Bxf7+ Kxf7 8 Qh5+ g6 9 Qxc5 d6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 Ng5 Nh6 6 Qh5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Be7
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Nf6 5 e5 Ng4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 c3 d5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 c3 dxc3 5 Bc4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 c3 dxc3 5 Nxc3 Bb4 6 Bc4 Nf6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Bb6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Nge7 7 Bb5 Nd8
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Nge7 7 Nc2
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Nge7 7 Qd2 d5 8 Nb5 Bxe3 9 Qxe3 0-0 10 Nxc7 Rb8 11 Nxd5 Nxd5 12 exd5 Nb4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Qg6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 Nb5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Nb3 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Nxc6 Qf6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Be7
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Nf6 5 e5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Nf6 5 Nc3 Nxe4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Nf6 5 Nxc6 bxc6 6 e5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Nf6 5 Nxc6 bxc6 6 Nd2
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Nxd4 5 Qxd4 d6 6 Bd3
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Be3
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qf4 10 a4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qf4 10 c4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qg6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Qxd2
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Nd2 Qxe4+ 7 Be2 Qxg2 8 Bf3 Qh3 9 Nxc7+ Kd8 10 Nxa8 Nf6 11 a3
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nc3 Bb4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nf3
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nf5
  1 e4 e5 2 Nf3 Nc6 3 d4 f5
  1 e4 e5 2 Nf3 Nc6 3 d4 f6
  1 e4 e5 2 Nf3 Nc6 3 d4 Nf6
  1 e4 e5 2 Nf3 Nc6 3 d4 Nxd4 4 Nxd4 exd4 5 Bc4
  1 e4 e5 2 Nf3 Nc6 3 d4 Nxd4 4 Nxe5 Ne6 5 Bc4 c6 6 0-0 Nf6 7 Nxf7
  1 e4 e5 2 Nf3 Nc6 3 d4 Nxd4 4 Nxe5 Ne6 5 Bc4 c6 6 Nxf7
  1 e4 e5 2 Nf3 Nc6 3 d4 Qe7
  1 e4 e5 2 Nf3 Nc6 3 d4 Qf6
  1 e4 e5 2 Nf3 Nc6 3 g3
  1 e4 e5 2 Nf3 Nc6 3 h3
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Bb4 4 Nd5 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 f5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 g6 4 d4 exd4 5 Nd5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 a3 d5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 a3 d6 5 h3
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 a6 5 Bxc6 dxc6 6 Nxe5 Nxe4 7 Nxe4 Qd4 8 0-0 Qxe5 9 Re1 Be6 10 d4 Qd5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 Bxc6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Bxc3 7 bxc3 d5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Bxc3 7 bxc3 d6 8 Re1
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Be6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bg4 8 Nd5 Nd4 9 Nxb4 Nxb5 10 Nd5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bxc3 8 bxc3 h6 9 Bh4 g5 10 Nxg5 Nxe4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bxc3 8 bxc3 Qe7 9 Re1 Nd8 10 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Ne7 8 Nh4 c6 9 Bc4 d5 10 Bb3 Qd6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Ne2
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Qe7 7 Ne2 d5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 Nd5 Bc5 7 d4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 Nd5 Nxd5 7 exd5 e4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 Nd5 Bc5 6 c3
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bc5 5 0-0 0-0 6 Nxe5 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bc5 5 0-0 0-0 6 Nxe5 Nxe5 7 d4 Bd6 8 f4 Nc6 9 e5 Bb4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bc5 5 Nxe5 Nd4 6 Ba4 0-0
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 d6 5 d3
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Nd4 5 0-0
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Nd4 5 Be2 Nxf3+ 6 Bxf3 Bc5 7 0-0 0-0 8 d3 d6 9 Na4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Nd4 5 Nxd4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Nd4 5 Nxe5 Qe7 6 f4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bc4 Bc5 5 d3 0-0 6 0-0 d6 7 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bc4 Bc5 5 d3 0-0 6 0-0 h6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bc4 Nxe4 5 Bxf7+
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Be2
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 Bb4 5 d5 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 Bb4 5 Nxe5 Qe7
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nd5 Nxe4 6 Qe2 f5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Bd3 d5 8 exd5 cxd5 9 0-0 0-0 10 Bg5 Be6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Bd3 d5 8 exd5 cxd5 9 0-0 0-0 10 Bg5 c6 11 Qf3 Be7 12 Rae1
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Qd4 Qe7 8 f3 c5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Nxe4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Nxe5 Nxe5 5 d4 Nc6 (6 d5 Ne5 7 f4 Ng6 8 e5 Ng8 9 d6 cxd6 10 exd6 Qf6 11 Nb5 Rb8)
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Nxe5 Nxe5 5 d4 Ng6 6 e5 Ng8 7 Bc4 Bb4 8 Qf3 f5
  1 e4 e5 2 Nf3 Nc6 3 Nxe5 Nxe5 4 d4
  1 e4 e5 2 Nf3 Nf6 3 Bc4 Nxe4 4 Nc3 d5
  1 e4 e5 2 Nf3 Nf6 3 d3
  1 e4 e5 2 Nf3 Nf6 3 d4 d5
  1 e4 e5 2 Nf3 Nf6 3 d4 exd4 4 Bc4
  1 e4 e5 2 Nf3 Nf6 3 d4 exd4 4 e5 Ne4 5 Bb5
  1 e4 e5 2 Nf3 Nf6 3 d4 exd4 4 e5 Ne4 5 Qe2 Nc5 6 Nxd4 Nc6
  1 e4 e5 2 Nf3 Nf6 3 d4 exd4 4 e5 Ne4 5 Qxd4
  1 e4 e5 2 Nf3 Nf6 3 d4 Nxe4 4 Bd3 d5 5 Nxe5 Bd6 6 0-0 0-0 7 c4 Bxe5
  1 e4 e5 2 Nf3 Nf6 3 d4 Nxe4 4 Bd3 Nc6
  1 e4 e5 2 Nf3 Nf6 3 Nc3
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nc4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nd3
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 Bd3
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 c4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d3
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Bd6 7 0-0 0-0 8 c4 Bg4 9 cxd5 f5 10 Re1 Bxh2+
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Bd6 7 0-0 0-0 8 c4 c6 9 Re1 Bg4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 0-0
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 c4 Nb4 9 Be2
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 c4 Nb4 9 cxd5
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c3 f5 10 c4 Bh4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c3 f5 (10 Nbd2)
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Nc6
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 Nf6
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 Nc3
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 Qe2
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nxf7 Kxf7 5 Bc4+
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nxf7 Kxf7 5 d4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 Nc6
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 Nxe4 4 Qe2 Qe7
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 Qe7
  1 e4 e5 2 Nf3 Nf6 3 Qe2 Nc6 4 d4
  1 e4 e5 2 Nf3 Qe7 3 Bc4 f5
  1 e4 e5 2 Nf3 Qf6 3 Bc4 Qg6 4 0-0
  1 e4 e5 2 Qe2 Nc6 3 c3 Nf6 4 Nf3 Bc5 5 d4 exd4 6 cxd4 Nxd4
  1 e4 e5 2 Qf3
  1 e4 e5 2 Qg4 Nf6 3 Qf5
  1 e4 e5 2 Qh5 Nc6 3 Bc4 Nh6 4 d3 g6 5 Qf3 f6 6 Ne2 d5
  1 e4 e5 2 Qh5 Nf6
  1 e4 e6 2 b3 d5 3 Bb2
  1 e4 e6 2 b4 Bxb4 3 e5
  1 e4 e6 2 Bb5
  1 e4 e6 2 c4 d5 3 cxd5 exd5 4 Qb3
  1 e4 e6 2 d3 d5 3 Nd2 Nf6 4 Ngf3 Nc6 5 Be2
  1 e4 e6 2 d3 f5
  1 e4 e6 2 d4 a6 3 c4 b5 4 cxb5 axb5 5 Bxb5
  1 e4 e6 2 d4 b5
  1 e4 e6 2 d4 c5
  1 e4 e6 2 d4 d5 3 Bd3
  1 e4 e6 2 d4 d5 3 Be3
  1 e4 e6 2 d4 d5 3 c4 dxe4
  1 e4 e6 2 d4 d5 3 e5 Bd7
  1 e4 e6 2 d4 d5 3 e5 c5 4 b4
  1 e4 e6 2 d4 d5 3 e5 c5 4 c3 Nc6 5 Nf3 Bd7
  1 e4 e6 2 d4 d5 3 e5 c5 4 c3 Nc6 5 Nf3 Qb6 6 a3 Nh6
  1 e4 e6 2 d4 d5 3 e5 c5 4 c3 Nc6 5 Nf3 Qb6 6 Bd3
  1 e4 e6 2 d4 d5 3 e5 c5 4 c3 Qb6 5 Nf3 Bd7
  1 e4 e6 2 d4 d5 3 e5 c5 4 dxc5
  1 e4 e6 2 d4 d5 3 e5 c5 4 Nf3 cxd4 5 Bd3
  1 e4 e6 2 d4 d5 3 e5 c5 4 Qg4 cxd4 5 Nf3
  1 e4 e6 2 d4 d5 3 exd5 exd5 4 c4
  1 e4 e6 2 d4 d5 3 exd5 exd5 4 Nc3 Bb4 5 Bd3 Ne7
  1 e4 e6 2 d4 d5 3 exd5 exd5 4 Nc3 Nf6 5 Bg5 Nc6
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 a3
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Bd2 dxe4 5 Qg4 Nf6 6 Qxg7 Rg8 7 Qh6
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Bd2 dxe4 5 Qg4 Qxd4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Bd2 Ne7 5 Nb1
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Bd3 c5 5 exd5 Qxd5 6 Bd2
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Ba5 6 b4 cxd4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 a4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 Nf3
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 Qg4 Qc7 8 Qxg7 Rg8 9 Qxh7 cxd4 10 Kd1
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 Qg4 Qc7 8 Qxg7 Rg8 9 Qxh7 cxd4 10 Ne2
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Qc7
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 cxd4 6 axb4 dxc3 7 Nf3
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 Bd2 Ne7 6 f4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 Qd7
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 exd5 exd5 5 Bd3 Ne7 6 Qh5
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Nge2 dxe4 5 a3 Be7 6 Nxe4 Nc6 7 g4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Nge2 dxe4 5 a3 Be7 6 Nxe4 Nf6 7 N2g3 0-0 8 Be2 Nc6
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Nge2 dxe4 5 a3 Bxc3+ 6 Nxc3 Nc6
  1 e4 e6 2 d4 d5 3 Nc3 c5
  1 e4 e6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bd7 5 Nf3 Bc6
  1 e4 e6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bd7 5 Nf3 Ngf6
  1 e4 e6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 e5
  1 e4 e6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Nf3 Ngf6 6 Nxf6+ Nxf6 7 Ne5
  1 e4 e6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Qd5
  1 e4 e6 2 d4 d5 3 Nc3 Nc6 4 exd5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bd3
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Be3
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Bc1
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Bd2 Bxc3 7 bxc3 Ne4 8 Qg4 Kf8 9 Bc1
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Bd2 Nfd7
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Be3
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Bh4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 exf6 hxg5 7 fxg7 Rg8 8 h4 gxh4 9 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 exd5 Qxd5 6 Bxf6 gxf6 7 Qd2 Qa5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 Ne2
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 Bxf6 Bxf6 6 e5 Be7 7 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Ne4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 Bd3
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 f4 0-0 8 Nf3 c5 9 Qd2 Nc6 10 0-0-0 c4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 Nb5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 Qd2 0-0 8 f4 c5 9 Nf3 Nc6 10 0-0-0 c4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 0-0
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 a6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 Bxg5 7 hxg5 Qxg5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 c5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 f6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Ng8 6 Be3 b6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 dxe4 5 Nxe4 Be7 6 Bxf6 Bxf6 7 Nf3 0-0
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 dxe4 5 Nxe4 Be7 6 Bxf6 gxf6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 e5 Nfd7 5 f4 c5 6 dxc5 Bxc5 7 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 e5 Nfd7 5 f4 c5 6 dxc5 Nc6 7 a3 Bxc5 8 Qg4 0-0 9 Nf3 f6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 e5 Nfd7 5 f4 c5 6 Nf3 Nc6 7 Be3
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 e5 Nfd7 5 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 exd5
  1 e4 e6 2 d4 d5 3 Nd2 a6
  1 e4 e6 2 d4 d5 3 Nd2 Be7
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 c3
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 exd5 5 Ngf3 c4
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 exd5 5 Ngf3 Nc6
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 Nf6
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 Qxd5 5 Ngf3 cxd4 6 Bc4 Qd6 7 0-0 Nf6 8 Nb3 Nc6 9 Nbxd4 Nxd4 10 Nxd4 a6
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 Qxd5 5 Ngf3 cxd4 6 Bc4 Qd8
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 Ngf3
  1 e4 e6 2 d4 d5 3 Nd2 f5
  1 e4 e6 2 d4 d5 3 Nd2 Nc6 4 c3 dxe4 5 Nxe4 e5
  1 e4 e6 2 d4 d5 3 Nd2 Nc6 4 Ngf3 Nf6
  1 e4 e6 2 d4 d5 3 Nd2 Nf6 4 e5 Nfd7 5 Bd3 c5 6 c3 b6
  1 e4 e6 2 d4 d5 3 Nd2 Nf6 4 e5 Nfd7 5 Bd3 c5 6 c3 Nc6 7 Ne2 cxd4 8 cxd4 Nb6
  1 e4 e6 2 d4 d5 3 Nd2 Nf6 4 e5 Nfd7 5 f4 c5 6 c3 Nc6 7 Ndf3 cxd4 8 cxd4 Nb6
  1 e4 e6 2 d4 d5 3 Nf3 dxe4 4 Ne5
  1 e4 e6 2 d4 d5 3 Nh3
  1 e4 e6 2 d4 d5 3 Qe2 e5 4 f4 exf4
  1 e4 e6 2 d4 d6
  1 e4 e6 2 d4 f5 3 exf5 Nf6
  1 e4 e6 2 d4 Nf6
  1 e4 e6 2 e5
  1 e4 e6 2 f4 d5 3 Nf3 dxe4
  1 e4 e6 2 g3
  1 e4 e6 2 Nc3 d5 3 f4
  1 e4 e6 2 Nc3 d5 3 Nf3
  1 e4 e6 2 Nf3 d5 3 e5 c5 4 b4
  1 e4 e6 2 Nf3 d5 3 Nc3
  1 e4 e6 2 Nf3 f5
  1 e4 e6 2 Qe2
  1 e4 f5
  1 e4 f6 2 d4 b6 3 c4 Bb7
  1 e4 f6 2 d4 Kf7
  1 e4 g5 2 d4 Bg7
  1 e4 g5 2 d4 e5
  1 e4 g5 2 d4 e6 3 c3 c5 4 dxc5 b6
  1 e4 g5 2 d4 h6 3 h4 g4
  1 e4 g6 2 Bc4 Bg7 3 Qf3 e6 4 d4 Bxd4 (5 Ne2 Bg7 6 Nbc3)
  1 e4 g6 2 d4 Bg7 3 Bc4 b5
  1 e4 g6 2 d4 Bg7 3 Bd2
  1 e4 g6 2 d4 Bg7 3 Bd3
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 d5 d6 5 Nc3 Bxc3+ 6 bxc3 Qa5
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 d5 d6 5 Nc3 Qa5
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 d5 Qa5+
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nc3 d6 5 Be3 Qa5
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nc3 d6 5 Nf3 Qa5
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nc3 d6 5 Nge2 Qa5
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nf3 d6 5 Be2 Qa5+
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nf3 d6 5 dxc5 Qa5+
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nf3 Qa5+
  1 e4 g6 2 d4 Bg7 3 c4 c5 4 Nf3 Qb6
  1 e4 g6 2 d4 Bg7 3 c4 d5 4 exd5 c6 5 dxc6 Bxd4
  1 e4 g6 2 d4 Bg7 3 f4 b6 4 c4 Bb7
  1 e4 g6 2 d4 Bg7 3 f4 c5 4 c3 Qa5
  1 e4 g6 2 d4 Bg7 3 f4 c5 4 Nf3 Qa5+
  1 e4 g6 2 d4 Bg7 3 g3 c5 4 dxc5 Qa5+
  1 e4 g6 2 d4 Bg7 3 g3 c5 4 Nf3 Qa5+
  1 e4 g6 2 d4 Bg7 3 Nc3 b6
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Be3
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 d5 Bxc3+ 5 bxc3 Qa5
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 d5 Qa5
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 dxc5 Bxc3+ 5 bxc3 Qa5 6 Nf3
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 dxc5 Qa5
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 Bc4
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 Bd2
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 Be2 d6
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 Be3
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 d5
  1 e4 g6 2 d4 Bg7 3 Nc3 c5 4 Nf3 Qa5 5 dxc5
  1 e4 g6 2 d4 Bg7 3 Nc3 c6 4 f4 d5 5 e5 h5
  1 e4 g6 2 d4 Bg7 3 Nc3 d5
  1 e4 g6 2 d4 Bg7 3 Nc3 d6 4 Bc4 c6 5 Qe2
  1 e4 g6 2 d4 Bg7 3 Nc3 d6 4 f4
  1 e4 g6 2 d4 Bg7 3 Nc3 d6 4 Nf3 c6 5 Bg5 Qb6 6 Qd2 Qxb2
  1 e4 g6 2 d4 Bg7 3 Nf3 b6
  1 e4 g6 2 d4 Bg7 3 Nf3 c5 4 Bc4 cxd4 5 Nxd4 Qa5+
  1 e4 g6 2 d4 Bg7 3 Nf3 c5 4 Be3 Qa5+
  1 e4 g6 2 d4 Bg7 3 Nf3 c5 4 c3 Qa5
  1 e4 g6 2 d4 Bg7 3 Nf3 c5 4 c4 Qb6
  1 e4 g6 2 d4 Bg7 3 Nf3 c5 4 dxc5 Qa5+
  1 e4 g6 2 d4 Bg7 3 Nf3 d6 4 c3
  1 e4 g6 2 d4 Bg7 3 Nf3 d6 4 c4 Bg4
  1 e4 g6 2 d4 d6 3 Nc3 c6
  1 e4 g6 2 d4 f5
  1 e4 g6 2 d4 Nf6 (3 e5 Nh5 4 g4 Ng7)
  1 e4 g6 2 d4 Nf6 3 e5 Nh5 4 Be2 d6
  1 e4 g6 2 d4 Nh6 3 Nc3 f5 4 Bxh6 Bxh6 5 exf5 0-0
  1 e4 g6 2 Nc3 Bg7 3 f4 c5 4 Nf3 Qa5
  1 e4 h5 2 d4 Nf6
  1 e4 h6 2 d4 e5
  1 e4 Na6
  1 e4 Nc6 2 b4 (Nxb4 3 c3 Nc6 4 d4)
  1 e4 Nc6 2 Bb5
  1 e4 Nc6 2 d4 a6
  1 e4 Nc6 2 d4 d5 3 Be3
  1 e4 Nc6 2 d4 d5 3 e5
  1 e4 Nc6 2 d4 d5 3 exd5 Nb4
  1 e4 Nc6 2 d4 d5 3 exd5 Qxd5 4 Nc3
  1 e4 Nc6 2 d4 d5 3 Nc3 a6
  1 e4 Nc6 2 d4 d5 3 Nc3 dxe4 4 d5 Nb8 5 f3
  1 e4 Nc6 2 d4 d5 3 Nc3 dxe4 4 d5 Ne5
  1 e4 Nc6 2 d4 d5 3 Nc3 e5
  1 e4 Nc6 2 d4 d5 3 Nc3 g6
  1 e4 Nc6 2 d4 d5 3 Nc3 Nf6
  1 e4 Nc6 2 d4 d6
  1 e4 Nc6 2 d4 e5 3 d5
  1 e4 Nc6 2 d4 e5 3 dxe5 Bc5
  1 e4 Nc6 2 d4 e5 3 dxe5 d6
  1 e4 Nc6 2 d4 e5 3 dxe5 f6
  1 e4 Nc6 2 d4 e5 3 dxe5 Nxe5 4 f4 Nc6
  1 e4 Nc6 2 d4 e5 3 dxe5 Nxe5 4 f4 Ng6
  1 e4 Nc6 2 d4 e5 3 dxe5 Nxe5 4 Nc3
  1 e4 Nc6 2 d4 e5 3 dxe5 Nxe5 4 Nf3
  1 e4 Nc6 2 d4 e5 3 dxe5 Qh4
  1 e4 Nc6 2 d4 e6 3 Nc3 f5 4 exf5 Nf6
  1 e4 Nc6 2 d4 e6 3 Nf3 f5 4 exf5 Nf6
  1 e4 Nc6 2 d4 f6
  1 e4 Nc6 2 Nc3 e6
  1 e4 Nc6 2 Nc3 g6
  1 e4 Nc6 2 Nc3 Nf6 3 d4 e5
  1 e4 Nc6 2 Nf3 d6
  1 e4 Nc6 2 Nf3 e6
  1 e4 Nc6 2 Nf3 f5 3 exf5
  1 e4 Nc6 2 Nf3 Nf6 3 e5 Ng4 4 d4 d6 5 h3 Nh6 6 Bb5
  1 e4 Nc6 2 Nf3 Nf6 3 e5 Ng4 4 d4 d6 5 h3 Nh6 6 e6
  1 e4 Nc6 2 Nf3 Nf6 3 e5 Ng4 4 d4 d6 5 h3 Nh6 6 exd6
  1 e4 Nf6 2 Bc4 Nxe4 3 Bxf7+
  1 e4 Nf6 2 d3
  1 e4 Nf6 2 d4
  1 e4 Nf6 2 e5 Nd5 3 b3
  1 e4 Nf6 2 e5 Nd5 3 Bc4 Nb6 4 Bb3 c5 5 d3
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb4 4 d4
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 b3
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 c5 Nd5 5 Bc4 e6 6 Nc3 d6 7 Nxd5 exd5 8 Bxd5
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 c5 Nd5 5 Nc3 e6 6 Bc4
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 c5 Nd5 5 Nc3 Nxc3 6 dxc3 d6 7 Bg5
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 d4 d6 5 exd6 cxd6 6 h3 g6 7 Nf3
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 d4 d6 5 Nf3 Bg4 6 Be2 dxe5 7 Nxe5
  1 e4 Nf6 2 e5 Nd5 3 c4 Nf4
  1 e4 Nf6 2 e5 Nd5 3 d4 b5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Bc4
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 (Nb6 5 c5)
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 exd6 cxd6 6 Nf3 g6 7 Be2 Bg7 8 0-0 0-0 9 h3 Nc6 10 Nc3 Bf5 11 Bf4
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 Bf5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Bf5 7 Nc3 e6 8 Nf3 Bb4 9 Bd3
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Bf5 7 Nc3 e6 8 Nf3 Be7 9 Be2 0-0 10 0-0 f6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Nc6 7 Be3 Bf5 8 Nc3 e6 9 Nf3 Qd7 10 Be2 0-0-0 11 0-0 Be7
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Nc6 7 Nf3 Bg4 8 e6 fxe6 9 c5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 g5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 g6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Bg4 5 Be2 c6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Bg4 5 c4 Nb6 6 Be2
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Bg4 5 c4 Nb6 6 d5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Bg4 5 h3
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 dxe5 5 Nxe5 c6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 g6 5 Bc4 Nb6 6 Bb3 Bg7 7 a4
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 g6 5 c4 Nb6 6 exd6 cxd6 7 h3 Bg7 8 Be2 0-0 9 Nc3 Nc6 10 0-0 Bf5 11 Bf4
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Nb6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 Nc6
  1 e4 Nf6 2 e5 Nd5 3 Na3
  1 e4 Nf6 2 e5 Nd5 3 Nc3
  1 e4 Nf6 2 e5 Ne4 3 d4 e6
  1 e4 Nf6 2 e5 Ne4 3 d4 f6
  1 e4 Nf6 2 e5 Ng8 3 d4 f5
  1 e4 Nf6 2 e5 Ng8 3 d4 Nc6 4 d5 Nb8
  1 e4 Nf6 2 Nc3 d5 3 d3 dxe4 4 Bg5
  1 e4 Nf6 2 Nc3 d5 3 e5 Nfd7 4 e6
  1 e4 Nf6 2 Nc3 d5 3 exd5 c6
  1 e4 Nf6 2 Nf3
  1 e4 Nh6 2 d4 g6 3 c4 f6
  1 f3 c5 2 Kf2 Nc6 3 Ke3 e6 4 c3 Be7 5 Kd3 Nf6 6 Kc2 0-0 7 Qe1 Nd5 8 Kd1
  1 f3 d5 2 e4 g6 3 d4 dxe4 4 c3
  1 f3 e5 2 e4 Nf6 3 Bc4
  1 f3 e5 2 Kf2
  1 f3 f5 2 e4 fxe4 3 Nc3
  1 f4 b5
  1 f4 d5 2 b3 Nf6 3 Bb2 d4 4 Nf3 c5 5 e3
  1 f4 d5 2 c4
  1 f4 d5 2 d4 f5
  1 f4 d5 2 e3 Nf6 3 Nf3 Bg4
  1 f4 d5 2 e3 Nf6 3 Nf3 c5
  1 f4 d5 2 e4 dxe4 3 d3
  1 f4 d5 2 e4 dxe4 3 Nc3 Nf6 4 Nge2
  1 f4 d5 2 g4
  1 f4 d5 2 Nf3 c5 3 e4 dxe4
  1 f4 d5 2 Nf3 Nf6 3 e3 c5
  1 f4 e5 2 d4 exd4 3 Nf3 c5 4 c3
  1 f4 e5 2 fxe5 d6 3 exd6 Bxd6 4 Nf3 g5
  1 f4 e5 2 fxe5 d6 3 exd6 Bxd6 4 Nf3 Nh6 5 d4
  1 f4 e5 2 fxe5 d6 3 exd6 Nf6
  1 f4 e5 2 fxe5 f6
  1 f4 e5 2 fxe5 Nc6
  1 f4 e5 2 fxe5 Ne7
  1 f4 e5 2 Nc3
  1 f4 f5 2 d4 d5
  1 f4 f5 2 e4 fxe4 3 Nc3 Nf6 4 g4
  1 f4 g5
  1 f4 h6 2 Nf3 g5
  1 f4 Nf6 2 b4 g6 3 Nf3
  1 f4 Nf6 2 c4
  1 f4 Nf6 2 Nf3 g6 3 b4
  1 f4 Nh6
  1 g3 c5
  1 g3 d5 2 Bg2 c5
  1 g3 d5 2 Bg2 c6
  1 g3 d5 2 Bg2 e5 3 c4 dxc4 4 b3
  1 g3 d5 2 Bg2 e6
  1 g3 d5 2 Nf3 g5
  1 g3 e5 2 a3 d5 3 Nf3 e4 4 Nh4 Be7 5 d3
  1 g3 e5 2 Bg2 d5 3 b4
  1 g3 e5 2 Nf3 e4 3 Ng1 Nf6 4 b4
  1 g3 e5 2 Nf3 e4 3 Nh4
  1 g3 e5 2 Nh3 d5 3 f4 Bxh3 4 Bxh3 exf4 5 0-0
  1 g3 f5 2 e4 fxe4 3 Qh5+ g6
  1 g3 g5
  1 g3 g6
  1 g3 h5 2 Nf3 h4
  1 g3 Nc6 2 Nc3 d5 3 d4 e5 4 dxe5 d4 5 Ne4 f5
  1 g3 Nf6
  1 g4 d5 2 Bg2 Bxg4 3 c4 d4
  1 g4 d5 2 Bg2 c6 3 c4 dxc4 4 b3
  1 g4 d5 2 Bg2 c6 3 g5
  1 g4 d5 2 Bg2 e5 3 c4
  1 g4 d5 2 Bg2 e5 3 d4 exd4 4 c3
  1 g4 d5 2 Bg2 h5 3 gxh5
  1 g4 d5 2 e4 dxe4 3 Nc3 e5 4 d3
  1 g4 d5 2 e4 dxe4 3 Nc3 h5
  1 g4 d5 2 h3 e5 3 Bg2 c6 4 d4 e4 5 c4 Bd6 6 Nc3 Ne7
  1 g4 e5 2 Bg2 d5 3 c4
  1 g4 e5 2 h3 d5 3 Bg2 c6
  1 g4 e5 2 h3 Nc6
  1 g4 f5
  1 g4 g5 2 f4
  1 h3 e5 2 a3 d5
  1 h3 h5 2 g4
  1 h4 c5 2 b4
  1 h4 d5 2 d4 c5 3 e3
  1 h4 d5 2 d4 c5 3 Nf3 cxd4 4 c3
  1 h4 d5 2 Rh3
  1 h4 e5 2 d4 exd4 3 c3
  1 h4 f5 2 e4 fxe4 3 d3
  1 h4 g5
  1 Na3 e5 2 d3 Bxa3 3 bxa3 d5 4 e3 c5 5 Rb1
  1 Na3 e5 2 Nc4 Nc6 3 e4 f5
  1 Na3 g6 2 g4
  1 Nc3 b5
  1 Nc3 c5 2 b4
  1 Nc3 c5 2 d4 cxd4 3 Qxd4 Nc6 4 Qh4
  1 Nc3 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4
  1 Nc3 c5 2 Rb1
  1 Nc3 d5 2 d3 Nf6 3 g3
  1 Nc3 d5 2 e3 e5 3 d4 Bb4
  1 Nc3 d5 2 e4 c6 3 h3
  1 Nc3 d5 2 e4 dxe4 3 Bc4
  1 Nc3 d5 2 e4 dxe4 3 d3
  1 Nc3 d5 2 e4 dxe4 3 f3
  1 Nc3 d5 2 e4 dxe4 3 Nxe4 e5 4 f4
  1 Nc3 d5 2 e4 Nf6 3 d3 dxe4 4 Bg5
  1 Nc3 d5 2 f4 d4 3 Ne4 c5
  1 Nc3 d5 2 f4 d4 3 Ne4 e5 4 Nf3
  1 Nc3 d5 2 f4 d4 3 Ne4 f5 4 Nf2 Nf6 5 Nf3 c5 6 b4
  1 Nc3 d5 2 f4 e5
  1 Nc3 d5 2 f4 g5
  1 Nc3 d6 2 f4 e5 3 fxe5 Nc6
  1 Nc3 e5 2 a3
  1 Nc3 e5 2 b3 d5 3 e4 dxe4 4 d3
  1 Nc3 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qa4
  1 Nc3 e5 2 e3 d5 3 Qh5 Be6
  1 Nc3 e5 2 e3 d5 3 Qh5 Nf6
  1 Nc3 e5 2 f4 exf4 3 e4
  1 Nc3 e5 2 Nf3 Bc5
  1 Nc3 e5 2 Nf3 Nc6 3 d4
  1 Nc3 f5 2 e4 fxe4 3 d3
  1 Nc3 g6 2 h3
  1 Nc3 Nc6 2 d4 d5 3 e4 dxe4 4 d5
  1 Nc3 Nf6 2 g4
  1 Nf3 a5
  1 Nf3 a6
  1 Nf3 b5
  1 Nf3 b6
  1 Nf3 c5 2 d4 cxd4 3 e3
  1 Nf3 c5 2 g3 d5 3 Bg2 Nc6 4 d4 e6 5 0-0
  1 Nf3 c5 2 Nc3 Nc6 3 d4 cxd4 4 Nxd4
  1 Nf3 c6
  1 Nf3 d5 2 a4
  1 Nf3 d5 2 b3 c5 3 c4 dxc4 4 e3
  1 Nf3 d5 2 b3 c5 3 c4 dxc4 4 Nc3
  1 Nf3 d5 2 b3 c5 3 e4
  1 Nf3 d5 2 b3 Nf6 3 Bb2 c5 4 e4
  1 Nf3 d5 2 b4
  1 Nf3 d5 2 c4 b5
  1 Nf3 d5 2 c4 c6 3 b3 Bg4
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 Bb2 Bf5
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 Bb2 Bg4
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 Bb2 e6 5 g3 Be7 6 Bg2 0-0 7 0-0
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 Bb2 g6
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 g3 Bf5
  1 Nf3 d5 2 c4 c6 3 b3 Nf6 4 g3 Bg4
  1 Nf3 d5 2 c4 d4 3 b4 c5
  1 Nf3 d5 2 c4 d4 3 e3 c5 4 b4
  1 Nf3 d5 2 c4 d4 3 Rg1
  1 Nf3 d5 2 c4 dxc4 3 e3 Be6
  1 Nf3 d5 2 d3
  1 Nf3 d5 2 e4
  1 Nf3 d5 2 g3 Bg4 3 Bg2 Nd7
  1 Nf3 d5 2 g3 c5 3 Bg2 Nc6 4 0-0 e6 5 d3 Nf6 6 Nbd2 Be7 7 e4 0-0 (8 Re1)
  1 Nf3 d5 2 g3 e5
  1 Nf3 d5 2 g3 g6 3 Bg2 Bg7 4 0-0 e5 5 d3 Ne7
  1 Nf3 d5 2 g3 Nc6 3 Bg2 e5 4 d3
  1 Nf3 d5 2 g3 Nf6 3 Bg2 c5 4 d4
  1 Nf3 d5 2 g3 Nf6 3 Bg2 c6 4 0-0 Bg4
  1 Nf3 d5 2 Nc3
  1 Nf3 d5 2 Rg1
  1 Nf3 d6 2 d4 Bg4 3 c4 Nd7 4 Qb3 Rb8
  1 Nf3 d6 2 d4 e5
  1 Nf3 d6 2 e4 Bg4
  1 Nf3 e5 2 Nxe5 Nc6 3 Nxc6 dxc6
  1 Nf3 e6 2 c4 a6 3 Nc3 c5 4 g3 b5
  1 Nf3 f5 2 d3 Nf6 3 e4
  1 Nf3 f5 2 e4
  1 Nf3 f6 2 e4 Nh6 3 d4 Nf7
  1 Nf3 g5
  1 Nf3 g6
  1 Nf3 h6
  1 Nf3 Na6 2 e4 c5
  1 Nf3 Na6 2 e4 Nh6
  1 Nf3 Nc6
  1 Nf3 Nf6 2 a4 g6 3 b4
  1 Nf3 Nf6 2 b3 g6 3 Bb2 Bg7 4 g3 0-0 5 Bg2 d6 6 0-0
  1 Nf3 Nf6 2 e3
  1 Nf3 Nf6 2 e4
  1 Nf3 Nf6 2 g3 b5
  1 Nf3 Nf6 2 g3 g5 3 b4
  1 Nf3 Nf6 2 g3 g6 3 b3 Bg7 4 Bb2 0-0 5 Bg2 d6 6 0-0
  1 Nf3 Nf6 2 g3 g6 3 b4
  1 Nf3 Nf6 2 g3 g6 3 Bg2 Bg7 4 0-0 0-0 5 d3 d5
  1 Nf3 Nf6 2 Nc3 Nc6
  1 Nf3 Nh6 2 d4 g6
  1 Nh3 d5 2 f3 e5 3 e4 f5
  1 Nh3 d5 2 g3 e5 3 f4 Bxh3 4 Bxh3 exf4 5 0-0 fxg3 6 hxg3"

let chess_openings_efficace =
  "1 c3 Nf6 2 Qa4 e6 3 f3 Bc5 4 Qh4 Be7 5 Kd1 Nc6 6 Qe1 b6 7 e4 Bb7 8 d4 0-0 9 Bd3 a6
  1 c4 c5 2 Nc3 Nf6 3 Nf3 d5 4 cxd5 Nxd5 5 g3 Nc6 6 Bg2 e6 7 0-0 Be7
  1 c4 c5 2 Nf3 Nf6 3 g3 b6 4 Bg2 Bb7 5 0-0 e6 6 Nc3 Be7 7 d4 cxd4 8 Qxd4 d6 9 Rd1 a6 10 b3 Nbd7
  1 c4 e5 2 Nc3 Nc6 3 Nf3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Bg5 h6 (7 Bh4 Bxc3+ 8 bxc3 Ne5)
  1 c4 e5 2 Nc3 Nf6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 e4 0-0 7 Nge2 c6 8 0-0 a6
  1 c4 e6 2 Nf3 d5 3 g3 Nf6 4 Bg2 Be7 5 0-0 c5 6 cxd5 Nxd5 7 Nc3 Nc6
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 cxd5 cxd5 5 Nc3 Nc6 6 Bf4 Bf5 7 e3 e6 8 Qb3 Bb4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bf5 5 cxd5 cxd5 6 Nc3 e6 7 Ne5 Nfd7
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 e3 Bf5 5 cxd5 cxd5 6 Qb3 Qc8 7 Bd2 e6 8 Na3
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 e3 e6 7 Bxc4 Bb4 8 0-0 0-0 9 Qe2 Ne4 10 g4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Ne5 e6 7 f3 Bb4 8 e4
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bf5 6 Ne5 Nbd7 7 Nxc4 Qc7 8 g3 e5 9 dxe5 Nxe5 10 Bf4 Nfd7 11 Bg2 g5
  1 d4 d5 2 c4 c6 3 Nf3 Nf6 4 Nc3 dxc4 5 a4 Bg4 6 e3 b5 7 Bd2 a5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 dxc5 Bxc5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 b5 8 Bb3 Bb7 9 Rd1 Nbd7 10 Nc3 Bd6
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 b5 8 Bb3 Nc6 9 Rd1 c4 10 Bc2 Nb4 11 Nc3 Nxc2 12 Qxc2 Bb7 13 d5 Qc7
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 Nc6 8 dxc5 Bxc5 9 e4 b5 10 e5
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 0-0 a6 7 Qe2 Nc6 8 Rd1 b5 9 Bb3 c4 10 Bc2 Nb4 11 Nc3 Nxc2 12 Qxc2 Bb7 13 d5 Qc7
  1 d4 d5 2 c4 dxc4 3 Nf3 Nf6 4 e3 e6 5 Bxc4 c5 6 Qe2 a6 7 dxc5 Bxc5 8 0-0 Nc6 9 e4 b5 10 e5
  1 d4 d5 2 c4 e5 3 dxe5 d4 4 Nf3 Nc6 5 Nbd2 Bg4 5 h3 Bxf3 7 Nxf3 Bb4+ 8 Bd2 Qe7
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 cxd4 5 Qxd4 Nc6 6 Qd1 exd5 7 Qxd5 Be6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 dxc5 d4 7 Na4 b5
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 Be6 10 Rc1 b6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 Be6 10 Rc1 c4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 c4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 h6 11 Be3 Bg4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 h6 11 Be3 Re8 12 Rc1 Be6
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 Bg5 cxd4 10 Nxd4 Re8
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 dxc5 Bxc5 10 Na4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Be7 8 0-0 0-0 9 dxc5 d4
  1 d4 d5 2 c4 e6 3 Nc3 c5 4 cxd5 exd5 5 Nf3 Nc6 6 g3 Nf6 7 Bg2 Bg4
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 e4 dxe4 5 Nxe4 Bb4+ 6 Bd2 Qxd4 7 Bxb4 Qxe4+ 8 Be2 c5 9 Bxc5 Qxg2
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 (7 Bd2 a5) (8 axb5 Bxc3 9 Bxc3 cxb5 10 b3 Bb7)
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 7 Bd2 Qb6
  1 d4 d5 2 c4 e6 3 Nc3 c6 4 Nf3 dxc4 5 a4 Bb4 6 e3 b5 7 Bd2 Qe7
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 b6 7 Bd3 Bb7 8 cxd5 exd5 9 Ne5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 b6 8 cxd5 exd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 b6 8 cxd5 Nxd5 9 Bxe7 Qxe7 10 Nxd5 exd5 11 Rc1 Be6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 cxd5 Nxc3 10 bxc3 exd5 11 Qb3 Qd6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 cxd5 Nxc3 10 bxc3 exd5 11 Qb3 Rd8 12 c4 Be6
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 Qc2 Nf6 10 Bd3 dxc4 11 Bxc4 c5 12 0-0 Nc6 13 Rfd1 Bd7
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bh4 Ne4 8 Bxe7 Qxe7 9 Rc1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 h6 7 Bxf6 Bxf6 8 Rc1 c6 9 Bd3 Nd7 10 0-0 dxc4 11 Bxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Qc2 c5 8 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 a6 8 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Bb5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Bd3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 b6 8 cxd5 exd5 9 Qa4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 b5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 dxe5 Nxe5 14 Nxe5 Qxe5 15 f4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 Qb1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 0-0 Nxc3 12 Rxc3 e5 13 Qc2
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 Bxe7 Qxe7 11 Ne4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Bd3 dxc4 9 Bxc4 Nd5 10 h4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 a6 9 a3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 a6 9 cxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 e3 0-0 6 Nf3 Nbd7 7 Rc1 c6 8 Qc2 Ne4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Be7 5 Nf3 h6 6 Bh4 0-0 7 Rc1 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 c5 5 Nf3 cxd4 6 Nxd4 e5 7 Ndb5 a6 8 Qa4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 cxd5 Nxd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 Nd2 Bb4 8 Qc2 0-0 9 Bh4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Bg5 Nbd7 5 e3 c6 6 Nf3 Qa5 7 Nd2 dxc4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 cxd5 exd5 5 Bg5 Be7 6 e3 0-0 7 Bd3 Nbd7 8 Qc2 Re8 9 Nge2 Nf8 10 0-0-0
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 c5 7 dxc5 Bxc5 8 Qc2 Nc6 9 a3 Qa5 10 0-0-0
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 Be7 5 Bf4 0-0 6 e3 c5 7 dxc5 Bxc5 8 Qc2 Nc6 9 a3 Qa5 10 Rd1
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 cxd5 Nxd5 6 e4 Nxc3 7 bxc3 cxd4 8 cxd4 Bb4+ 9 Bd2 Bxd2+ 10 Qxd2 0-0 11 Bb5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 cxd5 Nxd5 6 e4 Nxc3 7 bxc3 cxd4 8 cxd4 Bb4+ 9 Bd2 Qa5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c5 5 e3 Nc6 6 Bd3 Bd6 7 0-0 0-0 8 Qe2 Qe7 9 dxc5 Bxc5 10 e4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 exf6 gxh4 10 Ne5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 hxg5 10 Bxg5 Nbd7 11 g3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 hxg5 10 Bxg5 Nbd7 11 Qf3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 Bg5 dxc4 6 e4 b5 7 e5 h6 8 Bh4 g5 9 Nxg5 Nd5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 b4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 d5
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Ng4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Nxe5 12 Nxe5 axb5 13 0-0 Qd5 14 Qe2 Ba6 (15 Bg5)
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 a6 9 e4 c5 10 e5 cxd4 11 Nxb5 Nxe5 12 Nxe5 axb5 13 Qf3
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 b4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Bd3 dxc4 7 Bxc4 b5 8 Bd3 Bb7 9 e4 b4 10 Na4 c5 11 e5 Nd5 12 0-0 cxd4 13 Nxd4
  1 d4 d5 2 c4 e6 3 Nc3 Nf6 4 Nf3 c6 5 e3 Nbd7 6 Qc2 Bd6 7 e4 dxe4 8 Nxe4 Nxe4 9 Qxe4 e5 10 dxe5
  1 d4 d5 2 c4 Nc6 3 cxd5 Qxd5 4 e3 e5 5 Nc3 Bb4 6 Bd2 Bxc3 7 Bxc3 exd4 8 Ne2
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 Bg5 Bf5 5 Bxf6 exf6 6 g4 Bg6 7 Qe2 Bb4 8 Qb5+
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 e6 6 Bg5 Be7 7 Bd3 Nc6 8 0-0 Nxd4 9 Kh1
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Kh1
  1 d4 d5 2 e4 dxe4 3 Nc3 Nf6 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Qe1
  1 d4 d5 2 Nc3 Nf6 3 e4 dxe4 4 f3 exf3 5 Nxf3 g6 6 Bc4 Bg7 7 0-0 0-0 8 Qe1
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 c5 5 b3 Nc6 6 Bb2 Bd6 7 0-0 0-0
  1 d4 d5 2 Nf3 Nf6 3 e3 e6 4 Bd3 c5 5 b3 Nc6 6 Bb2 Be7 7 0-0 0-0
  1 d4 e6 2 c4 c5 3 d5 exd5 4 cxd5 d6 5 Nc3 g6 6 e4 Bg7 7 Nf3 Ne7
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d5 7 b3 c6 8 Ba3
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d5 7 Nc3 c6 8 Qc2 Qe8 9 Bg5
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 a5
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Ne4
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 b3
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 Qc2
  1 d4 f5 2 c4 Nf6 3 g3 e6 4 Bg2 Be7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Qe8 8 Re1
  1 d4 f5 2 c4 Nf6 3 g3 g6 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 d6 7 Nc3 c6
  1 d4 f5 2 c4 Nf6 3 g3 g6 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 d6 7 Nc3 Nc6
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 d6 7 e4 Bxf1 8 Kxf1 g6 9 g3 Bg7 10 Kg2 0-0 11 Nf3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 d6 7 Nf3 g6 8 g3
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 bxa6 Bxa6 6 Nc3 g6 7 Nf3 d6 8 g3 (Bg7 9 Bg2)
  1 d4 Nf6 2 c4 c5 3 d5 b5 4 cxb5 a6 5 Nc3 axb5 6 e4 b4 7 Nb5 (d6 8 Bc4)
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 Bb5+
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 e5
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 f4 Bg7 8 Nf3 0-0 9 Be2 Re8
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Be2 0-0 9 0-0 a6 10 a4 Bg4
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Be2 0-0 9 0-0 Re8 10 Nd2 Na6 11 f3
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 Bg5
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 e4 g6 7 Nf3 Bg7 8 h3
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 g3 Bg7 8 Bg2 0-0 9 0-0 a6 10 a4 Nbd7 11 Nd2 Re8
  1 d4 Nf6 2 c4 c5 3 d5 e6 4 Nc3 exd5 5 cxd5 d6 6 Nf3 g6 7 g3 Bg7 8 Bg2 0-0 9 0-0 Nbd7 10 Nd2 a6 11 a4 Re8
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Nc3 c6 8 Qd3
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 b3 b6 9 Rd1 Bb7 10 Nc3 b5
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Nbd2 b5
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Nbd2 b6 9 b3 a5 10 Bb2 Ba6
  1 d4 Nf6 2 c4 e6 3 g3 d5 4 Bg2 Be7 5 Nf3 0-0 6 0-0 Nbd7 7 Qc2 c6 8 Rd1 b6 9 a4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 0-0 6 e3 c5 7 Bd3 Nc6 8 Ne2 b6 9 e4 Ne8
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 c5 6 f3 d5 7 cxd5 Nxd5 8 dxc5 f5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 a3 Bxc3+ 5 bxc3 c5 6 f3 d5 7 e3 0-0 8 cxd5 Nxd5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Bd3 d5 6 a3 Bxc3+ 7 bxc3 c5 8 cxd5 exd5 9 Ne2 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 (6 Bd3 Nc6 7 0-0 dxc4)
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 b6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 b6 9 Qe2 Bb7 10 Rd1 Qc8
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Bd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Nbd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 dxc4 8 Bxc4 Qe7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nbd7
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nc6 8 a3 Bxc3 9 bxc3 dxc4 10 Bxc4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 0-0 5 Nf3 d5 6 Bd3 c5 7 0-0 Nc6 8 a3 dxc4 9 Bxc4 cxd4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 e3 c5 5 Bd3 Nc6 6 Nf3 Bxc3+ 7 bxc3 d6
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Nf3 c5 5 g3 0-0 6 Bg2 cxd4 7 Nxd4 d5 8 cxd5 Nxd5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qb3 c5 5 dxc5 Nc6 6 Nf3 Ne4 7 Bd2 Nxc5 8 Qc2 f5 9 g3
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qb3 c5 5 dxc5 Nc6 6 Nf3 Ne4 7 Bd2 Nxd2
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 c5 5 dxc5 0-0 6 a3 Bxc5 7 Nf3 b6 8 Bf4
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 a3 Bxc3+ 6 Qxc3 Ne4 7 Qc2 Nc6 8 e3 e5
  1 d4 Nf6 2 c4 e6 3 Nc3 Bb4 4 Qc2 d5 5 cxd5 Qxd5 6 Nf3 Qf5 7 Qd1 e5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 a3 Bb7 5 Nc3 d5 6 cxd5 Nxd5 7 Qc2 c5 8 e4 Nxc3 9 bxc3 Nc6 10 Bb2 cxd4 11 cxd4 Rc8 12 Rd1 Bd6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 e3 Bb7 5 Bd3 c5 6 0-0 Be7 7 b3 0-0 8 Bb2 cxd4 9 Nxd4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Bb4+ 6 Bd2 Bxd2+ 7 Qxd2 0-0 8 Nc3 Ne4 9 Qc2 Nxc3 10 Ng5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 d5 exd5 8 Nd4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 d5 exd5 8 Nh4
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 d5
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 Na6
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 g3 Bb7 5 Bg2 Be7 6 0-0 0-0 7 Nc3 Ne4 8 Qc2 Nxc3 9 Qxc3
  1 d4 Nf6 2 c4 e6 3 Nf3 b6 4 Nc3 Bb7 5 Bg5 h6 6 Bh4 g5 7 Bg3 Nh5
  1 d4 Nf6 2 c4 e6 3 Nf3 Bb4+ 4 Bd2 Bxd2+ 5 Qxd2 b6 6 g3 Bb7 7 Bg2 0-0 8 Nc3 Ne4 9 Qc2 Nxc3 10 Ng5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 0-0 c6 7 cxd5 cxd5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 c5 8 dxc5
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 c5 8 Nc3
  1 d4 Nf6 2 c4 g6 3 g3 d5 4 Bg2 Bg7 5 Nf3 0-0 6 cxd5 Nxd5 7 0-0 Nb6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 c5 7 d5 e6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Be2 0-0 6 Bg5 Na6 7 Qd2 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 c6 7 Bd3 a6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 d5 c6 8 Nge2 (cxd5)
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 d5 Nh5 8 Qd2 Qh4+ 9 g3 Nxg3 10 Qf2 Nxf1 11 Qxh4 Nxe3 12 Ke2 (Nxc4)
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 e5 7 Nge2 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 Nc6 7 Nge2 a6 8 Qd2 Rb8
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 0-0 6 Be3 Nc6 7 Nge2 Rb8
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f3 e5 6 d5 Nh5 7 Be3 Na6 8 Qd2 Qh4+ 9 g3 Nxg3 10 Qf2 Nxf1 11 Qxh4 Nxe3 12 Kf2 Nxc4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 d5 e6 8 dxe6 fxe6 9 g4 Nc6 10 h4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 d5 e6 8 Nf3 exd5 9 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 f4 0-0 6 Be2 c5 7 Nf3 cxd4 8 Nxd4 Nc6 9 Be3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nbd7 8 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nbd7 8 Re1 c6 9 Bf1 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 b4 Nh5 10 Qc2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 b4 Nh5 10 Re1
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Bd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Nd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Ne1 Nd7 10 Be3 f5 11 f3 f4 12 Bf2 g5 13 Rc1 Ng6 14 c5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 0-0 Nc6 8 d5 Ne7 9 Ne1 Nd7 10 f3 f5 11 g4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 d5 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 e4 d6 5 Nf3 0-0 6 Be2 e5 7 d5 Nbd7 8 Bg5 h6 9 Bh4 g5 10 Bg3 Nh5 11 h4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c5 7 0-0 Nc6 8 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c5 7 0-0 Nc6 8 dxc5 dxc5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Qa5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 c6 7 0-0 Qb6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nbd7 7 0-0 e5 8 e4 c6 9 h3
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nbd7 7 0-0 e5 8 e4 Re8 9 h3 exd4 10 Nxd4 Nc5 11 Re1 a5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 d5 Na5 9 Nd2 c5 10 Qc2 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 d5 Na5 9 Nd2 c5 10 Qc2 Rb8 11 b3 b5 12 Bb2 bxc4 13 bxc4 Bh6
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 a6 8 h3 Rb8 9 Be3 b5 10 Nd2
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 Bg4
  1 d4 Nf6 2 c4 g6 3 Nc3 Bg7 4 Nf3 d6 5 g3 0-0 6 Bg2 Nc6 7 0-0 e5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 cxd5 Nxd5 (7 Nxd5 Qxd5 8 Bxc7)
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 Rc1 c5 7 dxc5 Be6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Bf4 Bg7 5 e3 0-0 6 Rc1 c5 7 dxc5 Qa5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 b6 9 h4 Ba6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 Bg4 11 f3 Na5 12 Bxf7+
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 cxd4 11 cxd4 Bg4 12 f3 Na5 13 Bd3 Be6 14 d5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 c5 9 0-0 Nc6 10 Be3 cxd4 11 cxd4 Bg4 12 f3 Na5 13 Rc1
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 Nc6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Bc4 0-0 8 Ne2 Qd7 (9 0-0 b6)
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Nf3 c5 8 h3
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 cxd5 Nxd5 5 e4 Nxc3 6 bxc3 Bg7 7 Nf3 c5 8 Rb1 0-0 9 Be2 Nc6 10 d5 Bxc3+
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Bd3 c6 7 0-0 Bf5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Bd3 c6 7 0-0 Bg4
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 0-0 6 Qb3 dxc4 7 Bxc4 Nbd7 8 Ng5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 e3 c5 6 Be2 cxd4 7 exd4 Nc6 8 0-0 0-0
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 a6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 b6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Bg4 8 Be3 Nfd7 9 Be2 Nb6 10 Qd3 Nc6 11 0-0-0
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Bg4 8 Be3 Nfd7 9 Qb3 c5
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 c6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Na6
  1 d4 Nf6 2 c4 g6 3 Nc3 d5 4 Nf3 Bg7 5 Qb3 dxc4 6 Qxc4 0-0 7 e4 Nc6
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 0-0 5 Bg2 d6 6 0-0 Nc6 7 d5 Na5
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 d6 5 Bg2 0-0 6 0-0 Nbd7 7 Nc3 a6
  1 d4 Nf6 2 c4 g6 3 Nf3 Bg7 4 g3 d6 5 Bg2 0-0 6 0-0 Nbd7 7 Nc3 e5 8 b3
  1 e4 c5 2 b4 cxb4 3 a3 d5 4 exd5 Qxd5 5 Nf3 e5 6 Bb2 Nc6 7 c4 Qe6
  1 e4 c5 2 c3 d5 3 exd5 Qxd5 4 d4 cxd4 5 cxd4 Nc6 6 Nf3 Bg4 7 Nc3 Bxf3 8 gxf3 Qxd4 9 Qxd4 Nxd4
  1 e4 c5 2 c3 d5 3 exd5 Qxd5 4 d4 cxd4 5 cxd4 Nc6 6 Nf3 e5 7 Nc3 Bb4 8 Be2
  1 e4 c5 2 c3 Nf6 3 e5 Nd5 4 Nf3 Nc6 5 Bc4 Nb6 6 Bb3 c4 7 Bc2 Qc7 8 Qe2 g5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 d6 5 Bc4 e6 6 Nf3 Nf6 7 0-0 a6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 d6 6 Bc4 a6 7 0-0 Nf6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 a6 7 0-0 b5 8 Bb3 Bc5
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 a6 7 0-0 Qc7 8 Qe2 Bd6
  1 e4 c5 2 d4 cxd4 3 c3 dxc3 4 Nxc3 Nc6 5 Nf3 e6 6 Bc4 d6 7 0-0 a6 8 Qe2 b5
  1 e4 c5 2 Nc3 Nc6 3 g3 g6 4 Bg2 Bg7 5 d3 d6 6 f4 e5 7 Nh3 Nge7
  1 e4 c5 2 Nf3 d6 3 Bb5+ Nc6 4 0-0 Bd7 5 c3 a6 6 Bxc6 Bxc6 7 Re1 Nf6 8 d4 Bxe4 9 Bg5
  1 e4 c5 2 Nf3 d6 3 c3 Nf6 4 Be2 Nc6 5 d4 cxd4 6 cxd4 Nxe4 7 d5 Qa5+ 8 Nc3 Nxc3 9 bxc3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 g6 5 Nc3 Bg7 6 Be3 Nf6 7 Bc4 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Be2 e5 7 Nb3 Be7 8 0-0 0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Be2 e5 7 Nb3 Be7 8 0-0 Be6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 b5 8 e5 dxe5 9 fxe5 Qc7 10 Qe2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 h6 9 Bh4 g5 10 fxg5 Nfd7 11 Nxe6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 h6 9 Bh4 Qc7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Be7 8 Qf3 Qc7 9 0-0-0 Nbd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 e6 7 f4 Qb6 8 Qd2 Qxb2 9 Rb1 Qa3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 a6 6 Bg5 Nbd7 7 Bc4 Qa5 8 Qd2 e6 9 0-0-0 b5 10 Bb3 Bb7 11 Rhe1 Nc5 12 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 a6 7 Bb3 b5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Bb3 (Be7 8 Be3 0-0 9 f4)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Be3 Be7 8 Bb3 0-0 9 0-0 Na5 10 f4 b6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Bc4 Nc6 7 Be3 Be7 8 Qe2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Be7 8 f4 0-0 9 Be3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Nbd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Qc7 8 f4 Nc6 9 Be3 Be7 10 Qe1 0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 a6 7 0-0 Qc7 8 f4 Nc6 9 Kh1 Be7 10 a4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 Nc6 7 0-0 Be7 8 Be3 0-0 9 f4 Bd7 10 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be2 Nc6 7 0-0 Be7 8 Kh1
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 Be3 a6 7 g4 e5 8 Nf5 g6 9 g5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e6 6 f4 Nc6 7 Be3 Be7 8 Qf3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 0-0 0-0 8 Nb3 Nc6 9 Kh1
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 Bc4 12 Nxa5 Bxe2 13 Qxe2 Qxa5 14 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Qc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 0-0 0-0 9 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be2 Bg7 7 Be3 Nc6 8 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 f4 Qb6 10 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 (Bc4 12 Bd3 Bxd3 13 cxd3 d5)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Na5 11 f5 Bc4 12 Nxa5 Bxe2 13 Qxe2 Qxa5 14 g4
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Nb3 Be6 10 f4 Qc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 0-0 0-0 9 Qd2
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 Nb3
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 Be2 Nc6 8 Qd2 0-0 9 0-0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 0-0-0
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 a5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Bd7 10 0-0-0 Qa5 11 Bb3 Rfc8 12 h4 h5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Bd7 10 0-0-0 Rc8
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Nd7
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 Bc4 Nxd4 10 Bxd4 Be6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 0-0 8 Qd2 Nc6 9 g4 d5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 g6 6 Be3 Bg7 7 f3 Nc6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bc4 g6 7 Nxc6 bxc6 8 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Be2 Nxd4 7 Qxd4 g6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 a6 8 0-0-0 Bd7 9 f4 Be7 10 Nf3 b5 11 Bxf6
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 e5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 Nxd4 (10 Qxd4)
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 Nxd4 9 Qxd4 a6 10 f4 b5
  1 e4 c5 2 Nf3 d6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 Nc6 6 Bg5 e6 7 Qd2 Qb6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Bd3 Nf6 6 0-0 d6 7 c4 g6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 c4 Nf6 6 Nc3 Bb4 7 Bd3 Nc6 (8 Bc2)
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 a6 5 Nc3 b5 6 Bd3 Qb6 7 Be3 Bc5 8 Qg4
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nb5 d6 6 c4 Nf6 7 N1c3 a6 8 Na3 Be7 9 Be2 0-0 10 0-0 b6
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nb5 d6 6 c4 Nf6 7 N1c3 a6 8 Na3 d5
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Be3 a6 7 Be2 b5 8 Nxc6 Qxc6 9 a3 Bb7 10 Qd4
  1 e4 c5 2 Nf3 e6 3 d4 cxd4 4 Nxd4 Nc6 5 Nc3 Qc7 6 Ndb5 Qb8 7 Be3 a6 8 Bb6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Bg7 6 Be3 Nf6 7 Nc3 Ng4
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Bg7 6 Nc2 d6 7 Be2 Nh6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 g6 5 c4 Nf6 6 Nc3 Nxd4 7 Qxd4 d6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 a6 8 0-0-0 Bd7 9 f4 Be7 10 Nf3 b5 11 Bxf6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 e5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 Be7 8 0-0-0 0-0 9 f4 Nxd4 10 Qxd4
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 d6 6 Bg5 e6 7 Qd2 h6
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 (9 Nd5)
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 9 Bxf6 gxf6 10 Nd5 Bg7
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 b5 9 Bxf6 gxf6 10 Nd5 f5 11 Bxb5
  1 e4 c5 2 Nf3 Nc6 3 d4 cxd4 4 Nxd4 Nf6 5 Nc3 e5 6 Ndb5 d6 7 Bg5 a6 8 Na3 Be6
  1 e4 c6 2 d3 d5 3 Nd2 g6 4 g3 Bg7 5 Bg2 e5 6 Ngf3 Ne7 7 0-0 0-0 8 b4
  1 e4 c6 2 d4 d5 3 exd5 cxd5 4 c4 Nf6 5 Nc3 Nc6 6 Bg5 dxc4 7 d5 Na5
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 h4 h6 7 Nf3 Nd7 8 h5 Bh7 9 Bd3 Bxd3 10 Qxd3 (e6 11 Bd2 Ngf6 12 0-0-0 Bd6)
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Bf5 5 Ng3 Bg6 6 h4 h6 7 Nf3 Nd7 8 h5 Bh7 9 Bd3 Bxd3 10 Qxd3 e6 11 Bd2 Ngf6 12 0-0-0 Be7
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Bc4 Ngf6 6 Ng5 e6 7 Qe2 Nb6 8 Bb3
  1 e4 c6 2 d4 d5 3 Nc3 dxe4 4 Nxe4 Nd7 5 Ng5 Ngf6 6 Bd3 e6 7 N1f3 Bd6 8 Qe2 h6 9 Ne4 Nxe4 10 Qxe4
  1 e4 c6 2 Nc3 d5 3 Nf3 Bg4 4 h3 Bxf3 5 Qxf3 e6 6 d4 dxe4 7 Nxe4 Qxd4 8 Bd3
  1 e4 d6 2 d4 Nf6 3 Nc3 Nbd7 4 f4 e5 5 Nf3 exd4 6 Qxd4 c6 7 Bc4 d5
  1 e4 e5 2 Bc4 Bc5 3 b4 Bxb4 4 f4 exf4 5 Nf3 Be7 6 d4 Bh4+ 7 g3 fxg3 8 0-0 gxh2+ 9 Kh1
  1 e4 e5 2 Bc4 Bc5 3 c3 Nf6 4 d4 exd4 5 e5 d5 6 exf6 dxc4 7 Qh5 0-0
  1 e4 e5 2 Bc4 c6 3 d4 d5 4 exd5 cxd5 5 Bb5+ Bd7 6 Bxd7+ Nxd7 7 dxe5 Nxe5 8 Ne2
  1 e4 e5 2 d4 exd4 3 Qxd4 Nc6 4 Qe3 Nf6 5 Nc3 Bb4 6 Bd2 0-0 7 0-0-0 Re8 8 Bc4 d6 9 Nh3
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 b6 6 Nb5 Ba6 7 a4 g5 8 Nf3 Qh5 9 Ke1 Kd8 10 g4
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 d4 Qh4+ 5 Ke2 d5 6 exd5 Qe7+ 7 Kf2 Qh4+ 8 g3 fxg3+ 9 hxg3
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 Bc4 g4 6 0-0 gxf3 7 Qxf3 Ne5 8 Qxf4 Qf6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 d4 g4 6 Bc4 gxf3 7 0-0 d5 8 exd5 Bg4 9 dxc6
  1 e4 e5 2 Nc3 Nc6 3 f4 exf4 4 Nf3 g5 5 h4 g4 6 Ng5 h6 7 Nxf7 Kxf7 8 d4
  1 e4 e5 2 Nc3 Nf6 3 Bc4 Nxe4 4 Qh5 Nd6 5 Bb3 Nc6 6 Nb5 g6 7 Qf3 f5 8 Qd5 Qe7 9 Nxc7+ Kd8 10 Nxa8 b6
  1 e4 e5 2 Nc3 Nf6 3 f4 d5 4 fxe5 Nxe4 5 d3 Qh4+ 6 g3 Nxg3 7 Nf3 Qh5 8 Nxd5 Bg4 9 Nf4 Bxf3 10 Nxh5 Bxd1 11 hxg3 Bxc2 12 b3
  1 e4 e5 2 Nf3 d6 3 d4 exd4 4 Nxd4 Nf6 5 Nc3 Be7 6 Be2 0-0 7 0-0 c5 8 Nf3 Nc6 9 Bg5 Be6 10 Re1
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 Ng5 Nh6 6 f4 Be7 7 0-0 0-0 8 c3 d5
  1 e4 e5 2 Nf3 d6 3 d4 Nd7 4 Bc4 c6 5 Ng5 Nh6 6 f4 Be7 7 c3 0-0 8 0-0 d5
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 exd4 5 Nxd4 Be7 6 Be2 0-0 7 0-0 c5 8 Nf3 Nc6 9 Bg5 Be6 10 Re1
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 0-0 0-0 7 Qe2 c6 8 a4 exd4
  1 e4 e5 2 Nf3 d6 3 d4 Nf6 4 Nc3 Nbd7 5 Bc4 Be7 6 Ng5 0-0 7 Bxf7+ Rxf7 8 Ne6
  1 e4 e5 2 Nf3 f5 3 Bc4 fxe4 4 Nxe5 Qg5 5 Nf7 Qxg2 6 Rf1 d5 7 Nxh8 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 d4 b5 6 Bb3 exd4 7 Nxd4 Nxd4 8 Qxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 d6 5 d4 b5 6 Bb3 Nxd4 7 Nxd4 exd4 8 Qxd4 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 b5 6 Bb3 d6 7 Ng5 d5 8 exd5 Nd4 9 Re1 Bc5 10 Rxe5+ Kf8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 d4 b5 7 Bb3 d6 8 c3 Bg4 9 h3 Bxf3 10 Qxf3 exd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 d4 exd4 7 e5 Ne4 8 c3 dxc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Qe2 b5 7 Bb3 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Qe2 b5 7 Bb3 d6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 e4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 c6 12 Bxd5 cxd5 13 d4 Bd6 14 Re3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 c6 12 d4 Bd6 13 Re1 Qh4 14 g3 Qh3 15 Be3 Bg4 16 Qd3 Rae8 17 Nd2 Re6 18 a4 Qh5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d5 9 exd5 Nxd5 10 Nxe5 Nxe5 11 Rxe5 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 Bc2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 d4 Bg4 10 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 a5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Be6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 h6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Bd7 13 Nf1 Rfe8 14 Ne3 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 cxd4 13 cxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Nc6 13 dxc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Na5 10 Bc2 c6 11 d4 Qc7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d4 Nbd7 11 Nbd2 Bb7 12 Bc2 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nb8 10 d4 Nbd7 11 Nh4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 0-0 8 c3 d6 9 h3 Re8 10 d4 Bb7 11 Nbd2 Bf8 12 a3 h6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 Bc2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 a5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Bb7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Be6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 h6 10 d4 Re8 11 Nbd2 Bf8 12 a3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Bb7 12 Nbd2 cxd4 13 cxd4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Nc6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 Bd7 13 Nf1 Rfe8 14 Ne3 g6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c5 11 d4 Qc7 12 Nbd2 cxd4 13 cxd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Na5 10 Bc2 c6 11 d4 Qc7 12 Nbd2 Nc4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d4 Nbd7 11 Nbd2 Bb7 12 Bc2 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nb8 10 d4 Nbd7 11 Nh4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Nd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 0-0 9 h3 Qd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 a4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 h3 Nc6 12 d5 Nb8 13 Nbd2 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 h3 Nc6 12 d5 Nd8 13 Nbd2 g5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 c3 Na5 9 Bc2 c5 10 d4 Qc7 11 Nbd2 0-0 12 Nf1 Bg4 13 Ne3 Bxf3 14 Qxf3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 b5 7 Bb3 d6 8 d4 Nxd4 9 Nxd4 exd4 10 Qxd4 c5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 d6 7 c3 0-0 8 d4 Bd7 9 Nbd2 Be8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Be7 6 Re1 d6 7 c3 Bd7 8 d4 0-0 9 Nbd2 Be8
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 d6 6 Bxc6+ bxc6 7 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 d6 6 Bxc6+ bxc6 7 d4 Nxe4 8 Re1 f5 9 dxe5 d5 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 a4 Nxd4 9 Nxd4 exd4 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Bc5 10 Nbd2 0-0 11 Bc2 Nxf2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Bc5 10 Qd3 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 a4 b4 11 Nd4 Qd7 12 Nxe6 fxe6
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Nbd2 0-0 11 Qe2
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Re1 0-0 11 Nd4 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Be7 10 Re1 0-0 11 Nd4 Qd7 12 Nxe6 fxe6 13 Rxe4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 c3 Nc5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Nbd2 Bc5 10 Qe1
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Nbd2 Nc5 10 c3 d4 11 Ng5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Qe2 Be7 10 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Be6 9 Qe2 Be7 10 Rd1 0-0 11 c4 bxc4 12 Bxc4 Qd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 b5 7 Bb3 d5 8 dxe5 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 0-0 Nxe4 6 d4 exd4 7 Re1 d5 8 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Nc3 Bc5 6 Nxe5 Nxe5 7 d4 Bd6 8 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Qe2 b5 6 Bb3 Be7 7 c3 d6 8 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Ba4 Nf6 5 Qe2 b5 6 Bb3 Be7 7 d4 d6 8 c3 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 d4 exd4 6 Qxd4 Qxd4 (7 Nxd4 Bd6)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 a6 4 Bxc6 dxc6 5 d4 exd4 6 Qxd4 Qxd4 (7 Nxd4 Bd7)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 d5 6 Nxe5 dxe4 7 Nxc6 Qd5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 f5 4 Nc3 fxe4 5 Nxe4 d5 6 Nxe5 dxe4 7 Nxc6 Qg5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 d6 5 d4 Bd7 6 Nc3 Be7 7 Re1 0-0
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 b3
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 c4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 (6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nc3 0-0 10 Re1 Nc5 11 Nd4 Ne6 12 Be3 Nxd4 13 Bxd4 c5)
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nc3 0-0 10 Re1 Re8 11 Qc4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nb7 9 Nd4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Be7 6 Qe2 Nd6 7 Bxc6 bxc6 8 dxe5 Nf5
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 6 Bxc6 dxc6 7 dxe5 Ne4
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nf6 4 0-0 Nxe4 5 d4 Nd6 6 Bxc6 dxc6 7 dxe5 Nf5 8 Qxd8+ Kxd8 9 Nc3 Bd7
  1 e4 e5 2 Nf3 Nc6 3 Bb5 Nge7 4 d4 exd4 5 Nxd4 g6 6 Nc3 Bg7 7 Be3 0-0 8 Qd2 d5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Nh6 7 d4 d6 8 Bxh6 dxe5 9 Bxg7 Rg8 10 Bxf7+ Kxf7 11 Bxe5 Qg5 12 Nd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Qg5 7 Bxf7+ Ke7 8 Qh5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bb6 5 b5 Na5 6 Nxe5 Qg5 7 Qf3 Qxe5 8 Qxf7+ Kd8 9 Bb2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bd7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 d6 7 d4 exd4 8 Qb3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 0-0 Nf6 7 d4 0-0 8 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 d6 7 Qb3 Qd7 8 dxe5 dxe5 9 0-0 Bb6 10 Ba3 Na5 11 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 b5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 d3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 d6 (8 Qb3)
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 dxc3 8 Qb3 Qf6 9 e5 Qg6 10 Nxc3 Nge7 11 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 dxc3 8 Qb3 Qf6 9 e5 Qg6 10 Nxc3 Nge7 11 Rd1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 Nf6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Ba5 6 d4 exd4 7 0-0 Nge7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 0-0 d6 7 d4 exd4 8 cxd4 Bb6 9 Nc3 Bg4 10 Qa4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 d5 Na5 10 Bb2 Ne7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 Nc3 Bg4 10 Qa4 Bd7 11 Qb3 Na5 12 Bxf7+ Kf8 13 Qc2 Kxf7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 0-0 d6 8 cxd4 Bb6 9 Nc3 Na5 10 Bg5 f6 11 Be3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 cxd4 Bb4+ 8 Bd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 b4 Bxb4 5 c3 Bc5 6 d4 exd4 7 cxd4 Bb4+ 8 Kf1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d3 a6 6 Bb3 Ba7 7 h3 0-0 8 0-0 d6 9 Re1
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Bd2 Nxe4 8 Bxb4 Nxb4 9 Bxf7+ Kxf7 10 Qb3+ d5 11 Ne5+ Kf6 12 f3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 bxc3 d5 10 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 d5 Bf6 10 Re1 Ne7 11 Rxe4 d6 12 Bg5 Bxg5 13 Nxg5 0-0 14 Nxh7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Bxc3 9 d5 Bf6 10 Re1 Ne7 11 Rxe4 d6 12 g4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Nxc3 9 bxc3 Bxc3 10 Ba3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 cxd4 Bb4+ 7 Nc3 Nxe4 8 0-0 Nxc3 9 bxc3 Bxc3 10 Qb3 d5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 e5 d5 7 Bb5 Ne4 8 cxd4 Bb4+
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Nf6 5 d4 exd4 6 e5 Ne4 7 Bd5 Nxf2 8 Kxf2 dxc3+ 9 Kg3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 0-0 Nf6 7 a4 a6 8 Re1 d6 9 h3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 c3 Qe7 5 d4 Bb6 6 d5 Nf6 7 a4 a6 8 0-0 d6 9 h3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Bc5 4 Nc3 Nf6 5 d3 d6 6 Bg5 h6 7 Bxf6 Qxf6 8 Nd5 Qd8 9 c3 Ne7 10 d4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d3 d6 6 Bg5 h6 7 Bh4 g5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d4 Bxd4 6 Nxd4 Nxd4 7 Bg5 d6 8 f4 Qe7 9 fxe5 dxe5 10 Nc3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 0-0 Bc5 5 d4 Bxd4 6 Nxd4 Nxd4 7 Bg5 h6 8 Bh4 g5 9 f4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 fxg7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 g6
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 Qd5 10 Nc3 Qf5 11 g4 Qg6 12 Nce4 Bb6 (13 f4 0-0-0)
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Bc5 6 e5 d5 7 exf6 dxc4 8 Re1+ Be6 9 Ng5 Qd5 10 Nc3 Qf5 11 Nce4 Bf8
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Re1 d5 7 Bxd5 Qxd5 8 Nc3 Qa5 9 Nxe4 Be6 10 Bd2 Qd5 11 Bg5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 0-0 Nxe4 6 Re1 d5 7 Bxd5 Qxd5 8 Nc3 Qa5 9 Nxe4 Be6 10 Bg5 h6 11 Bh4 g5 12 Nf6+ Ke7 13 b4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 d4 exd4 5 e5 d5 6 Bb5 Ne4 7 Nxd4 Bc5 8 Nxc6 Bxf2+ 9 Kf1 Qh4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nf3 e4 10 Ne5 Bd6 11 d4 Qc7 12 Bd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nf3 e4 10 Ne5 Qc7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Be2 h6 9 Nh3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 cxb5
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 Qc7 9 Bd3
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 Bb5+ c6 7 dxc6 bxc6 8 Qf3 Rb8
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 d3 h6 7 Nf3 e4 8 Qe2 Nxc4 9 dxc4 Bc5 10 Nfd2
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Na5 6 d3 h6 7 Nf3 e4 8 Qe2 Nxc4 9 dxc4 Be7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nd4 6 c3 b5 7 Bf1 Nxd5 8 Ne4
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nxd5 6 Nxf7 Kxf7 7 Qf3+ Ke6 8 Nc3 Ncb4 9 Qe4 c6 10 a3 Na6 11 d4 Nac7
  1 e4 e5 2 Nf3 Nc6 3 Bc4 Nf6 4 Ng5 d5 5 exd5 Nxd5 6 Nxf7 Kxf7 7 Qf3+ Ke6 8 Nc3 Nce7
  1 e4 e5 2 Nf3 Nc6 3 c3 d5 4 Qa4 Bd7 5 exd5 Nd4 6 Qd1 Nxf3+ 7 Qxf3 f5 8 Bc4 Bd6 9 d3
  1 e4 e5 2 Nf3 Nc6 3 c3 f5 4 d4 d6 5 d5 fxe4 6 Ng5 Nb8 7 Nxe4 Nf6 8 Bd3 Be7
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bb4+ 5 c3 dxc3 6 0-0 cxb2 7 Bxb2 Nf6 8 Ng5 0-0 9 e5 Nxe5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 Ng5 Nh6 6 Nxf7 Nxf7 7 Bxf7+ Kxf7 8 Qh5+ g6 9 Qxc5 d5
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Bc4 Bc5 5 Ng5 Nh6 6 Nxf7 Nxf7 7 Bxf7+ Kxf7 8 Qh5+ g6 9 Qxc5 d6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Nge7 7 Bb5 Nd8
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Bc5 5 Be3 Qf6 6 c3 Nge7 7 Qd2 d5 8 Nb5 Bxe3 9 Qxe3 0-0 10 Nxc7 Rb8 11 Nxd5 Nxd5 12 exd5 Nb4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qf4 10 a4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qf4 10 c4
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Nxd2 Qg6
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Bd2 Qxe4+ 7 Be2 Kd8 8 0-0 Bxd2 9 Qxd2
  1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 Nxd4 Qh4 5 Nb5 Bb4+ 6 Nd2 Qxe4+ 7 Be2 Qxg2 8 Bf3 Qh3 9 Nxc7+ Kd8 10 Nxa8 Nf6 11 a3
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 a6 5 Bxc6 dxc6 6 Nxe5 Nxe4 7 Nxe4 Qd4 8 0-0 Qxe5 9 Re1 Be6 10 d4 Qd5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Bxc3 7 bxc3 d5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Bxc3 7 bxc3 d6 8 Re1
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Be6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bg4 8 Nd5 Nd4 9 Nxb4 Nxb5 10 Nd5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bxc3 8 bxc3 h6 9 Bh4 g5 10 Nxg5 Nxe4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Bxc3 8 bxc3 Qe7 9 Re1 Nd8 10 d4 Bg4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 d6 7 Bg5 Ne7 8 Nh4 c6 9 Bc4 d5 10 Bb3 Qd6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 d3 Qe7 7 Ne2 d5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bb4 5 0-0 0-0 6 Nd5 Nxd5 7 exd5 e4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Bc5 5 0-0 0-0 6 Nxe5 Nxe5 7 d4 Bd6 8 f4 Nc6 9 e5 Bb4
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Bb5 Nd4 5 Be2 Nxf3+ 6 Bxf3 Bc5 7 0-0 0-0 8 d3 d6 9 Na4 Bb6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Bd3 d5 8 exd5 cxd5 9 0-0 0-0 10 Bg5 Be6
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Bd3 d5 8 exd5 cxd5 9 0-0 0-0 10 Bg5 c6 11 Qf3 Be7 12 Rae1
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 d4 exd4 5 Nxd4 Bb4 6 Nxc6 bxc6 7 Qd4 Qe7 8 f3 c5
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Nxe5 Nxe5 5 d4 Nc6 (6 d5 Ne5 7 f4 Ng6 8 e5 Ng8 9 d6 cxd6 10 exd6 Qf6 11 Nb5 Rb8)
  1 e4 e5 2 Nf3 Nc6 3 Nc3 Nf6 4 Nxe5 Nxe5 5 d4 Ng6 6 e5 Ng8 7 Bc4 Bb4 8 Qf3 f5
  1 e4 e5 2 Nf3 Nf6 3 d4 Nxe4 4 Bd3 d5 5 Nxe5 Bd6 6 0-0 0-0 7 c4 Bxe5
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Bd6 7 0-0 0-0 8 c4 Bg4 9 cxd5 f5 10 Re1 Bxh2+
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Bd6 7 0-0 0-0 8 c4 c6 9 Re1 Bg4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 0-0
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 c4 Nb4 9 Be2
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 c4 Nb4 9 cxd5
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c3 f5 10 c4 Bh4
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c3 f5 (10 Nbd2)
  1 e4 e5 2 Nf3 Nf6 3 Nxe5 d6 4 Nf3 Nxe4 5 d4 d5 6 Bd3 Be7 7 0-0 Nc6 8 Re1 Bg4 9 c4
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 Qg4 Qc7 8 Qxg7 Rg8 9 Qxh7 cxd4 10 Kd1
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 e5 c5 5 a3 Bxc3+ 6 bxc3 Ne7 7 Qg4 Qc7 8 Qxg7 Rg8 9 Qxh7 cxd4 10 Ne2
  1 e4 e6 2 d4 d5 3 Nc3 Bb4 4 Nge2 dxe4 5 a3 Be7 6 Nxe4 Nf6 7 N2g3 0-0 8 Be2 Nc6
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 Bd2 Bxc3 7 bxc3 Ne4 8 Qg4 Kf8 9 Bc1
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 e5 h6 6 exf6 hxg5 7 fxg7 Rg8 8 h4 gxh4 9 Qg4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Bb4 5 exd5 Qxd5 6 Bxf6 gxf6 7 Qd2 Qa5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 f4 0-0 8 Nf3 c5 9 Qd2 Nc6 10 0-0-0 c4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 Bxe7 Qxe7 7 Qd2 0-0 8 f4 c5 9 Nf3 Nc6 10 0-0-0 c4
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 Be7 5 e5 Nfd7 6 h4 Bxg5 7 hxg5 Qxg5
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 Bg5 dxe4 5 Nxe4 Be7 6 Bxf6 Bxf6 7 Nf3 0-0
  1 e4 e6 2 d4 d5 3 Nc3 Nf6 4 e5 Nfd7 5 f4 c5 6 dxc5 Nc6 7 a3 Bxc5 8 Qg4 0-0 9 Nf3 f6
  1 e4 e6 2 d4 d5 3 Nd2 c5 4 exd5 Qxd5 5 Ngf3 cxd4 6 Bc4 Qd6 7 0-0 Nf6 8 Nb3 Nc6 9 Nbxd4 Nxd4 10 Nxd4 a6
  1 e4 e6 2 d4 d5 3 Nd2 Nf6 4 e5 Nfd7 5 Bd3 c5 6 c3 Nc6 7 Ne2 cxd4 8 cxd4 Nb6
  1 e4 e6 2 d4 d5 3 Nd2 Nf6 4 e5 Nfd7 5 f4 c5 6 c3 Nc6 7 Ndf3 cxd4 8 cxd4 Nb6
  1 e4 Nf6 2 e5 Nd5 3 c4 Nb6 4 c5 Nd5 5 Bc4 e6 6 Nc3 d6 7 Nxd5 exd5 8 Bxd5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 exd6 cxd6 6 Nf3 g6 7 Be2 Bg7 8 0-0 0-0 9 h3 Nc6 10 Nc3 Bf5 11 Bf4
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Bf5 7 Nc3 e6 8 Nf3 Bb4 9 Bd3
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Bf5 7 Nc3 e6 8 Nf3 Be7 9 Be2 0-0 10 0-0 f6
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Nc6 7 Be3 Bf5 8 Nc3 e6 9 Nf3 Qd7 10 Be2 0-0-0 11 0-0 Be7
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 c4 Nb6 5 f4 dxe5 6 fxe5 Nc6 7 Nf3 Bg4 8 e6 fxe6 9 c5
  1 e4 Nf6 2 e5 Nd5 3 d4 d6 4 Nf3 g6 5 c4 Nb6 6 exd6 cxd6 7 h3 Bg7 8 Be2 0-0 9 Nc3 Nc6 10 0-0 Bf5 11 Bf4
  1 f3 c5 2 Kf2 Nc6 3 Ke3 e6 4 c3 Be7 5 Kd3 Nf6 6 Kc2 0-0 7 Qe1 Nd5 8 Kd1
  1 Nf3 d5 2 g3 c5 3 Bg2 Nc6 4 0-0 e6 5 d3 Nf6 6 Nbd2 Be7 7 e4 0-0 (8 Re1)"

(*Sea-cadet mate*)
let piege_blanc = 
  "1 e4 e5 2 Nf3 Nc6 3 d4 exd4 4 c3 dxc3 5 Nxc3 d6 6 Bc4 Bg4 7 0-0 Ne5 8 Nxe5 Bxd1 9 Bxf7+ Ke7 10 Nd5#"

(*Fonction convertissant un répertoire d'ouvertures en une list de list de coups notés avec le type Mouvement*)
let translation algebraic =
  let line_break_detection = Str.split (Str.regexp "\n") algebraic
  in let rec func list = match list with
    |[] -> []
    |h :: t -> move_list_of_san h true Null (true, true, true, true) chessboard :: func t
  in func line_break_detection

(*Toutes les ouvertures*)
let ouvertures_exhaustif = translation chess_openings_exhaustif

(*Ouvertures suffisament longues*)
let ouvertures_efficaces = translation chess_openings_efficace

let rec select liste n = match liste with
  |[] -> []
  |h::t -> if n = 0 then [] else h :: select t (n - 1)

(*Echantillon d'ouvertures*)
let ouvertures_echantillon = select (ouvertures_exhaustif) 10