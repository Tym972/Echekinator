# ♟️ Echekinator - UCI Chess Engine

[![Lichess Profile](https://img.shields.io/badge/Lichess-Echekinator-000000?style=flat&logo=lichess)](https://lichess.org/@/Echekinator) [![OCaml](https://img.shields.io/badge/Language-OCaml-EC6813?style=flat&logo=ocaml)](https://ocaml.org) [![License](https://img.shields.io/github/license/Tym972/Echekinator)](https://github.com/Tym972/Echekinator/blob/main/LICENSE)


Echekinator is an UCI-compatible chess engine written in OCaml.

## Features

- Move Generation
    - Legal generator with 3 modes (all moves, captures/promotions, quiets)
- HCE
    - PeSTO Piece-square tables 
    - Mobility
    - Pawn Evaluation
    - Rook activity
    - Bishop pair
    - Minimalist king safety
- Search
    - Negamax with alpha-beta pruning
    - Principle Variation Search (PVS)
    - Quiescence Search with stand-pat cutoffs and SEE filtering
    - Iterative Deepening
    - Adaptive Aspiration Windows
    - Transposition Table
    - Move Ordering
        - Transposition Table Move
        - MVV-LVA for captures and promotions
        - Differentiation of captures with Static Exchange Evaluation
        - Killer Move Heuristic (2 slots per ply)
        - Butterfly History Heuristic
    - Selectivity & Pruning
        - TT Cutoffs
        - Reverse Futility Pruning
        - Razoring
        - Null Move Pruning
        - Late Move Pruning
        - Futility Pruning
        - Late Move Reductions

## Build

```bash
git clone https://github.com/Tym972/Echekinator
cd Echekinator
opam install . --deps-only
dune build
```

The compiled engine binary will be located in:

```bash
Echekinator/_build/default/bin/echekinator.exe
```

To run the engine (raw UCI):

```bash
dune exec bin/echekinator.exe
```

## Ratings

|   Version       | CCRL 40/15 | CCRL Blitz | Architecture   |
|-----------------|------------|------------|----------------|
| Echekinator 1.0 | —          | 2143       | 0x88 + HCE     |
| Echekinator 1.1 | —          | —          | Bitboards + HCE|


## Credits
This project would not have been possible without the help of the Stockfish Discord server. 
Special thanks to:
- Disservin fom Fastchess
- Chess Programming Wiki
- CCRL for testing the engine