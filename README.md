# ♟️ Echekinator - UCI Chess Engine

[![Lichess Profile](https://img.shields.io/badge/Lichess-Echekinator-000000?style=flat&logo=lichess)](https://lichess.org/@/Echekinator) [![OCaml](https://img.shields.io/badge/Language-OCaml-EC6813?style=flat&logo=ocaml)](https://ocaml.org) [![License](https://img.shields.io/github/license/Tym972/Echekinator)](https://github.com/Tym972/Echekinator/blob/main/LICENSE)


Echekinator is a UCI-compatible chess engine written in OCaml.

## Overview
The engine currently relies on:
- Principal Variation Search (PVS)
- Quiescence Search
- Iterative Deepening
- Null Move Pruning
- Reverse Futility Pruning
- Late Move Reduction
- PeSTO's evaluation function
- Killer moves
- History heuristic
- Transposition Table
- UCI Protocol

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

## Playing strength
Echekinator will be tested on the CCRL lists.

- CCRL 40/15 rating: TBD
- CCRL Blitz rating: TBD


No official rating available yet.

## Credits
This project would not have been possible without the help of the Stockfish Discord server. 
Special thanks to:
- Fastchess for engine tournaments
- Chess Programming Wiki
- PeSTO for the evaluation
- Arthurus on YouTube for the inspiration