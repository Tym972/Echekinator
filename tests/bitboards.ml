open Libs.Bitboards
open Libs.Board
open Libs.Translation

(*Array used in print_board*)
let tab_print = [|"   |"; " P |"; " N |"; " B |"; " R |"; " Q |"; " K |"; " p |"; " n |"; " b |"; " r |"; " q |"; " k |"|]

let mailbox_of_bitboard pieces_bitboards =
  let mailbox = Array.make 64 0 in
  let rec aux index piece = match index with
    |[] -> ()
    |h::t ->
      mailbox.(h) <- piece;
      aux t piece
  in for i = 1 to 12 do
    aux (index_list pieces_bitboards.(i)) i
  done;
  mailbox

let coord = [|
  "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1";
  "a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2";
  "a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3";
  "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4";
  "a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5";
  "a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6";
  "a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7";
  "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8"
|]

let zouk mailbox st =
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  begin
    let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt"
    in output_string fichier_sortie (st ^ "\n" ^ !display ^ "     a   b   c   d   e   f   g   h\n");
    close_out fichier_sortie
  end

let print_board mailbox =
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")


let print_bitboard bitboard =
  let mailbox = Array.make 64 0 in
  let rec aux_1 mailbox index = match index with
    |[] -> ()
    |h::t ->
      mailbox.(h) <- 6;
      aux_1 mailbox t
  in aux_1 mailbox (index_list bitboard);
  let display = ref "   +---+---+---+---+---+---+---+---+\n"
  in for i = 8 downto 1 do
    let k_list = ref [] in
    let k = string_of_int i ^ "  |" in
    for j = 8 * (i - 1) to 8 * i - 1 do
      let piece = mailbox.(j) in
      k_list := tab_print.(piece) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    display := !display ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  begin
  let fichier_sortie = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Harry.txt"
  in output_string fichier_sortie (!display ^ "     a   b   c   d   e   f   g   h\n");
  close_out fichier_sortie
end;
  print_endline (!display ^ "     a   b   c   d   e   f   g   h\n")

let usa tab start stop =
  let b = ref 0L in
  for i = start to stop do
    b := Int64.logor !b tab.(i)
  done;
  !b

let _ = coord, zouk, print_bitboard, usa

let bitboard_of_mailbox mailbox =
  let bitboard = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|] in
  for i = 0 to 63 do
    let piece = mailbox.(i) in
    if piece > 0 then begin
      bitboard.(piece ) <- Int64.logor bitboard.(piece) (Int64.shift_left 1L i)
    end
  done;
  bitboard

let chessboard = [|
  4;  2; 3;  5;  6; 3; 2; 4;
  1;  1; 1;  1;  1; 1; 1; 1;
  0;  0; 0;  0;  0; 0; 0; 0;
  0;  0; 0;  0;  0; 0; 0; 0;
  0;  0; 0;  0;  0; 0; 0; 0;
  0;  0; 0;  0;  0; 0; 0; 0;
  7;  7; 7;  7;  7; 7; 7; 7;
  10; 8; 9; 11; 12; 9; 8; 10
  |]

let pieces = bitboard_of_mailbox chessboard

let occupancy =
  let o = [|0L; 0L|] in
  for i = 1 to 6 do
    o.(0) <- Int64.logor o.(0) pieces.(i);
  done;
  for i = 7 to 12 do
    o.(1) <- Int64.logor o.(1) pieces.(i)
  done;
  o
let depth = 6

let rec algoperft2 position depth =
  if depth = 0 then begin
    1
  end
  else begin
    legal_moves position;
    let nodes = ref 0 in
    let ply = position.ply in
    let moves = position.moves.(ply) in
    for i = 0 to position.number_of_moves.(ply) - 1 do
      let move = moves.(i) in
      make position move;
      let perft = (algoperft2 position (depth - 1)) in
      nodes := !nodes + perft;
      if position.ply = 1 then begin
        print_endline (uci_of_mouvement move ^ ": " ^ string_of_int perft)
      end;
      unmake position move;
    done;
    !nodes
  end

let () =
  begin
  let t = Sys.time () in
    print_board (mailbox_of_bitboard pieces);
    let nodes = algoperft2 {position with pieces = pieces; occupancy = occupancy; mailbox = mailbox_of_bitboard pieces} depth in
    let total_time = (Sys.time () -. t) in
    print_endline ("\nPerft " ^ (string_of_int depth));
    print_endline ("Total time (s) : " ^ (string_of_float total_time));
    print_endline ("Nodes searched : " ^ (string_of_int nodes));
    print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. total_time)))
  end