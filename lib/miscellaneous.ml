(**)

open Bitboards

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

let flip square =
  let rank = square / 8 in
  let file = square mod 8 in
  (7 - rank) * 8 + file

(*Fonction décomposant une chain de caractère en list de substring correspondants aux mots*)
let word_detection chain =
  Str.split (Str.regexp " +") chain

(*Fonction vérifiant si une chain de caractère représente un entier*)
let is_integer_string chain =
  let i = try int_of_string chain with _ -> (-1) in
  i > 0