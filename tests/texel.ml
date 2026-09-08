open Libs.Bitboards
open Libs.Board
open Libs.Move_ordering
open Libs.Quiescence
open Libs.Fen

let k = 1.

let sigmoid s =
  1. /. (1. +. (Float.pow 10. (-.k *. s /. 400.)))

let format_result result = match result with
  |"1-0" -> 1.
  |"1/2-1/2" -> 0.5
  |_ -> 0.

let read_line ic =
  let line = input_line ic in
  match String.split_on_char '|' line with
  | [fen_string; score; result] -> fen_string, int_of_string score, format_result result
  | _ -> invalid_arg ""

let error ic =
  let position = create_position () in
  let search_tables = create_search_tables () in
  let sigma = ref 0. in
  let n = ref 0 in
  let continue = ref true in
  while true do
    try
      incr n;
      let fen_string, _, result = read_line ic in
      position_of_fen fen_string position;
      let qscore = float_of_int (quiescence_search position search_tables 0 0 (- max_int) max_int true) /. 100. in
      sigma := !sigma +. Float.pow (result -. (sigmoid qscore)) 2.
    with _ -> continue := false
  done;
  !sigma /. (float_of_int !n)