open Libs.Board
open Libs.Move_ordering
open Libs.Quiescence
open Libs.Fen

let k = 1.

type training_date = {
  fen_string : string;
  score : float;
  result : float
}

let sigmoid s =
  1. /. (1. +. (Float.pow 10. (-.k *. s /. 400.)))

let format_result result = match result with
  |"1-0" -> 1.
  |"1/2-1/2" -> 0.5
  |_ -> 0.

let read_file filename =
  let ic = open_in filename in
  let data_list = ref [] in
  let continue = ref true in
  while !continue do
    try
      let line = input_line ic in begin
        match String.split_on_char '|' line with
        | [fen_string; score; result] -> data_list := {
            fen_string = String.trim fen_string;
            score = float_of_string (String.trim score);
            result = format_result (String.trim result)
          } :: !data_list
        | _ -> ()
      end;
    with End_of_file -> continue := false
  done;
  close_in ic;
  Array.of_list (List.rev !data_list)

let error filename =
  let position = create_position () in
  let search_tables = create_search_tables () in
  let sigma = ref 0. in
  let n = ref 0 in
  let data = read_file filename in
  for i = 0 to Array.length data - 1 do
    let pos = data.(i) in let _ = pos.score in
    incr n;
    position_of_fen pos.fen_string position;
    let qscore = float_of_int ((quiescence_search position search_tables 0 0 (- max_int) max_int true) * (- 2 * position.white_to_move + 1)) in
    sigma := !sigma +. Float.pow (pos.result -. (sigmoid qscore)) 2.
  done;
  !sigma /. (float_of_int !n)

let () =
  print_endline (string_of_float (error "Extracted.txt"))