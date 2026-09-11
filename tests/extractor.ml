open Libs.Bitboards
open Libs.Board
open Libs.Fen
open Libs.Move_ordering
open Libs.Evaluation
open Libs.Quiescence
open Algebraic

let process_pgn_file filename engine1 engine2 =
  let ic = open_in filename in
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "Extracted.txt" in
  let moves_regexp = Str.regexp {|\([KQRBN]?[a-h]?[1-8]?x?[a-h][1-8]\(=[QRBN]\)?\|[a-h][1-8][a-h][1-8][qrbn]?\|O-O-O\|O-O\)|} in
  let eval_regexp = Str.regexp {|{\(\([+-]?[0-9]*\.[0-9]+\)\|\([+-]M[0-9]+\)\)/[0-9][0-9]*[^}]*}|} in
  let game_count = ref 0 in
  let position = create_position () in
  let search_tables = create_search_tables () in
  let rec read_games () =
    try
      let line = input_line ic in
      (* Détecte le début d'une partie *)
      if String.starts_with ~prefix:"[Event" line then begin
        process_game ic
      end;
      read_games ()
    with End_of_file -> close_in ic
  
  and process_game ic =
    let engine = ref 0 in
    let fen_string = ref "" in
    let result = ref "" in
    let moves = ref [] in
    let engine_evals = ref [] in
    
    (* Lire les métadonnées *)
    let rec read_headers () =
      try
        let line = input_line ic in

        (*Fen reading*)
        if String.starts_with ~prefix:"[FEN" line then begin
          fen_string := extract "FEN" line;
          read_headers ()
        end

        (*Result reading*)
        else if String.starts_with ~prefix:"[Result" line then begin
          result := extract "Result" line;
          read_headers ()
        end

        (*Engine1 plays white*)
        else if String.starts_with ~prefix: (Printf.sprintf "[White \"%s\"]" engine1) line then begin
          engine := !engine lor 1;
          read_headers ()
        end

        (*Engine1 plays white*)
        else if String.starts_with ~prefix: (Printf.sprintf "[Black \"%s\"]" engine1) line then begin
          engine := !engine lor 2;
          read_headers ()
        end

        (*Engine2 plays white*)
        else if String.starts_with ~prefix: (Printf.sprintf "[White \"%s\"]" engine2) line then begin
          engine := !engine lor 1;
          read_headers ()
        end

        (*Engine2 plays white*)
        else if String.starts_with ~prefix: (Printf.sprintf "[Black \"%s\"]" engine2) line then begin
          engine := !engine lor 2;
          read_headers ()
        end

        (*Beginning of the moves*)
        else if line = "" then begin
          read_moves ()
        end
        
        (*Continue reading headers*)
        else begin
          read_headers ()
        end
        
      with End_of_file -> ()
    
    and read_moves () =
      try
        let line = input_line ic in

        (*Moves reading*)
        if line <> "" then begin
          moves := !moves @ (extract_moves line);
          engine_evals := !engine_evals @ (extract_evals line);
          read_moves ()
        end
        
        (*Next game
        else begin
          read_games ()
        end*)

      with End_of_file -> close_in ic
    
    and extract data line =
      try
        let _ = Str.search_forward (Str.regexp (data ^ {| "\([^"]*\)"|})) line 0 in
        Str.matched_group 1 line
      with _ -> ""

    and extract_all regex line =
      let rec loop pos acc =
        try
          let _ = Str.search_forward regex line pos in
          let g = Str.matched_group 1 line in
          loop (Str.match_end ()) (g :: acc)
        with _ -> List.rev acc
      in loop 0 []

    and extract_moves line =
      extract_all moves_regexp line

    and extract_evals line =
      extract_all eval_regexp line

    and select_position engine moves engine_evals fen_string result =
      incr game_count;
      if !game_count mod 1000 = 0 then begin
        print_endline (Printf.sprintf "Game #%i" !game_count);
      end;
      position_of_fen fen_string position;
      let rec moves_loop moves engine_evals = match moves, engine_evals with
        |[], _ | _, [] -> ()
        |move :: other_moves, engine_eval :: other_engine_evals ->
          if (position.white_to_move + 1) land !engine <> 0 && isquiet_position position search_tables engine_eval then begin
            let entry = Printf.sprintf "%s | %i | %s\n" (fen position) (format_score engine_eval position.white_to_move) result in
            output_string oc entry
          end;
          make position (move_of_algebric position move); 
          moves_loop other_moves other_engine_evals;
      in moves_loop moves engine_evals
  
    and isquiet_position position search_tables engine_eval =
      not (position.state_array.(position.game_ply).in_check || String.contains engine_eval 'M') &&
      begin
        let static_eval = hce position in
        (abs (static_eval - (int_of_float ((float_of_string engine_eval) *. 100.)))) <= 70 && (abs (static_eval - quiescence_search position search_tables 0 0 (- max_int) max_int true)) <= 60
      end
    
    and format_score engine_eval white_to_move =
      (int_of_float ((float_of_string engine_eval) *. 100.)) * (- 2 * white_to_move + 1)

    in
    read_headers ();
    select_position engine !moves !engine_evals !fen_string !result

  in
  read_games ();
  close_out oc


let () = process_pgn_file "Pgn_fastchess.pgn" "new" "base"