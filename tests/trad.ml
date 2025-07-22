open Config
open Libs.Board
open Libs.Generator
open Libs.Of_algebraic
open To_algebraic
open Libs.Fen
open Libs.Uci

let rec algoperft board white_to_move last_move castling_right depth =
  if depth = 0 then begin
    1
  end
  else begin
    let cp = ref (legal_moves board white_to_move last_move castling_right) in
    if ((List.map (fun c -> mouvement_of_uci c board !cp) (List.map (fun c -> uci_of_mouvement c) !cp)) <> !cp) then begin print_board board;
        List.iter (fun c -> print_string (algebraic_of_move c board [] ^ " ")) !cp; print_newline ();
        List.iter (fun c -> print_string (algebraic_of_move c board [] ^ " ")) (List.map (fun c -> mouvement_of_uci c board !cp) (List.map (fun c -> uci_of_mouvement c) !cp)); print_newline (); print_newline ();
        List.iter (fun l -> List.iter (fun c -> print_string (uci_of_mouvement c ^ " ")) l; print_newline ()) [!cp;  (List.map (fun c -> mouvement_of_uci c board !cp) (List.map (fun c -> uci_of_mouvement c) !cp))]; print_newline ()
      end;
    let nodes = ref 0 in
    while !cp <> [] do
      let coup = List.hd !cp in
      make board coup;
      cp := List.tl !cp;
      nodes := !nodes + (algoperft board (not white_to_move) coup (modification_roque coup castling_right) (depth - 1));
      dejoue board coup
    done;
    !nodes 
  end

let algoperftime board white_to_move historique castling_right depth =
  let t = Sys.time () in
  let fx = algoperft board white_to_move historique castling_right depth in
  fx, (Sys.time () -. t)

let perft depth board =
  let nodes, time = algoperftime board !white_to_move !last_move !castling_right depth in
  print_board board;
  print_endline (fen board !white_to_move !last_move !castling_right !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int depth));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft depth_perft board