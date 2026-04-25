open Libs.Bitboards

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
    print_board position.pieces;
    let nodes = algoperft2 position depth in
    let total_time = (Sys.time () -. t) in
    print_endline ("\nPerft " ^ (string_of_int depth));
    print_endline ("Total time (s) : " ^ (string_of_float total_time));
    print_endline ("Nodes searched : " ^ (string_of_int nodes));
    print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. total_time)))
  end
