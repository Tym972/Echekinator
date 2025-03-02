open Libs.Plateau
open Libs.Generateur
open Libs.Zobrist
open Libs.Transposition
open Libs.Traduction2
open Libs.Traduction3
open Config

let table_perft = ZobristHashtbl.create 200000000

let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur racine zobrist_position =
  if profondeur = 0 then begin
    1
  end
  else begin
    let nombre = try ZobristHashtbl.find table_perft zobrist_position with _ -> (-1) in
    if nombre <> (-1) then begin
      nombre
    end
    else begin
      let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque) in
      let nodes = ref 0 in
      while !cp <> [] do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_doit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_zobrist = if profondeur > 1 then (nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_doit_au_roque lxor (profondeur - 1)) else 0 in
        let perft = (algoperft plateau (not trait_aux_blancs) coup nouveau_doit_au_roque (profondeur - 1) false nouveau_zobrist) in
        nodes := !nodes + perft;
        if racine then begin
          print_endline (uci_of_mouvement coup ^ ": " ^ string_of_int perft)
        end;
        dejoue plateau coup
      done;
      ZobristHashtbl.add table_perft zobrist_position !nodes;
      !nodes;
    end
  end

let algoperftime plateau trait_aux_blancs dernier_coup droit_au_roque profondeur =
  let t = Sys.time () in
  let fx = algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur true (zobrist plateau trait_aux_blancs dernier_coup droit_au_roque lxor profondeur) in
  fx, (Sys.time () -. t)

let perft profondeur plateau =
  let nodes, time = algoperftime plateau !trait_aux_blancs !dernier_coup !droit_au_roque profondeur in
  print_newline ();
  affiche plateau;
  print_endline (fen plateau !trait_aux_blancs !dernier_coup !droit_au_roque !releve_coups !releve_plateau);
  print_endline ("\nPerft " ^ (string_of_int profondeur));
  print_endline ("Total time (s) : " ^ (string_of_float time));
  print_endline ("Nodes searched : " ^ (string_of_int nodes));
  print_endline ("Nodes/seconde : " ^ (string_of_float ((float_of_int nodes)/. time)))
 

let () = perft profondeur_perft plateau