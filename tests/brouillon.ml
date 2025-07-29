(*open Libs.Plateau
open Libs.Traduction1
open Libs.Generateur
open Config*)

let () = ()

open Libs.Board
open Libs.Evaluation

let f tb tn board =
  let material = ref 0 in
  let position = ref 0 in
  let white_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  let black_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i);
      white_pieces.(square) <- white_pieces.(square) + 1
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i);
      black_pieces.(- square) <- black_pieces.(- square) + 1
    end
  done;
  let only_white_king = white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
  in let only_black_king = black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0
  in let score_draw =
    let func () =
      let white_minor = white_pieces.(2) + white_pieces.(3) in
      let black_minor = black_pieces.(2) + black_pieces.(3) in
      white_minor < 3 && black_minor < 3 && begin
        (white_minor < 2 && black_minor < 2) || (*K vs K, K + Minor vs K + Minor*)
        ((white_pieces.(3) = 1 && (white_minor = 1 || black_minor > 0)) || (black_pieces.(3) = 1 && (black_minor = 1 || white_minor > 0))) || (*K + B + B vs K + B, K + B vs K + Minor, K + B vs K*)
        ((white_pieces.(2) = 2 && black_minor < 2) || black_pieces.(2) = 2 && white_minor < 2) (*K + N + N vs K + Minor, K + N + N vs K*)
      end
    in white_pieces.(1) = 0 && black_pieces.(1) = 0
      &&
      (only_white_king && only_black_king ||
      (white_pieces.(4) = 0 && black_pieces.(4) = 0 && white_pieces.(5) = 0 && black_pieces.(5) = 0 && func ()))
  in if score_draw then
    0, 0
  else
    !material, !position

let eval_materiel3 board (tb, tn) white_to_move =
  let material = ref 0 in
  let position = ref 0 in
  let white_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  let black_pieces = [|0; 0; 0; 0; 0; 0; 0|] in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square);
      position := !position + tb.(square - 1).(i);
      white_pieces.(square) <- white_pieces.(square) + 1
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square);
      position := !position - tn.(- square - 1).(i);
      black_pieces.(- square) <- black_pieces.(- square) + 1
    end
  done;
  (*let only_white_king () = white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
  in let only_black_king () = black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0
  in let score_draw =
    let func () =
      let white_minor = white_pieces.(2) + white_pieces.(3) in
      let black_minor = black_pieces.(2) + black_pieces.(3) in
      white_minor < 3 && black_minor < 3 && begin
        (white_minor < 2 && black_minor < 2) || (*K vs K, K + Minor vs K + Minor*)
        ((white_pieces.(3) = 1 && (white_minor = 1 || black_minor > 0)) || (black_pieces.(3) = 1 && (black_minor = 1 || white_minor > 0))) || (*K + B + B vs K + B*)
        ((white_pieces.(2) = 2 && black_minor < 2) || black_pieces.(2) = 2 && white_minor < 2) (*K + N + N vs K + Minor, K + N + N vs K*)
      end
    in white_pieces.(1) = 0 && black_pieces.(1) = 0
      &&
      (only_white_king && only_black_king ||
      (white_pieces.(4) = 0 && black_pieces.(4) = 0 && white_pieces.(5) = 0 && black_pieces.(5) = 0 && func ()))*)
  zugzwang :=
    if white_to_move then
      white_pieces.(2) = 0 && white_pieces.(3) = 0 && white_pieces.(4) = 0 && white_pieces.(5) = 0
    else
      black_pieces.(2) = 0 && black_pieces.(3) = 0 && black_pieces.(4) = 0 && black_pieces.(5) = 0;
  (*in let draw =
    if interior then

    else
      false*)
  (*if score_draw then
    0, 0
  else*)
    !material, !position
(*Draw Fide
- K vs K
- K + Minor vs K
- K + B vs K + B (de même couleur)

Draw (si les joueurs sont pas d'énormes abrutis)
- K + Minor vs K + Minor
- K + Minor vs K + N + N
- K + Minor vs K + B + N
- K vs K + N + N
- K + B vs K + B + B

Pas Nulle































if nb_pions > 0 || nb_tours > 0 || nb_dames > 0 then
  false  (* mat possible *)
else if nb_cavaliers > 1 || nb_fous > 1 then
  false  (* mat possible *)
else if nb_fous = 1 && nb_cavaliers = 1 then
  false  (* mat possible *)
else if nb_cavaliers = 1 then
  (* Roi + cavalier contre roi seul *)
  nb_fous = 0
else if nb_fous = 1 then
  (* Roi + fou contre roi seul *)
  nb_cavaliers = 0
else if nb_fous = 2 then
  (* Roi + fou contre roi + fou : vérifier couleur *)
  (* ex: fou_case_white, fou_case_black *)
  couleur_case fou_case_white = couleur_case fou_case_black
else
  true  (* roi contre roi *)*)


(*let coup = tolerance plateau "0-0-0" !trait_aux_blancs (coups_valides plateau !trait_aux_blancs !dernier_coup !droit_au_roque)
let bitboard = [|0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L; 0L|]
let main bitboard =
  affiche (mailbox_of_bitboard bitboard);
  joue plateau coup;
  affiche plateau;
  update_bitboard coup bitboard;
  affiche (mailbox_of_bitboard bitboard)

let () = main bitboard*)

(*

let tab_mvvlva = [|
  6; 12; 18; 24; 30;
  5; 11; 17; 23; 29;
  4; 10; 16; 22; 28;
  3;  9; 15; 21; 27;
  2;  8; 14; 20; 26;
  1;  7; 13; 19; 25
  |]

let bmvvlva2 liste =
  let rec association liste_coups = match liste_coups with
    |[] -> []
    |Classique {piece; depart; arrivee; prise} :: t when prise <> 0 ->
      (tab_mvvlva.(5 * (abs piece - 1) + (abs prise - 1)), Classique {piece; depart; arrivee; prise}) :: association t
    |Promotion {depart; arrivee; prise; promotion} :: t ->
      (tab_mvvlva.((if prise <> 0 then (abs prise - 1) else 0)) + tabvalue.(abs promotion), Promotion {depart; arrivee; prise; promotion}) :: association t
    |h :: t -> (0, h) :: association t
  in List.map snd (tri_fusion (association liste))



let main plateau =
  if false then begin
    let b = ref true in
    for i = 0 to 959 do
      fischer i plateau releve_plateau;
      if not (est_960 chaine_fen) then begin
        b := false;
        print_endline "Ce n'est pas une position 960";
        affiche plateau
      end
    done;
    print_endline (if !b then  "Fonction correcte" else "Les problèmes")
  end
  else if true then begin
    affiche plateau;
    print_endline (uci_of_san liste_coup !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart)
  end
  else begin
    affiche plateau;
    print_endline (san_of_uci (uci_of_san liste_coup !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart) !trait_aux_blancs_initial !dernier_coup_initial !droit_au_roque_initial position_de_depart)
  end*)


  (* fastchess   -openings order=random file=/home/tym972/openbench-books/UHO_Lichess_4852_v1.epd   -engine name=new cmd=/home/tym972/Echekinator/_build/install/default/bin/main_new   -engine name=base cmd=/home/tym972/Echekinator/_build/install/default/bin/main   -concurrency 4   -each tc=8+0.08 -rounds 4000 -repeat -recover   -sprt alpha=0.05 beta=0.10 elo0=0 elo1=10 -pgnout file=/home/tym972/Pgn_fastchess.pgn notation=san seldepth=true -pgnout notation=san file=/home/tym972/Echekinator/Résultats/Pgn_fastchess.pgn
      
      if hash_node_type = All && node_type <> All && hash_depth = profondeur && !best_score > hash_value then begin
        print_newline ();
        print_endline (Printf.sprintf "Défaillance All profondeur %i, alpha : %i, beta : %i" profondeur alpha beta);
        print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] []);
        print_endline (Printf.sprintf "best_score : %i et hash_value : %i" !best_score hash_value);
        if node_type = Pv then print_endline "Pv" else print_endline "Cut";
        (*affiche plateau*)
      end
      else if hash_node_type = Cut && node_type <> Cut && hash_depth = profondeur && !best_score < hash_value then begin
        print_newline ();
        print_endline (Printf.sprintf "Défaillance Cut profondeur %i, alpha ; %i, beta : %i" profondeur alpha beta);
        print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] []);
        print_endline (Printf.sprintf "best_score : %i et hash_value : %i" !best_score hash_value);
        if node_type = Pv then print_endline "Pv" else print_endline "All";
        (*affiche plateau*)
      end;
  
  affiche position_de_depart;
  print_endline (coord.(!depart_roi_blanc) ^ " " ^ coord.(!depart_tour_blanche_pr) ^ " " ^ coord.(!depart_tour_blanche_gr));
  print_endline (coord.(!depart_roi_noir) ^ " " ^ coord.(!depart_tour_noire_pr) ^ " " ^ coord.(!depart_tour_noire_gr));
  print_endline (string_of_bool !roi_blanc_clouable ^ " " ^ string_of_bool !roi_noir_clouable);
  print_endline (coord.(!clouage_roi_blanc_1) ^ " " ^ coord.(!clouage_roi_blanc_2) ^ " " ^ coord.(!clouage_roi_noir_1) ^ " " ^ coord.(!clouage_roi_noir_2));
  print_endline ((string_of_int !longueur_chemin_roi_blanc_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_blanc_gr));
  print_endline ((string_of_int !longueur_chemin_roi_noir_pr) ^ " " ^ (string_of_int !longueur_chemin_roi_noir_gr));
  print_endline (string_of_bool !tour_blanche_gr_en_a ^ " " ^ string_of_bool !tour_blanche_gr_en_b ^ " " ^ string_of_bool !tour_blanche_pr_en_h);
  print_endline (string_of_bool !tour_noire_gr_en_a ^ " " ^ string_of_bool !tour_noire_gr_en_b ^ " " ^ string_of_bool !tour_noire_pr_en_h);
  print_newline ();
  List.iter (fun tab -> Array.iter (fun c -> if c <> 0 then print_string (coord.(c) ^ " ")) tab; print_newline ()) [chemin_blanc_pr; chemin_blanc_gr; chemin_noir_pr; chemin_noir_gr]; print_newline ();
  List.iter (fun list -> List.iter (fun c -> print_string (coord.(c) ^ " ")) list; print_newline ()) [!vides_blanc_pr; !vides_blanc_gr; !vides_noir_pr; !vides_noir_gr]
*)

(*


let deplacements_all2 plateau trait_aux_blancs position_roi piece_clouees =
  let liste_coups = ref [] in
  let liste_coups_roi = deplacements_roi2 plateau position_roi in
  let liste_coups_clouees = ref [] in
  if trait_aux_blancs then begin
    let aux depart arrivee = 
    for i = depart downto arrivee do
      if not (List.mem i piece_clouees) then begin
        let piece = plateau.(i) in
        if piece > 0 then begin
          (tabfun.(piece - 1) plateau i liste_coups)
        end
      end
      else begin
        let piece = plateau.(i) in (tabfun.(piece - 1) plateau i liste_coups_clouees)
      end
    done in List.iter (fun (a, b) -> aux a b) [63, position_roi + 1; position_roi - 1, 0]
  end
  else begin
    let aux depart arrivee =
      for i = depart to arrivee do
        if not (List.mem i piece_clouees) then begin
          let piece = plateau.(i) in
          if piece < 0 then begin
            (tabfun.(- piece - 1) plateau i liste_coups)
          end
        end
        else begin
          let piece = plateau.(i) in (tabfun.(- piece - 1) plateau i liste_coups_clouees)
        end
      done
    in List.iter (fun (a, b) -> aux a b) [0, position_roi - 1; position_roi + 1, 63]
  end;
  !liste_coups, liste_coups_roi, !liste_coups_clouees


(*let menacee plateau case trait_aux_blancs =
  let b = ref false in
  let m = tab64.(case) in
  let signe_joueur = if trait_aux_blancs then 1 else (-1) in
  let vect_pion = [|(-9) * signe_joueur; (-11) * signe_joueur|] in
  let i = ref 0 in
  while (not !b && !i < 2) do
    let dir = vect_pion.(!i) in
    if tab120.(m + dir) <> (-1) then begin
      let candidat = tab120.(m + dir) in
      if plateau.(candidat) = (- signe_joueur) then begin
        b := true
      end
    end;
    incr i
  done;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 8) do
      let dir = vect_cavalier.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = (-2) * signe_joueur then begin
          b := true
        end
      end;
      incr i
    done
  end;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 4) do
      let dir = vect_fou.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(m + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(m + (!k * dir)) in
        let dest = plateau.(candidat) * signe_joueur in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin
          if dest = (-3) || dest = (-5) || (dest = (-6) && !k = 1) then begin
            b := true
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  if not !b then begin
    let i = ref 0 in
    while (not !b && !i < 4) do
      let dir = vect_tour.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(m + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(m + (!k * dir)) in
        let dest = plateau.(candidat) * signe_joueur in
        if dest = 0 then begin
          incr k
        end
        else if dest > 0 then begin
          s :=  false
        end
        else begin
          if dest = (-4) || dest = (-5) || (dest = (-6) && !k = 1) then begin
            b := true
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  !b*)


let rec algoperft plateau trait_aux_blancs dernier_coup droit_au_roque profondeur racine zobrist_position =
  if profondeur = 0 then begin
    1
  end
  else begin
    let nombre = try ZobristHashtbl.find table_perft zobrist_position with _ -> (-1) in
    if nombre <> (-1) then begin incr a;
      nombre
    end
    else begin
      let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque) in
      let nodes = ref 0 in
      while !cp <> [] do
        let coup = List.hd !cp in
        joue plateau coup;
        cp := List.tl !cp;
        let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
        let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque lxor profondeur in
        let perft = (algoperft plateau (not trait_aux_blancs) coup nouveau_droit_au_roque (profondeur - 1) false nouveau_zobrist) in
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

let negalphabeta_tri liste_coups plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation = incr compteur_recherche;
  if List.length liste_coups = 1 then begin
    List.hd liste_coups
  end
  else begin
    let best_score = ref (-99999) in
    let best_move = ref Aucun in
    (*if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
      best_score := 0
    end
    else*) if profondeur = 0 then begin incr compteur_noeuds_terminaux;
      best_score := traitement_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup alpha beta
    end
    else begin
      let cp = ref liste_coups
      in begin
        let b = ref true in
        let alpha0 = ref alpha in
        while (!b && !cp <> []) do
          let coup = List.hd !cp in
          joue plateau coup;
          cp := List.tl !cp;
          let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
          let nouveau_releve = adapte_releve plateau coup profondeur trait_aux_blancs nouveau_droit_au_roque releve_plateau
          in let score =
            let note, _ = negalphabeta plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- beta) (- !alpha0) evaluation
            in - note
          in if score > !best_score then begin
            best_score := score;
            best_move := coup;
            if score >= beta then begin
              b := false
            end
            else begin
              alpha0 := max !alpha0 score
            end
          end;
          dejoue plateau coup
        done
      end
    end;
    !best_move
  end

let tk = [|0; 0; 1; 1; 2; 2; 2; 2; 2; 2; 2;|]

let sort liste_coups plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation =
  if profondeur = 1 then begin
    List.hd liste_coups
  end
  else if profondeur = 2 then begin
    List.hd (mvvlva liste_coups)
  end
  else begin
    negalphabeta_tri liste_coups plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau tk.(profondeur - 1) tk.(profondeur - 1) (-99999) 99999 evaluation
  end

let rec negalphabeta_yalta plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur profondeur_initiale alpha beta evaluation zobrist_position = incr compteur_recherche;
  let best_score = ref (-99999) in
  let best_move = ref Aucun in
  let presence = ref true in
  let hash_node_type, hash_depth, hash_value, hash_move, _ = try ZobristHashtbl.find table zobrist_position with _ -> begin
    presence := false;
    (Pv, (-1), 0, Aucun, 0)
  end
  in if repetition releve_plateau 3 then begin incr compteur_noeuds_terminaux;
    best_score := 0
  end
  else begin
    let alpha0 = ref alpha in
    let beta0 = ref beta in
    let continuation = ref true in
    if !presence then begin
      traitement_hash hash_node_type hash_depth hash_value hash_move profondeur alpha0 beta0 best_score best_move continuation
    end;
    if !continuation then begin
      if profondeur = 0 then begin incr compteur_noeuds_terminaux;
        best_score := traitement_quiescent_profondeur_0 evaluation plateau trait_aux_blancs dernier_coup !alpha0 !beta0;
      end
      else begin
        let b = ref true in
        if (hash_node_type = Pv || (hash_node_type = Cut && hash_value > beta)) && hash_move <> Aucun then begin
          joue plateau hash_move;
          let nouveau_droit_au_roque = modification_roque hash_move droit_au_roque in
          let nouveau_zobrist = zobrist plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque in
          let nouveau_releve = adapte_releve2 nouveau_zobrist hash_move profondeur releve_plateau
          in let score =
            let note, _ = negalphabeta_yalta plateau (not trait_aux_blancs) hash_move nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
            in - note
          in if score > !best_score then begin
            best_score := score;
            best_move := hash_move;
            alpha0 := max !alpha0 score;
            if score >= !beta0 then begin
              b := false
            end
          end;
          dejoue plateau hash_move;
        end;
        if !b then begin
          let cp = ref (coups_valides plateau trait_aux_blancs dernier_coup droit_au_roque) in
          if !cp = [] then begin incr compteur_noeuds_terminaux;
            if (menacee plateau (index_tableau plateau (roi trait_aux_blancs)) trait_aux_blancs) then begin
              best_score := (profondeur_initiale - (profondeur + 99999))
            end 
            else begin
              best_score := 0
            end
          end
          else begin
            while (!b && !cp <> []) do
              let coup = sort !cp plateau trait_aux_blancs dernier_coup droit_au_roque releve_plateau profondeur evaluation in
              joue plateau coup;
              let nouveau_droit_au_roque = modification_roque coup droit_au_roque in
              let nouveau_zobrist = nouveau_zobrist coup dernier_coup zobrist_position droit_au_roque nouveau_droit_au_roque in
              let nouveau_releve = adapte_releve2 nouveau_zobrist coup profondeur releve_plateau
              in let score =
                let note, _ = negalphabeta_yalta plateau (not trait_aux_blancs) coup nouveau_droit_au_roque nouveau_releve (profondeur - 1) profondeur_initiale (- !beta0) (- !alpha0) evaluation nouveau_zobrist
                in - note 
              in if score > !best_score then begin
                best_score := score;
                best_move := coup;
                alpha0 := max !alpha0 score;
                if score >= !beta0 then begin
                  b := false
                end
              end;
              if !b then begin
                cp := filtre coup !cp
              end;
              dejoue plateau coup
            done
          end; 
          (*if !best_move <> Aucun then begin
            if !best_move = hash_move then tab_hash_best.(0) <- tab_hash_best.(0) + 1 else tab_hash_best.(1) <- tab_hash_best.(1) + 1;
            let j = index_liste !best_move copie_cp in tab_hash_2.(j) <- tab_hash_2.(j) + 1
          end;*)
        end;
      end
    end
  end;
  let node_type =
    if !best_score <= alpha then begin
      All
    end
    else if !best_score >= beta then begin
      Cut
    end
    else begin
      Pv
    end in
  (*if hash_node_type = Pv && !presence && hash_value <> !best_score && node_type = Pv && profondeur = hash_depth then begin
    print_endline (Printf.sprintf "hash_value : %i best_score : %i" hash_value !best_score);
    print_endline (Printf.sprintf "hash_depth : %i et profondeur : %i"  profondeur hash_depth);
    print_endline (Printf.sprintf "hash_best : %s et bestmove : %s" (algebric_of_mouvement hash_move plateau []) (algebric_of_mouvement !best_move plateau []));
    print_endline (Printf.sprintf " alpha : %i et beta : %i" alpha beta);
    print_endline (fen plateau trait_aux_blancs dernier_coup droit_au_roque [] releve_plateau);
    affiche plateau
  end;*)
  if !presence then begin
    if profondeur > hash_depth then begin
      ZobristHashtbl.replace table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
    end
  end
  else begin
    ZobristHashtbl.add table zobrist_position (node_type, profondeur, !best_score, !best_move, 0)
  end;
  !best_score, !best_move

let rec filtre elt liste = match liste with
  |[] -> []
  |h::t -> if h = elt then t else h :: (filtre elt t)
  
  let smaller_attaquer plateau case trait_aux_blancs =
  let coup = ref Aucun in
  let b = ref false in
  let m = tab64.(case) in
  let piece = plateau.(case) in
  if trait_aux_blancs then begin
    let vect_pion = [|(-9); (-11)|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = (-1) then begin
          let coup_potentiel = Classique {piece = -1; depart = candidat; arrivee = case; prise = piece} in
          if est_valide plateau coup_potentiel false then begin
            b := true;
            coup := coup_potentiel
          end
        end
      end;
      incr i
    done;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 8) do
        let dir = vect_cavalier.(!i) in
        if tab120.(m + dir) <> (-1) then begin
          let candidat = tab120.(m + dir) in
          if plateau.(candidat) = (-2) then begin
            let coup_potentiel = Classique {piece = -2; depart = candidat; arrivee = case; prise = piece} in
            if est_valide plateau coup_potentiel false then begin
              b := true;
              coup := coup_potentiel
            end
          end
        end;
        incr i
      done
    end;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 4) do
        let dir = vect_fou.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(m + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            s :=  false
          end
          else begin
            if dest = (-3) || dest = (-5) || (dest = (-6) && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel false then begin
                if dest = -3 then begin
                  b := true
                end;
                coup := coup_potentiel
              end
            end;
            s :=  false
          end
        done;
        incr i
      done
    end;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 4) do
        let dir = vect_tour.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(m + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest > 0 then begin
            s :=  false
          end
          else begin
            if dest = (-4) || dest = (-5) || (dest = (-6) && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel false then begin
                b := true;
                coup := coup_potentiel
              end
            end;
            s :=  false
          end
        done;
        incr i
      done
    end
  end
  else if piece < 0 then begin
    let vect_pion = [|9; 11|] in
    let i = ref 0 in
    while (not !b && !i < 2) do
      let dir = vect_pion.(!i) in
      if tab120.(m + dir) <> (-1) then begin
        let candidat = tab120.(m + dir) in
        if plateau.(candidat) = 1 then begin
          let coup_potentiel = Classique {piece = 1; depart = candidat; arrivee = case; prise = piece} in
          if est_valide plateau coup_potentiel true then begin
            b := true;
            coup := coup_potentiel
          end
        end
      end;
      incr i
    done;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 8) do
        let dir = vect_cavalier.(!i) in
        if tab120.(m + dir) <> (-1) then begin
          let candidat = tab120.(m + dir) in
          if plateau.(candidat) = 2 then begin
            let coup_potentiel = Classique {piece = 2; depart = candidat; arrivee = case; prise = piece} in
            if est_valide plateau coup_potentiel true then begin
              b := true;
              coup := coup_potentiel
            end
          end
        end;
        incr i
      done
    end;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 4) do
        let dir = vect_fou.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(m + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest < 0 then begin
            s :=  false
          end
          else begin
            if dest = 3 || dest = 5 || (dest = 6 && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel true then begin
                if dest = 3 then begin
                  b := true
                end;
                coup := coup_potentiel
              end
            end;
            s :=  false
          end
        done;
        incr i
      done
    end;
    if not !b then begin
      let i = ref 0 in
      while (not !b && !i < 4) do
        let dir = vect_tour.(!i) in
        let k = ref 1 in
        let s = ref true in
        while (tab120.(m + (!k * dir)) <> (-1) && !s) do
          let candidat = tab120.(m + (!k * dir)) in
          let dest = plateau.(candidat) in
          if dest = 0 then begin
            incr k
          end
          else if dest < 0 then begin
            s :=  false
          end
          else begin
            if dest = 4 || dest = 5 || (dest = 6 && !k = 1) then begin
              let coup_potentiel = Classique {piece = dest; depart = candidat; arrivee = case; prise = piece} in
              if est_valide plateau coup_potentiel true then begin
                b := true;
                coup := coup_potentiel
              end
            end;
            s :=  false
          end
        done;
        incr i
      done
    end
  end;
  !coup
  *)