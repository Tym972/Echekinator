open Board
open Generator
open Evaluation

let smaller_attacker board square white_to_move =
  let move = ref Null in
  let smaller = ref (-20) in
  let smaller_possible = ref true in
  let tab64_square = tab64.(square) in
  let piece = board.(square) in
  let player_sign = if white_to_move then 1 else (-1) in
  let i = ref 0 in
  let bishop_iterations = Array.make 4 true in
  while (!smaller_possible && !i < 4) do
    let direction = bishop_vect.(!i) in
    let attacker_square = tab120.(tab64_square + direction) in
    if attacker_square <> (-1) then begin
      let attacker = board.(attacker_square) * player_sign in
      if attacker <> 0 then begin
        bishop_iterations.(!i) <-  false;
        if (attacker = (-1) && direction * player_sign < 0) then begin
          smaller_possible := false;
          move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
        end
        else if ((attacker <= (-3) && attacker <> (-4)) && attacker > !smaller) then begin
          smaller := attacker;
          move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece};
        end
      end
    end;
    incr i
  done;
  if !smaller_possible then begin
    let i = ref 0 in
    while (!smaller_possible && !i < 8) do
      let direction = knight_vect.(!i) in
      if tab120.(tab64_square + direction) <> (-1) then begin
        let attacker_square = tab120.(tab64_square + direction) in
        if board.(attacker_square) = (-2) * player_sign then begin
          smaller_possible := false;
          move := Normal {piece = (-2) * player_sign; from = attacker_square; to_ = square; capture = piece}
        end
      end;
      incr i
    done;
    if !smaller = (-3) then begin
      smaller_possible := false
    end
  end;
  i := 0;
  while (!smaller_possible && !i < 4) do 
    let direction = bishop_vect.(!i) in
    let iterate = ref (bishop_iterations.(!i) && tab120.(tab64_square + direction) <> (-1)) in
    let distance = ref 2 in
    while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
      let attacker_square = tab120.(tab64_square + (!distance * direction)) in
      let attacker = board.(attacker_square) * player_sign in
      if attacker = 0 then begin
        incr distance
      end
      else if attacker > 0 then begin
        iterate :=  false
      end
      else begin
        if attacker = (-3) then begin
          smaller_possible := false;
          move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
        end
        else if attacker = (-5) then begin
          smaller := attacker;
          move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
        end;
        iterate :=  false
      end
    done;
    incr i
  done;
  if !smaller_possible  then begin
    let i = ref 0 in
    while (!smaller_possible && !i < 4) do
      let direction = rook_vect.(!i) in
      let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
      if !iterate then begin
        let attacker_square = tab120.(tab64_square + direction) in
        let attacker = board.(attacker_square) * player_sign in
        if attacker <> 0 then begin
          iterate :=  false;
          if attacker <= (-4) && attacker > !smaller then begin
            if attacker = (-4) then begin
              smaller_possible := false
            end
            else begin
              smaller := attacker
            end;
            move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
          end
        end;
        if !smaller_possible then begin
          let distance = ref 2 in
          while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
            let attacker_square = tab120.(tab64_square + (!distance * direction)) in
            let attacker = board.(attacker_square) * player_sign in
            if attacker = 0 then begin
              incr distance
            end
            else if attacker > 0 then begin
              iterate :=  false
            end
            else begin
              if attacker = (-4) then begin
                smaller_possible := false;
                move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
              end
              else if attacker = (-5) then begin
                smaller := attacker;
                move := Normal {piece = attacker * player_sign; from = attacker_square; to_ = square; capture = piece}
              end;
              iterate :=  false
            end
          done
        end
      end;
      incr i
    done
  end;
  !move

let rec see board square white_to_move =
  let value = ref 0 in
  let move = smaller_attacker board square white_to_move in
  if move <> Null then begin
    make board move;
    value := max 0 (tabvalue.(abs (capture move)) - see board square (not white_to_move));
    unmake board move
  end;
  !value

let see_forced board move =
  make board move;
    let note = tabvalue.(abs (capture move)) - see board (to_ move) (piece move > 0) in
  unmake board move;
  note

(*Tri les coups selon leur potentiel SEE en supprimant ceux dont cette évaluation est négative*)
let tri_see liste board white_to_move =
  begin
    let rec association liste_coups =
      match liste_coups with
      |[] -> []
      (*|Promotion {from = _; to_; promotion; capture} as move :: t ->
        make board move;
        let note = tabvalue.(abs promotion) + tabvalue.(abs capture) - see board to_ white_to_move in
        unmake board move;
        if note >= 0 then
          (note, move) :: association t else association t*)
      |move :: t ->
        make board move;
        let note = tabvalue.(abs (capture move)) - see board (to_ move) white_to_move in
        unmake board move;
        if note >= 0 then
          (note, move) :: association t
        else
          association t
    in List.map snd (merge_sort (association liste))
  end

let get_attackers board square tab64_square first_attacker first_capture first_attacker_square =
  let white_attackers = ref [] in
  let black_attackers = ref [] in
  let first_attacker_direction = ref 0 in
  let first_attacker_distance = ref 0 in
  if abs first_attacker = 1 && first_capture = 0 then begin
    first_attacker_direction := first_attacker * 10;
    first_attacker_distance := abs (square - first_attacker_square) / 8
  end;
  for i = 0 to 7  do
    let direction = knight_vect.(i) in
    if tab120.(tab64_square + direction) <> (-1) then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if abs attacker = 2 && attacker_square <> first_attacker_square then begin
        let attackers = if attacker > 0 then white_attackers else black_attackers in
        attackers := (attacker, 0, 0) :: !attackers
      end
    end;
  done;
  for i = 0 to 3 do
    let direction = bishop_vect.(i) in
    let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
    if !iterate then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square) in
      if attacker <> 0 then begin
        iterate :=  false;
        if ((abs attacker >= 3 && abs attacker <> 4) || (abs attacker = 1 && direction * attacker > 0)) then begin
          if attacker_square <> first_attacker_square then begin
            let attackers = if attacker > 0 then white_attackers else black_attackers in
            attackers := (attacker, direction, 1) :: !attackers;
          end
          else begin
            first_attacker_direction := direction;
            first_attacker_distance := 1
          end
        end
      end;
      let distance = ref 2 in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker_square = tab120.(tab64_square + (!distance * direction)) in
        let attacker = board.(attacker_square) in
        if attacker = 0 then begin
          incr distance
        end
        else if not (List.mem (abs attacker) [3; 5]) then begin
          iterate :=  false
          end
        else begin
          if attacker_square <> first_attacker_square then begin
            let attackers = if attacker > 0 then white_attackers else black_attackers in
            attackers :=  (attacker, direction, !distance) :: !attackers;
          end
          else begin
            first_attacker_direction := direction;
            first_attacker_distance := !distance
          end;
          iterate :=  false
        end
      done
    end
  done;
  for i = 0 to 3 do
    let direction = rook_vect.(i) in
    let iterate = ref (tab120.(tab64_square + direction) <> (-1)) in
    if !iterate then begin
      let attacker_square = tab120.(tab64_square + direction) in
      let attacker = board.(attacker_square)in
      if attacker <> 0 then begin
        iterate :=  false;
        if abs attacker >= 4 then begin
          if attacker_square <> first_attacker_square then begin
            let attackers = if attacker > 0 then white_attackers else black_attackers in
            attackers :=  (attacker, direction, 1) :: !attackers;
          end
          else begin
            first_attacker_direction := direction;
            first_attacker_distance := 1
          end
        end
      end;
      let distance = ref 2 in
      while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
        let attacker_square = tab120.(tab64_square + (!distance * direction)) in
        let attacker = board.(attacker_square) in
        if attacker = 0 then begin
          incr distance
        end
        else if not (List.mem (abs attacker) [4; 5])  then begin
          iterate :=  false
        end
        else begin
          if attacker_square <> first_attacker_square then begin
            let attackers = if attacker > 0 then white_attackers else black_attackers in
            attackers := (attacker, direction, !distance) :: !attackers
          end
          else begin
            first_attacker_direction := direction;
            first_attacker_distance := !distance
          end;
          iterate :=  false
        end
      done
    end
  done;
  white_attackers := List.sort (fun (piece_1,_,_) (piece_2, _, _) -> compare (abs piece_1) (abs piece_2)) !white_attackers;
  black_attackers := List.sort (fun (piece_1,_,_) (piece_2, _, _) -> compare (abs piece_1) (abs piece_2)) !black_attackers;
  if first_attacker > 0 then begin
    white_attackers := (first_attacker, !first_attacker_direction, !first_attacker_distance) :: !white_attackers
  end
  else begin
    black_attackers := (first_attacker, !first_attacker_direction, !first_attacker_distance) :: !black_attackers
  end;
  white_attackers, black_attackers

let directions = [|0; 4; 0; 0; 0; 0; 0; 0; 0; 3; 4; 3|]

let rec insert (x_piece, x_distance, x_direction) list = match list with
  |[] -> [(x_piece, x_distance, x_direction)]
  |(piece, distance, direction) :: t ->
    if abs x_piece <= abs piece then
      (x_piece, x_distance, x_direction) :: list
    else
      (piece, distance, direction) :: insert (x_piece, x_distance, x_direction) t

let xray_attackers board white_attackers black_attackers tab64_square direction distance =
  let xray_attacker = ref (0, 0, 0) in
  let iterate = ref true in
  let distance = ref (distance + 1) in
  while (!iterate && tab120.(tab64_square + (!distance * direction)) <> (-1)) do
    let attacker_square = tab120.(tab64_square + (!distance * direction)) in
    let attacker = board.(attacker_square) in
    if attacker = 0 then begin
      incr distance
    end
    else if not (List.mem (abs attacker) [directions.(abs direction); 5])  then begin
      iterate :=  false
    end
    else begin
      if attacker > 0 then begin
        white_attackers := insert (attacker, direction, !distance) !white_attackers
      end
      else begin
        black_attackers := insert (attacker, direction, !distance) !black_attackers
      end;
      xray_attacker := (attacker, direction, !distance);
      iterate :=  false
    end
  done

let new_see board move =
  let square, first_attacker, first_atttacker_square, first_capture = match move with
    |Normal {piece; from; to_; capture} -> to_, piece, from, capture
    |Promotion {from; to_; capture; promotion} -> to_, (if promotion > 0 then 1 else (-1)), from, capture
    |Enpassant {from; to_} -> to_ , (if to_ < 24 then 1 else (-1)), from, (if to_ < 24 then (-1) else 1)
    |_ -> 0,0,0,0
  in let tab64_square = tab64.(square) in
  let white_attackers, black_attackers = get_attackers board square tab64_square first_attacker first_capture first_atttacker_square in
  let white_to_move = ref (first_attacker > 0) in
  let gain = Array.make 20 0 in
  gain.(0) <- tabvalue.(abs first_capture);
  let index = ref 1 in
  let iterate = ref true in
  while !iterate do
    let attackers = if !white_to_move then white_attackers else black_attackers in
    if !attackers <> [] then begin
      let smaller_attacker, direction, distance = List.hd !attackers in
      attackers := List.tl !attackers;
      white_to_move := not !white_to_move;
      gain.(!index) <- tabvalue.(abs smaller_attacker) - gain.(!index - 1);
      incr index;
      if abs smaller_attacker <> 2 && abs smaller_attacker <> 6 then begin
        xray_attackers board white_attackers black_attackers tab64_square direction distance
      end
    end
    else begin
      iterate := false
    end;
  done;
  for i = (!index - 2) downto 1 do
    gain.(i - 1) <- - (max (-gain.(i - 1)) gain.(i))
  done;
  gain.(0)

(*Fonction triant une liste de coups selon la logique Most Valuable Victim - Least Valuable Agressor*)
let mvvlva move = match move with
  |Normal {piece; from = _; to_ = _; capture} when capture <> 0 ->
    10 * tabvalue.(abs capture) - tabvalue.(abs piece)
  |Enpassant {from = _; to_ = _} ->
    9 * tabvalue.(1)                                                          (*10 * tabvalue.(1) - tabvalue.(1)*)
  |Promotion {from = _; to_ = _; capture; promotion} ->
    10 * (tabvalue.(abs capture) + tabvalue.(abs promotion)) - tabvalue.(1)
  |_ -> 0

let killer_moves = Array.make (2 * max_depth) Null
let history_moves = Array.make 8192 0

let aux_history white_to_move =
  if white_to_move then 0 else 1

let g move = match move with |Normal _ -> true |_ -> false

let move_ordering board white_to_move last_move castling_right king_position in_check ply =
  let legal_moves = legal_moves board white_to_move last_move castling_right king_position in_check in
  let score move =
    (*let copper = if g move then see_forced board move else 0 in*)
    (*if g move then begin
      let _ = see_forced board move in ();
      (*let _ = new_see board move in ();*)
      (*if a <> b then begin
        print_board board;
        print_endline (coord.(from move) ^ coord.(to_ move));
        print_endline (string_of_int a ^ " " ^ string_of_int b)
      end*)
    end;*)
    if isquiet move then begin
      if killer_moves.(2 * ply) = move then begin
        8000000
      end
      else if killer_moves.(2 * ply + 1) = move then begin
        7000000
      end
      else begin
        history_moves.(4096 * aux_history white_to_move + 64 * from move + to_ move)
      end
    end
    else begin
      let see_score = see_forced board move in
      if see_score >= 0 then
        9000000 + see_score
      else
        see_score - 1000000
    end
  in List.map snd (merge_sort (List.map (fun move -> (score move, move)) legal_moves))