open Board
open Generator
open Evaluation

let smaller_attacker board square white_to_move =
  let move = ref Null in
  let attacker_found = ref false in
  let tab64_square = tab64.(square) in
  let piece = board.(square) in
  let player_sign = if white_to_move then 1 else (-1) in
  let pawn_vect = [|(-9) * player_sign; (-11) * player_sign|] in
  let i = ref 0 in
  while (not !attacker_found && !i < 2) do
    let dir = pawn_vect.(!i) in
    if tab120.(tab64_square + dir) <> (-1) then begin
      let candidat = tab120.(tab64_square + dir) in
      if board.(candidat) = (- player_sign) then begin
        let coup_potentiel = Normal {piece = (- player_sign); from = candidat; to_ = square; capture = piece} in
          attacker_found := true;
          move := coup_potentiel
      end
    end;
    incr i
  done;
  if not !attacker_found then begin
    let i = ref 0 in
    while (not !attacker_found && !i < 8) do
      let dir = knight_vect.(!i) in
      if tab120.(tab64_square + dir) <> (-1) then begin
        let candidat = tab120.(tab64_square + dir) in
        if board.(candidat) = (-2) * player_sign then begin
          let coup_potentiel = Normal {piece = (-2) * player_sign; from = candidat; to_ = square; capture = piece} in
            attacker_found := true;
            move := coup_potentiel
        end
      end;
      incr i
    done
  end;
  if not !attacker_found then begin
    let i = ref 0 in
    while (not !attacker_found && !i < 4) do
      let dir = bishop_vect.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(tab64_square + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(tab64_square + (!k * dir)) in
        let attaquant = board.(candidat) in
        let neg_attaquant = attaquant * player_sign in
        if attaquant = 0 then begin
          incr k
        end
        else if neg_attaquant > 0 then begin
          s :=  false
        end
        else begin
          if neg_attaquant = (-3) || neg_attaquant = (-5) || (neg_attaquant = (-6) && !k = 1) then begin
            let coup_potentiel = Normal {piece = attaquant; from = candidat; to_ = square; capture = piece} in
              if neg_attaquant = (-3) then begin
                attacker_found := true
              end;
              move := coup_potentiel
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  if not !attacker_found then begin
    let i = ref 0 in
    while (not !attacker_found && !i < 4) do
      let dir = rook_vect.(!i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(tab64_square + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(tab64_square + (!k * dir)) in
        let attaquant = board.(candidat) in
        let neg_attaquant = attaquant * player_sign in
        if attaquant = 0 then begin
          incr k
        end
        else if neg_attaquant  > 0 then begin
          s :=  false
        end
        else begin
          if neg_attaquant = (-4) || neg_attaquant = (-5) || (neg_attaquant = (-6) && !k = 1) then begin
            let coup_potentiel = Normal {piece = attaquant; from = candidat; to_ = square; capture = piece} in
              attacker_found := true;
              move := coup_potentiel
          end;
          s :=  false
        end
      done;
      incr i
    done
  end;
  !move

(*Static Exchange Evaluation, examine une série d'échange sur une square donnée*)
let rec see board square white_to_move =
  let value = ref 0 in
  let move = smaller_attacker board square white_to_move in
  if move <> Null then begin
    make board move;
    value := max 0 (tabvalue.(abs (capture move)) - see board square (not white_to_move));
    unmake board move
  end;
  !value

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
          (note, move) :: association t else association t
    in List.map snd (merge_sort (association liste))
  end

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

let move_ordering board white_to_move last_move castling_right king_position in_check ply =
  let legal_moves = legal_moves board white_to_move last_move castling_right king_position in_check in
  let score move =
    if isquiet move then begin
      if killer_moves.(2 * ply) = move then begin
        90000000
      end
      else if killer_moves.(2 * ply + 1) = move then begin
        80000000
      end
      else begin
        history_moves.(4096 * aux_history white_to_move + 64 * from move + to_ move)
      end
    end
    else begin
      100000000 + mvvlva move
    end
  in List.map snd (merge_sort (List.map (fun move -> (score move, move)) legal_moves))