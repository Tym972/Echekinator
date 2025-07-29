open Libs.Board
open Libs.Generator
open Libs.Evaluation
open Libs.Piece_square_tables

let rook_mobility board square counter =
  let co = board.(square) in
  let t = tab64.(square) in
  for i = 0 to 3 do
    let dir = rook_vect.(i) in
    let k = ref 1 in
    let s = ref true in
    while (tab120.(t + (!k * dir)) <> (-1) && !s) do
      let candidat = tab120.(t + (!k * dir)) in
      let dest = board.(candidat) in
      if dest = 0 then begin
        incr counter;
        incr k
      end
      else if co * dest > 0 then begin
        s :=  false
      end
      else begin 
        incr counter;
        s :=  false
      end
    done
  done

(*Fonction construisant une liste des déplacements possible d'un fou*)
let bishop_mobility board square counter =
  let co = board.(square) in
  let f = tab64.(square) in
  for i = 0 to 3 do
    let dir = bishop_vect.(i) in
    let k = ref 1 in
    let s = ref true in
    while (tab120.(f + (!k * dir)) <> (-1) && !s) do
      let candidat = tab120.(f + (!k * dir)) in
      let dest = board.(candidat) in
      if dest = 0 then begin
        incr counter;
        incr k
      end
      else if co * dest > 0 then begin
        s :=  false
      end
      else begin
        incr counter;
        s :=  false
      end
    done
  done

(*Fonction construisant une liste des déplacements possible d'un cavalier*)
let knight_mobility board square counter =
  let co = board.(square) in
  let c = tab64.(square) in
  for i = 0 to 7 do
    let dir = knight_vect.(i) in
    if tab120.(c + dir) <> (-1) then begin
      let candidat = tab120.(c + dir) in
      let dest = board.(candidat) in
      if co * dest <= 0 then begin
        incr counter
      end
    end
  done

(*Fonction construisant une liste des déplacements possible d'une dame*)
let queen_mobility board square counter =
  (rook_mobility board square counter);
  (bishop_mobility board square counter)

(*Fonction construisant une liste des déplacements possible d'un roi*)
let king_mobility board square counter =
  let co = board.(square) in
  let r = tab64.(square) in
  for i = 0 to 7 do
    let dir = king_vect.(i) in
    if tab120.(r + dir) <> (-1) then begin
      let candidat = tab120.(r + dir) in
      let dest = board.(candidat) in
      if co * dest <= 0 then begin
        incr counter
      end
    end
  done

(*Fonction construisant une liste des déplacements possible d'un pion*)
let pawn_mobility board square counter =
  let co = board.(square) in
  let p = tab64.(square) in
  if co > 0 then begin
    let candidate_1 = tab120.(p - 10) in
    if board.(candidate_1) = 0 then begin
      if square > 15 then begin
        incr counter;
        if (square > 47 && square < 56) then begin
          let candidate_2 = tab120.(p - 20) in
          if board.(candidate_2) = 0 then begin
            incr counter
          end
        end
      end
      else begin
        counter := !counter + 4
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidate_3 = tab120.(p - 9) in
      let dest3 = board.(candidate_3) in
      if dest3 < 0 then begin
        if square > 15 then begin
          incr counter
        end
        else begin
          counter := !counter + 4
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidat4 = tab120.(p - 11) in
      let dest4 = board.(candidat4) in
      if dest4 < 0 then begin
        if square > 15 then begin
          incr counter
        end
        else begin
          counter := !counter + 4
        end
      end
    end
  end
  else begin
    let candidate_1 = tab120.(p + 10) in
    if board.(candidate_1) = 0 then begin
      if square < 48 then begin
        incr counter;
        if (square > 7 && square < 16) then begin
          let candidate_2 = tab120.(p + 20) in
          if (board.(candidate_2) = 0) then begin
            incr counter
          end
        end
      end
      else begin
        counter := !counter + 4
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidate_3 = tab120.(p + 9) in
      let dest3 = board.(candidate_3) in
      if dest3 > 0 then begin
        if square < 48 then begin
          incr counter
        end
        else begin
          counter := !counter + 4
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidat4 = tab120.(p + 11) in
      let dest4 = board.(candidat4) in
      if dest4 > 0 then begin
        if square < 48 then begin
          incr counter
        end
        else begin
          counter := !counter + 4
        end
      end
    end
  end

let mobility_tab = [|pawn_mobility; knight_mobility; bishop_mobility; rook_mobility; queen_mobility; king_mobility|]

let mobilite board white_to_move =
  let counter = ref 0 in
  if white_to_move then begin
    for i = 63 downto 0 do
      let pl = board.(i) in
      if pl > 0 then begin
        (mobility_tab.(pl - 1) board i counter)
      end
    done
  end
  else begin
    for i = 0 to 63 do
      let pl = board.(i) in
      if pl < 0 then begin
        (mobility_tab.(- pl - 1) board i counter)
      end
    done
  end;
  !counter

(*Fonction indiquant le nombre de square séparant un pion passé de la promotion (elle renvoie 0 si le pion n'est pas passé)*)
let passed_pawn board square =
  let counter = ref 0 in
  let b = ref true in
  let co = board.(square) in
  let t = tab64.(square) in
  let right = ref false in
  let left = ref false in
  if co > 0 then begin
    if tab120.(t - 9) <> (-1) then begin
      right := true
    end;
    if tab120.(t - 11) <> (-1) then begin
      left := true
    end;
    let k = ref 1 in
    while (tab120.(t + (!k * (-10))) <> (-1) && !b) do
    incr counter;
      let candidate_1 = tab120.(t + (!k * (-10))) in
      let dest1 = board.(candidate_1) in
      if dest1 = (-1) then begin
        b := false
      end;
      if (!b && !left) then begin
        let candidate_2 = tab120.((t - 1) + (!k * (-10))) in
        let dest2 = board.(candidate_2) in
        if dest2 = (-1) then begin
          b := false
        end
      end;
      if (!b && !right) then begin
        let candidate_3 = tab120.((t + 1) + (!k * (-10))) in
        let dest3 = board.(candidate_3) in
        if dest3 = (-1) then begin
          b := false
        end
      end;
      if !b then begin
        incr k
      end;
    done
  end
  else begin
    if tab120.(t - 9) <> (-1) then begin
      left := true
    end;
    if tab120.(t - 11) <> (-1) then begin
      right := true
    end;
    let k = ref 1 in
    while (tab120.(t + (!k * 10)) <> (-1) && !b) do
    incr counter;
      let candidate_1 = tab120.(t + (!k * 10)) in
      let dest1 = board.(candidate_1) in
      if dest1 = 1 then begin
        b := false
      end;
      if (!b && !right) then begin
        let candidate_2 = tab120.((t - 1) + (!k * 10)) in
        let dest2 = board.(candidate_2) in
        if dest2 = 1 then begin
          b := false
        end
      end;
      if (!b && !left) then begin
        let candidate_3 = tab120.((t + 1) + (!k * 10)) in
        let dest3 = board.(candidate_3) in
        if dest3 = 1 then begin
          b := false
        end
      end;
      if !b then begin
        incr k
      end;
    done
  end;
  if !b then !counter else 0

(*Fonction détectant les positions nulles par manque de matériel, si les joueurs jouent correctement*)
let manque_de_materiel_approximatif board =
  let possibly_insufficient = ref true in
  let counter = ref 0 in
  let cavaliers_blancs = ref 0 in
  let cavaliers_noirs = ref 0 in
  let fous_blancs= ref 0 in
  let fous_noirs = ref 0 in
  let i = ref 0 in
  while (!possibly_insufficient && !i < 64) do
    let square = board.(!i) in
    if square <> 0 then begin
      if insufficient_mating_materiel_vect.(abs square) then begin
        possibly_insufficient := false
      end
      else if abs square <> 6 then begin
        incr counter;
        if !counter > 2 then begin
          possibly_insufficient := false
        end
        else begin
          match square with
            |2 -> cavaliers_blancs := !cavaliers_blancs + 1
            |(-2) -> cavaliers_noirs := !cavaliers_noirs + 1
            |3 -> fous_blancs := !fous_blancs + 1
            |(-3) -> fous_noirs := !fous_noirs + 1
            |_ -> ()
        end
      end
    end;
    incr i
  done;
  if !possibly_insufficient && !counter = 2 && (!cavaliers_blancs <> 2 && !cavaliers_noirs <> 2) && ((!fous_blancs + !cavaliers_blancs) <> (!fous_noirs + !cavaliers_noirs)) then begin
    possibly_insufficient := false
  end;
  !possibly_insufficient

(*Fonction évaluant le positionnement d'une rook ou d'une dame*)
let evalue_tour board square white_to_move defendues attaquee king_position king_in_check piece_clouees =
  let counter = ref 0 in
  let co = board.(square) in
  let t = tab64.(square) in
  if co > 0 then begin
    for i = 0 to 3 do
      let dir = rook_vect.(i) in
      let k = ref 1 in
      let s = ref true in 
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest > 0 then begin
          counter := !counter + 2;
          if not white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end;
          s := false
        end
      done
    done
  end
  else begin
    for i = 0 to 3 do
      let dir = rook_vect.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest < 0 then begin
          counter := !counter + 2;
          if white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end;
          s := false
        end
      done
    done
  end;
  !counter

(*Fonction évaluant le positionnement d'un fou ou d'une dame*)
let evalue_fou board square white_to_move defendues attaquee king_position king_in_check piece_clouees =
  let counter = ref 0 in
  let co = board.(square) in
  let t = tab64.(square) in
  if co > 0 then begin
    for i = 0 to 3 do
      let dir = bishop_vect.(i) in
      let k = ref 1 in
      let s = ref true in 
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest > 0 then begin
          counter := !counter + 2;
          if not white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end;
          s := false
        end
      done
    done
  end
  else begin
    for i = 0 to 3 do
      let dir = bishop_vect.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest < 0 then begin
          counter := !counter + 2;
          if white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end;
          s := false
        end
      done
    done
  end;
  !counter

(*Fonction évaluant le positionnement d'un cavalier*)
let evalue_cavalier board square white_to_move defendues attaquee king_position king_in_check piece_clouees = 
  let counter = ref 0 in
  let co = board.(square) in
  let c = tab64.(square) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = knight_vect.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = board.(candidat) in
        if dest <= 0 then begin
          if dest = 0 then begin
            counter := !counter + 1
          end
          else if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end
        end
        else begin
          counter := !counter + 2;
          if not white_to_move then begin
            defendues := candidat :: !defendues;
          end
        end
      end
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = knight_vect.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = board.(candidat) in
        if dest >= 0 then begin
          if dest = 0 then begin
            counter := !counter + 1
          end
          else if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end
        end
        else begin
          counter := !counter + 2;
          if white_to_move then begin
            defendues := candidat :: !defendues;
          end
        end
      end
    done
  end;
  !counter

(*Fonction donnant le nombre de cases controllées par une dame*)
let evalue_dame board square white_to_move defendues attaquee king_position king_in_check piece_clouees =
  let counter = ref 0 in
  let co = board.(square) in
  let t = tab64.(square) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = king_vect.(i) in
      let k = ref 1 in
      let s = ref true in 
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest > 0 then begin
          counter := !counter + 2;
          if not white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = tabvalue.(co) - tabvalue.(- dest) in
              if difference < 0 then begin
                attaquee := max !attaquee (- difference)
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(- dest)
            end
          end;
          s := false
        end
      done
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = king_vect.(i) in
      let k = ref 1 in
      let s = ref true in
      while (tab120.(t + (!k * dir)) <> (-1) && !s) do
        let candidat = tab120.(t + (!k * dir)) in
        let dest = board.(candidat) in
        if dest = 0 then begin
          incr counter;
          k := !k + 1
        end
        else if dest < 0 then begin
          counter := !counter + 2;
          if white_to_move then begin
            defendues := candidat :: !defendues;
          end;
          s := false
        end
        else begin
          if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidat; capture = dest}) white_to_move king_position king_in_check piece_clouees) then begin
            if List.mem candidat !defendues then begin
              let difference = - tabvalue.(- co) + tabvalue.(dest) in
              if difference > 0 then begin
                attaquee := max !attaquee difference
              end
            end
            else begin
              attaquee := max !attaquee tabvalue.(dest)
            end
          end;
          s := false
        end
      done
    done
  end;
  !counter

(*Fonction évaluant le positionnement d'un roi*) 
let evalue_roi board square white_to_move defendues attaquee king_position king_in_check piece_clouees = let _ = king_position, king_in_check, piece_clouees in
  let counter = ref 0 in
  let co = board.(square) in
  let c = tab64.(square) in
  if co > 0 then begin
    for i = 0 to 7 do
      let dir = king_vect.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = board.(candidat) in
        if dest <= 0 then begin
          if dest = 0 then begin
            counter := !counter + 1
          end
          else begin
            if white_to_move && not (List.mem candidat !defendues) then begin
              attaquee := max !attaquee tabvalue.(- dest)
            end;
          incr counter;
          end
        end
        else begin
          counter := !counter + 2;
          if not white_to_move then begin
            defendues := candidat :: !defendues;
          end;
        end
      end
    done
  end
  else begin
    for i = 0 to 7 do
      let dir = king_vect.(i) in
      if tab120.(c + dir) <> (-1) then begin
        let candidat = tab120.(c + dir) in
        let dest = board.(candidat) in
        if dest >= 0 then begin
          if dest = 0 then begin
            counter := !counter + 1
          end
          else begin
            if not (white_to_move || List.mem candidat !defendues) then begin
              attaquee := max !attaquee tabvalue.(dest)
            end;
          incr counter;
          end
        end
        else begin
          counter := !counter + 2;
          if white_to_move then begin
            defendues := candidat :: !defendues;
          end;
        end
      end
    done
  end;
  !counter

(*Fonction évaluant le positionnement d'un pion*)
let evalue_pion board square white_to_move defendues attaquee king_position king_in_check piece_clouees =
  let counter = ref 0 in
  let co = board.(square) in
  let p = tab64.(square) in
  if co > 0 then begin
    if ((square + 1) mod 8 <> 0) then begin
      let candidate_1 = tab120.(p - 9) in
      let dest1 = board.(candidate_1) in
      if dest1 <= 0 then begin
        if dest1 = 0 then begin
          counter := !counter + 1
        end
        else if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidate_1; capture = dest1}) white_to_move king_position king_in_check piece_clouees) then begin
          if List.mem candidate_1 !defendues then begin
            let difference = tabvalue.(co) - tabvalue.(- dest1) in
            if difference < 0 then begin
              attaquee := max !attaquee (- difference)
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(- dest1)
          end
        end
      end
      else begin
        counter := !counter + 2;
        if not white_to_move then begin
          defendues := candidate_1 :: !defendues;
        end
      end
    end;
    if (square mod 8 <> 0) then begin
      let candidate_2 = tab120.(p - 11) in
      let dest2 = board.(candidate_2) in
      if dest2 <= 0 then begin
        if dest2 = 0 then begin
          counter := !counter + 1
        end
        else if white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidate_2; capture = dest2}) white_to_move king_position king_in_check piece_clouees) then begin
          if List.mem candidate_2 !defendues then begin
            let difference = tabvalue.(co) - tabvalue.(- dest2) in
            if difference < 0 then begin
              attaquee := max !attaquee (- difference)
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(- dest2)
          end
        end
      end
      else begin
        counter := !counter + 2;
        if not white_to_move then begin
          defendues := candidate_2 :: !defendues;
        end
      end
    end
  end
  else begin
    if (square mod 8 <> 0) then begin
      let candidate_1 = tab120.(p + 9) in
      let dest1 = board.(candidate_1) in
      if dest1 >= 0 then begin
        if dest1 = 0 then begin
          counter := !counter + 1
        end
        else if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidate_1; capture = dest1}) white_to_move king_position king_in_check piece_clouees) then begin
          if List.mem candidate_1 !defendues then begin
            let difference = - tabvalue.(- co) + tabvalue.(dest1) in
            if difference > 0 then begin
              attaquee := max !attaquee difference
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(dest1)
          end
        end
      end
      else begin
        counter := !counter + 2;
        if white_to_move then begin
          defendues := candidate_1 :: !defendues;
        end
      end
    end;
    if ((square + 1) mod 8 <> 0) then begin
      let candidate_2 = tab120.(p + 11) in
      let dest2 = board.(candidate_2) in
      if dest2 >= 0 then begin
        if dest2 = 0 then begin
          counter := !counter + 1
        end
        else if not white_to_move && (is_legal_effective board (Normal {piece = co; from = square; to_ = candidate_2; capture = dest2}) white_to_move king_position king_in_check piece_clouees) then begin
          if List.mem candidate_2 !defendues then begin
            let difference = - tabvalue.(- co) + tabvalue.(dest2) in
            if difference > 0 then begin
              attaquee := max !attaquee difference
            end
          end
          else begin
            attaquee := max !attaquee tabvalue.(dest2)
          end
        end
      end
      else begin
        counter := !counter + 2;
        if white_to_move then begin
          defendues := candidate_2 :: !defendues;
        end;
      end
    end
  end;
  !counter

let tabfun2 = [|evalue_pion; evalue_cavalier; evalue_fou; evalue_tour; evalue_dame; evalue_roi|]

let placement_ouverture board material position =
  if board.(59) = 5 then begin
    position := !position + 3
  end;
  if board.(3) = (-5) then begin
    position := !position - 3
  end;
  if board.(57) = 2 then begin
    position := !position - 3
  end;
  if board.(62) = 2 then begin
    position := !position - 3
  end;
  if board.(1) = (-2) then begin
    position := !position + 3
  end;
  if board.(6) = (-2) then begin
    position := !position + 3
  end;
  if board.(58) = 3 then begin
    position := !position - 3
  end;
  if board.(61) = 3 then begin
    position := !position - 3
  end;
  if board.(2) = (-3) then begin
    position := !position + 3
  end;
  if board.(5) = (-3) then begin
    position := !position + 3
  end;
  if board.(27) = 1 then begin
    position := !position + 8
  end
  else if board.(27) = (-1) then begin
    position := !position - 8
  end;
  if board.(28) = 1 then begin
    position := !position + 8
  end
  else if board.(28) = (-1) then begin
    position := !position - 8
  end;
  if board.(35) = 1 then begin
    position := !position + 8
  end
  else if board.(35) = (-1) then begin
    position := !position - 8
  end;
  if board.(36) = 1 then begin
    position := !position + 8
  end
  else if board.(36) = (-1) then begin
    position := !position - 8
  end;
  if (board.(62) = 6 && board.(63) <> 4) then begin
    position := !position + 15;
    if (board.(53) = 1) then begin
      position := !position + 5
    end;
    if (board.(54) = 1) then begin
      position := !position + 10
    end
    else if (board.(46) = 1) then begin
      position := !position + 5
    end;
    if (board.(55) = 1) then begin
      position := !position + 2
    end
  end
  else if (board.(58) = 6 && board.(56) <> 4) then begin
    position := !position + 15;
    if (board.(48) = 1) then begin
      position := !position + 2
    end; 
    if (board.(49) = 1) then begin
      position := !position + 10
    end
    else if (board.(41) = 1) then begin
      position := !position + 5
    end;
    if (board.(50) = 1) then begin 
      position := !position + 5
    end
  end
  else if board.(60) <> 6 then begin
    material := !material - 4;
    if board.(59) = 6 || board.(61) = 6 then begin
      material := !material + 1
    end
  end;
  if (board.(6) = (-6) && board.(7) <> (-4))then begin
    position := !position - 15;
    if (board.(13) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(14) = (-1)) then begin
      position := !position - 10
    end
    else if (board.(22) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(15) = (-1)) then begin
      position := !position - 2
    end
  end
  else if (board.(2) = (-6) && board.(0) <> (-4)) then begin
    position := !position - 15;
    if (board.(8) = (-1)) then begin
      position := !position - 2
    end;
    if (board.(9) = (-1)) then begin
      position := !position - 10
    end
    else if (board.(17) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(10) = (-1)) then begin
      position := !position - 5
    end
  end
  else if board.(4) <> (-6) then begin
    material := !material + 4;
    if board.(3) = (-6) || board.(5) = (-6) then begin
      material := !material - 1
    end
  end

let placement_mdj board position =
  if (board.(62) = 6 && board.(63) <> 4) then begin
    position := !position + 15;
    if (board.(53) = 1) then begin
      position := !position + 5
    end;
    if (board.(54) = 1) then begin
      position := !position + 10
    end
    else if (board.(46) = 1) then begin
      position := !position + 5
    end;
    if (board.(55) = 1) then begin
      position := !position + 2
    end
  end
  else if ((board.(58) = 6 || board.(57) = 6) && board.(56) <> 4) then begin
    position := !position + 15;
    if (board.(48) = 1) then begin
      position := !position + 2
    end; 
    if (board.(49) = 1) then begin
      position := !position + 10
    end
    else if (board.(41) = 1) then begin
      position := !position + 5
    end;
    if (board.(50) = 1) then begin 
      position := !position + 5
    end
  end;
  if (board.(6) = (-6) && board.(7) <> (-4))then begin
    position := !position - 15;
    if (board.(13) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(14) = (-1)) then begin
      position := !position - 10
    end
    else if (board.(22) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(15) = (-1)) then begin
      position := !position - 2
    end
  end
  else if ((board.(2) = (-6) || board.(1) = (-6)) && board.(0) <> (-4)) then begin
    position := !position - 15;
    if (board.(8) = (-1)) then begin
      position := !position - 2
    end;
    if (board.(9) = (-1)) then begin
      position := !position - 10
    end
    else if (board.(17) = (-1)) then begin
      position := !position - 5
    end;
    if (board.(10) = (-1)) then begin
      position := !position - 5
    end
  end

let eval_noirs_sl board white_to_move piece_clouees defendues pieces_joueur attaque_noirs king_position king_in_check material position =
  for i = 0 to 63 do
    let square = board.(i) in
    if square < 0 then begin
      let eval_piece = (tabfun2.(- square - 1)) board i white_to_move defendues attaque_noirs king_position king_in_check piece_clouees in
      material := !material - tabvalue.(- square);
      position := !position - eval_piece
    end
    else if square > 0 then begin
      pieces_joueur := i :: !pieces_joueur
    end
  done
  
let rec eval_blancs liste_cases board white_to_move defendues attaque_blancs king_position king_in_check piece_clouees material position =
  match liste_cases with
    |[] -> ()
    |h::t -> 
      let square = board.(h) in
      let eval_piece = (tabfun2.(square - 1)) board h white_to_move defendues attaque_blancs king_position king_in_check piece_clouees in
      material := !material + tabvalue.(square);
      position := !position + eval_piece;
      eval_blancs t board white_to_move defendues attaque_blancs king_position king_in_check piece_clouees material position

let eval_blancs_sl board white_to_move piece_clouees defendues pieces_joueur attaque_blancs king_position king_in_check material position =
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      let eval_piece = (tabfun2.(square - 1)) board i white_to_move defendues attaque_blancs king_position king_in_check piece_clouees in
      material := !material + tabvalue.(square);
      position := !position + eval_piece;
    end
    else if square < 0 then begin
      pieces_joueur := i :: !pieces_joueur
    end
  done

let rec eval_noirs liste_cases board white_to_move defendues attaque_noirs king_position king_in_check piece_clouees material position =
  match liste_cases with
    |[] -> ()
    |h::t -> let square = board.(h) in
      let eval_piece = (tabfun2.(- square - 1)) board h white_to_move defendues attaque_noirs king_position king_in_check piece_clouees in
      material := !material - tabvalue.(- square);
      position := !position - eval_piece;
      eval_noirs t board white_to_move defendues attaque_noirs king_position king_in_check piece_clouees material position

let evaluation_double board white_to_move king_position king_in_check material position =
  let piece_clouees = pinned_squares board king_position white_to_move in
  let defendues = ref [] in
  let pieces_joueur = ref [] in
  let attaque_blancs = ref 0 in
  let attaque_noirs = ref 0 in
  if white_to_move then begin
    eval_noirs_sl board white_to_move piece_clouees defendues pieces_joueur attaque_noirs king_position king_in_check material position;
    eval_blancs !pieces_joueur board white_to_move defendues attaque_blancs king_position king_in_check piece_clouees material position;
    material := !material + !attaque_blancs
  end
  else begin
    eval_blancs_sl board white_to_move piece_clouees defendues pieces_joueur attaque_blancs king_position king_in_check material position;
    eval_noirs !pieces_joueur board white_to_move defendues attaque_noirs king_position king_in_check piece_clouees material position;
    material := !material - !attaque_noirs
  end

let evaluation_double_finale board white_to_move king_position king_in_check material position =
  let piece_clouees = pinned_squares board king_position white_to_move in
  let defendues = ref [] in
  let pieces_joueur = ref [] in
  let attaque_blancs = ref 0 in
  let attaque_noirs = ref 0 in
  if white_to_move then begin
    for i = 0 to 63 do
      let square = board.(i) in
      if square < 0 then begin
        if square = (-1) then begin
          material := !material - (2 * (7 - passed_pawn board i));
        end;
        let eval_piece = (tabfun2.(- square - 1)) board i white_to_move defendues attaque_noirs king_position king_in_check piece_clouees in
        material := !material - tabvalue.(- square);
        position:= !position - eval_piece
      end
      else if square > 0 then begin
        if square = 1 then begin
          material := !material + (2 * (7 - passed_pawn board i))
        end;
        pieces_joueur := i :: !pieces_joueur
      end
    done;
    let rec eval_blancs liste_cases = match liste_cases with
      |[] -> ()
      |h::t -> let square = board.(h) in
                let eval_piece = (tabfun2.(square - 1))board h white_to_move defendues attaque_blancs king_position king_in_check piece_clouees in
                material := !material + tabvalue.(square);
                position:= !position + eval_piece;
                eval_blancs t
    in eval_blancs !pieces_joueur;
    material := !material + !attaque_blancs
  end
  else begin
    for i = 0 to 63 do
      let square = board.(i) in
      if square > 0 then begin
        if square = 1 then begin
          material := !material + (2 * (7 - passed_pawn board i))
        end;
        let eval_piece = (tabfun2.(square - 1)) board i white_to_move defendues attaque_blancs king_position king_in_check piece_clouees in
        material := !material + tabvalue.(square);
        position:= !position + eval_piece;
      end
      else if square < 0 then begin
        if square = (-1) then begin
          material := !material - (2 * (7 - passed_pawn board i))
        end;
        pieces_joueur := i :: !pieces_joueur
      end
    done;
    let rec eval_noirs liste_cases = match liste_cases with
      |[] -> ()
      |h::t -> let square = board.(h) in
                let eval_piece = (tabfun2.(- square - 1)) board h white_to_move defendues attaque_noirs king_position king_in_check piece_clouees in
                material := !material - tabvalue.(- square);
                position:= !position - eval_piece;
                eval_noirs t
    in eval_noirs !pieces_joueur;
    material := !material - !attaque_noirs
  end

(*Fonction évaluant la position d'un player en utilisée en ouverture si aucun coup théorique n'existe*)
let evalue_ouverture board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let material = ref (2 * (doubled board)) in
  let position = ref 0 in
  placement_ouverture board material position;
  evaluation_double board white_to_move king_position king_in_check material position;
  treatment white_to_move !material !position

(*Fonction évaluant la position d'un player utilisée en milieu de jeu*)
let evalue_mdj board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let material = ref (2 * (doubled board)) in
  let position = ref 0 in
  placement_mdj board position;
  evaluation_double board white_to_move king_position king_in_check material position;
  treatment white_to_move !material !position

(*Fonction évaluant la position d'un player utilisée en finale*)
let evalue_finale board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  if manque_de_materiel_approximatif board then begin
    0
  end
  else begin
    let material = ref (2 * (doubled board)) in
    let position = ref 0 in
    evaluation_double_finale board white_to_move king_position king_in_check material position;
    treatment white_to_move !material !position
  end

(**)
let eval_materiel board =
  let material = ref 0 in
  for i = 0 to 63 do
    let square = board.(i) in
    if square > 0 then begin
      material := !material + tabvalue.(square)
    end
    else if square < 0 then begin
      material := !material - tabvalue.(- square)
    end
  done;
  !material

(*Fonction d'évaluation à n'appliquer que sur les positions stables*)
let evalue_simple board white_to_move (king_position : int) (king_in_check : bool) alpha beta = let _ = alpha, beta in
  let _ = white_to_move, king_position, king_in_check in
  let position = ref 0 in
  let note_provisoire = treatment white_to_move (eval_materiel board) 0 in
  note_provisoire + !position

let fp board position table =
  let note = ref 0 in
  let tb, tn = table in
  for i = 0 to 63 do 
    let square = board.(i) in
    if square > 0 then begin
      note := !note + tb.(square - 1).(i)
    end
    else if square < 0 then begin
      note := !note - tn.(abs square - 1).(i)
    end
  done;
  position := !position + (!note / 5)

let eval1 board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let material = ref (2 * (doubled board)) in
  let position = ref 0 in
  fp board position tab_ouverture;
  evaluation_double board white_to_move king_position king_in_check material position;
  treatment white_to_move !material !position

let eval2 board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let material = ref (2 * (doubled board)) in
  let position = ref 0 in
  fp board position tab_mdg;
  evaluation_double board white_to_move king_position king_in_check material position;
  treatment white_to_move !material !position

let eval3 board white_to_move king_position king_in_check (alpha : int) (beta : int) =
  let _ = alpha, beta in
  let material = ref (2 * (doubled board)) in
  let position = ref 0 in
  fp board position tab_finale;
  evaluation_double board white_to_move king_position king_in_check material position;
  treatment white_to_move !material !position