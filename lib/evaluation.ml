open Bitboards
open Board
open Bigarray

let is_material_draw position =
  let pieces = position.pieces in
  population_count pieces.(pawn) = 0 && population_count pieces.(pawn + 6) = 0 &&
  population_count pieces.(rook) = 0 && population_count pieces.(rook + 6) = 0 &&
  population_count pieces.(queen) = 0 && population_count pieces.(queen + 6) = 0 &&
  population_count pieces.(knight) + population_count pieces.(bishop) + population_count pieces.(knight + 6) + population_count pieces.(bishop + 6) <= 1

let hidden_weights = Array1.create Int16_signed C_layout (hidden_size * 768)
let hidden_bias = Array1.create Int16_signed C_layout hidden_size

let output_weights = Array1.create Int16_signed C_layout (2 * hidden_size * output_buckets)
let output_bias = Array1.create Int16_signed C_layout output_buckets

let weights_loaded = ref false

let [@inline] feature_weight neuron feature = hidden_weights.{feature * hidden_size + neuron}

let weights_payload_size = 2 * ((768 * hidden_size) + hidden_size + (2 * hidden_size * output_buckets) + output_buckets)
let weights_padding = String.concat "" (List.init 10 (fun _ -> "bullet")) ^ "bu"

let load_weights_contents source contents =
  if String.length contents < weights_payload_size then
    failwith
      (Printf.sprintf
         "Invalid NNUE weights file '%s': expected at least %d bytes, got %d"
         source weights_payload_size (String.length contents));
  let offset = ref 0 in
  let read_int_array kind count =
    Array1.init kind C_layout count (fun _ ->
      let low = Char.code contents.[!offset] in
      let high = Char.code contents.[!offset + 1] in
      offset := !offset + 2;
      let value = low lor (high lsl 8) in
      if value land 0x8000 = 0 then
        value
      else
        value - 0x10000)
  in let loaded_hidden_weights = read_int_array Int16_signed (768 * hidden_size) in
  let loaded_hidden_bias = read_int_array Int16_signed hidden_size in
  let loaded_output_weights = read_int_array Int16_signed (2 * hidden_size * output_buckets) in
  let loaded_output_bias = read_int_array Int16_signed output_buckets in
  Array1.blit loaded_hidden_weights hidden_weights;
  Array1.blit loaded_hidden_bias hidden_bias;
  Array1.blit loaded_output_weights output_weights;
  Array1.blit loaded_output_bias output_bias;
  weights_loaded := true

let read_weights_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  with
  | Sys_error message ->
    failwith (Printf.sprintf "Unable to load NNUE weights from '%s': %s" path message)

let load_weights path = load_weights_contents path (read_weights_file path)

let load_embedded_weights () =
  load_weights_contents "embedded NNUE weights" Embedded_weights.contents

let [@inline] screlu x =
  let x = max 0 (min 255 x) in
  x * x

let get_accumulator position =
  let state = position.state_array.(position.game_ply) in
  let board = position.board in
  let white_to_move = position.white_to_move in
  let side_accumulator = state.side_accumulator in
  let not_side_accumulator = state.not_side_accumulator in
  Array1.blit hidden_bias side_accumulator;
  Array1.blit hidden_bias not_side_accumulator;
  for square =  0 to 63 do
    let piece = board.(square) in
    if piece <> empty then begin
      let piece_type = piece mod 6 in
      let piece_color = piece / 6 in
      let side_is_enemy = piece_color lxor white_to_move in
      let side_idx = 64 * (piece_type + 6 * side_is_enemy) + (square lxor (56 * white_to_move)) in
      let not_side_idx = 64 * (piece_type + 6 * (side_is_enemy lxor 1)) + (square lxor (56 * (white_to_move lxor 1))) in
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i}  + feature_weight i side_idx);
        not_side_accumulator.{i} <- (not_side_accumulator.{i}  + feature_weight i not_side_idx)
      done
    end
  done

let feature_index perspective piece square =
  let enemy = (piece / 6) lxor perspective in
  let oriented_square = square lxor (56 * perspective) in
  64 * ((piece mod 6) + (6 * enemy)) + oriented_square

let update_accumulator position move =
  let board = position.board in
  let white_to_move = position.white_to_move in
  let from = get_move_from move in
  let to_ = get_move_to move in
  let piece = board.(from) mod 6 in
  let captured_piece = board.(to_) mod 6 in

  let p_side = white_to_move lxor 1 in
  let p_not_side = white_to_move in

  let enemy_side = (board.(from) / 6) lxor p_side in
  let enemy_not_side = (board.(from) / 6) lxor p_not_side in
  let enemy_cap_side = (board.(to_) / 6) lxor p_side in
  let enemy_cap_not_side = (board.(to_) / 6) lxor p_not_side in

  let oriented_from_side = from lxor (56 * p_side) in
  let oriented_from_not_side = from lxor (56 * p_not_side) in
  let oriented_to_side = to_ lxor (56 * p_side) in
  let oriented_to_not_side = to_ lxor (56 * p_not_side) in

  let side_idx_to = 64 * (piece + 6 * enemy_side) + oriented_to_side in
  let side_idx_from = 64 * (piece + 6 * enemy_side) + oriented_from_side in
  let side_idx_capture = 64 * (captured_piece + 6 * enemy_cap_side) + oriented_to_side in

  let not_side_idx_to = 64 * (piece + 6 * enemy_not_side) + oriented_to_not_side in
  let not_side_idx_from = 64 * (piece + 6 * enemy_not_side) + oriented_from_not_side in
  let not_side_idx_capture = 64 * (captured_piece + 6 * enemy_cap_not_side) + oriented_to_not_side in

  let state = position.state_array.(position.game_ply) in
  let new_state = position.state_array.(position.game_ply + 1) in
  let side_accumulator = new_state.side_accumulator in
  let not_side_accumulator = new_state.not_side_accumulator in
  Array1.blit state.side_accumulator not_side_accumulator;
  Array1.blit state.not_side_accumulator side_accumulator;
  let flag = get_move_flag move in
  begin match flag with
    |0 |1 ->
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i}  + feature_weight i side_idx_to - feature_weight i side_idx_from);
        not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to - feature_weight i not_side_idx_from)
      done
    |4 ->
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to - feature_weight i side_idx_from - feature_weight i side_idx_capture);
        not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to - feature_weight i not_side_idx_from - feature_weight i not_side_idx_capture)
      done
    |2 ->
      let player_castling_info = castling_infos.(white_to_move) in
      let from_rook = player_castling_info.from_short_rook in
      let to_rook = player_castling_info.to_short_rook in
      let rook_piece_type = rook in
      let rook_color_piece = rook + 6 * white_to_move in
      let rook_enemy_side = (rook_color_piece / 6) lxor p_side in
      let rook_enemy_not_side = (rook_color_piece / 6) lxor p_not_side in
      let oriented_rf_side = from_rook lxor (56 * p_side) in
      let oriented_rt_side = to_rook lxor (56 * p_side) in
      let oriented_rf_not_side = from_rook lxor (56 * p_not_side) in
      let oriented_rt_not_side = to_rook lxor (56 * p_not_side) in
      let side_idx_from_rook = 64 * (rook_piece_type + 6 * rook_enemy_side) + oriented_rf_side in
      let side_idx_to_rook = 64 * (rook_piece_type + 6 * rook_enemy_side) + oriented_rt_side in
      let not_side_idx_from_rook = 64 * (rook_piece_type + 6 * rook_enemy_not_side) + oriented_rf_not_side in
      let not_side_idx_to_rook = 64 * (rook_piece_type + 6 * rook_enemy_not_side) + oriented_rt_not_side in
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to - feature_weight i side_idx_from + feature_weight i side_idx_to_rook - feature_weight i side_idx_from_rook);
        not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to - feature_weight i not_side_idx_from + feature_weight i not_side_idx_to_rook - feature_weight i not_side_idx_from_rook)
      done
    |3 ->
      let player_castling_info = castling_infos.(white_to_move) in
      let from_rook = player_castling_info.from_long_rook in
      let to_rook = player_castling_info.to_long_rook in
      let rook_piece_type = rook in
      let rook_color_piece = rook + 6 * white_to_move in
      let rook_enemy_side = (rook_color_piece / 6) lxor p_side in
      let rook_enemy_not_side = (rook_color_piece / 6) lxor p_not_side in
      let oriented_rf_side = from_rook lxor (56 * p_side) in
      let oriented_rt_side = to_rook lxor (56 * p_side) in
      let oriented_rf_not_side = from_rook lxor (56 * p_not_side) in
      let oriented_rt_not_side = to_rook lxor (56 * p_not_side) in
      let side_idx_from_rook = 64 * (rook_piece_type + 6 * rook_enemy_side) + oriented_rf_side in
      let side_idx_to_rook = 64 * (rook_piece_type + 6 * rook_enemy_side) + oriented_rt_side in
      let not_side_idx_from_rook = 64 * (rook_piece_type + 6 * rook_enemy_not_side) + oriented_rf_not_side in
      let not_side_idx_to_rook = 64 * (rook_piece_type + 6 * rook_enemy_not_side) + oriented_rt_not_side in
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to - feature_weight i side_idx_from + feature_weight i side_idx_to_rook - feature_weight i side_idx_from_rook);
        not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to - feature_weight i not_side_idx_from + feature_weight i not_side_idx_to_rook - feature_weight i not_side_idx_from_rook)
      done
    |5 ->
      let captured_pawn_square = (to_ - push_vects.(white_to_move)) in
      let pawn_color_piece = pawn + 6 * (white_to_move lxor 1) in
      let pawn_enemy_side = (pawn_color_piece / 6) lxor p_side in
      let pawn_enemy_not_side = (pawn_color_piece / 6) lxor p_not_side in
      let oriented_cp_side = captured_pawn_square lxor (56 * p_side) in
      let oriented_cp_not_side = captured_pawn_square lxor (56 * p_not_side) in
      let side_idx_captured_pawn = 64 * (pawn + 6 * pawn_enemy_side) + oriented_cp_side in
      let not_side_idx_captured_pawn = 64 * (pawn + 6 * pawn_enemy_not_side) + oriented_cp_not_side in
      for i = 0 to hidden_size - 1 do
        side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to - feature_weight i side_idx_from - feature_weight i side_idx_captured_pawn);
        not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to - feature_weight i not_side_idx_from - feature_weight i not_side_idx_captured_pawn)
      done
    |_ ->
      begin
        let promotion_piece = (flag lor 4) - 11 in
        let promo_color_piece = promotion_piece + 6 * white_to_move in
        let promo_enemy_side = (promo_color_piece / 6) lxor p_side in
        let promo_enemy_not_side = (promo_color_piece / 6) lxor p_not_side in
        let side_idx_to_promotion = 64 * (promotion_piece + 6 * promo_enemy_side) + oriented_to_side in
        let not_side_idx_to_promotion = 64 * (promotion_piece + 6 * promo_enemy_not_side) + oriented_to_not_side in
        if flag land 4 <> 0 then begin
          for i = 0 to hidden_size - 1 do
          side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to_promotion - feature_weight i side_idx_from - feature_weight i side_idx_capture);
          not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to_promotion - feature_weight i not_side_idx_from - feature_weight i not_side_idx_capture)
        done
        end
        else begin
          for i = 0 to hidden_size - 1 do
            side_accumulator.{i} <- (side_accumulator.{i} + feature_weight i side_idx_to_promotion - feature_weight i side_idx_from);
            not_side_accumulator.{i} <- (not_side_accumulator.{i} + feature_weight i not_side_idx_to_promotion - feature_weight i not_side_idx_from)
          done
        end
      end
  end

let evaluate position =
  let state = position.state_array.(position.game_ply) in
  let side_accumulator = state.side_accumulator in
  let not_side_accumulator = state.not_side_accumulator in
  let number_of_pieces = population_count (position.occupancy.(0) ||| position.occupancy.(1)) in
  let bucket = (number_of_pieces - 2 ) / 4 in
  let bucket_offset = bucket * 2 * hidden_size in
  let output = ref 0 in
  for i = 0 to hidden_size - 1 do
    output := !output
      + screlu side_accumulator.{i} * output_weights.{bucket_offset + i}
      + screlu not_side_accumulator.{i} * output_weights.{bucket_offset + hidden_size + i}
  done;
  ((!output / 255 + output_bias.{bucket}) * 400) / (255 * 64)