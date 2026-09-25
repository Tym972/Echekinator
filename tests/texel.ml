open Libs.Board
open Libs.Fen
open Libs.Evaluation

let k = ref 1.

type training_data = {
  fen_string : string;
  result : float
}

let sigmoid s =
  1. /. (1. +. Float.pow 10. (-. !k *. s /. 400.))

let format_result result = match result with
  | "1-0" -> 1.
  | "1/2-1/2" -> 0.5
  | _ -> 0.

let read_file filename =
  let ic = open_in filename in
  let data_list = ref [] in
  let continue = ref true in
  while !continue do
    try
      let line = input_line ic in begin
        match String.split_on_char '|' line with
        | [fen_string; _; result] -> data_list := {
            fen_string = String.trim fen_string;
            result = format_result (String.trim result)
          } :: !data_list
        | _ -> ()
      end
    with End_of_file -> continue := false
  done;
  close_in ic;
  Array.of_list (List.rev !data_list)

let round_to_int f = int_of_float (Float.round f)

let update_weights weights =
  for i = 0 to 383 do
    mg_pst.(i) <- round_to_int weights.(i);
    eg_pst.(i) <- round_to_int weights.(i + 384)
  done;
  for i = 0 to 3 do
    mob_center.(i + 1) <- round_to_int weights.(i + 768);
    mob_step_mg.(i + 1) <- round_to_int weights.(i + 772);
    mob_step_eg.(i + 1) <- round_to_int weights.(i + 776)
  done;
  for i = 0 to 5 do
    pp_mg.(i + 1) <- round_to_int weights.(i + 780);
    pp_eg.(i + 1) <- round_to_int weights.(i + 786);
    pp_blocked_mg.(i + 1) <- round_to_int weights.(i + 792);
    pp_blocked_eg.(i + 1) <- round_to_int weights.(i + 798);
  done;
  for i = 0 to 6 do
    shield_value.(i + 1) <- round_to_int weights.(i + 804)
  done;
  distance_weight := round_to_int weights.(811);
  bishop_mg := round_to_int weights.(812);
  bishop_eg := round_to_int weights.(813);
  open_mg := round_to_int weights.(814);
  open_eg := round_to_int weights.(815);
  semi_open_mg := round_to_int weights.(816);
  semi_open_eg := round_to_int weights.(817);
  open_king := round_to_int weights.(818);
  doubled_mg := round_to_int weights.(819);
  doubled_eg := round_to_int weights.(820);
  isolated_mg := round_to_int weights.(821);
  isolated_eg := round_to_int weights.(822)

(* Calcul de l'erreur parallélisé sur tous les cœurs CPU disponibles *)
let calculate_error weights data =
  update_weights weights;
  let n = Array.length data in
  let num_domains = min 16 (max 1 (Domain.recommended_domain_count ())) in
  
  if n < num_domains then begin
    let position = create_position () in
    let search_tables = Libs.Move_ordering.create_search_tables () in
    let sigma = ref 0. in
    for i = 0 to n - 1 do
      position_of_fen data.(i).fen_string position;
      let score = float_of_int (Libs.Quiescence.quiescence_search position search_tables 0 0 (-max_int) max_int * (- 2 * position.white_to_move + 1)) in
      let err = data.(i).result -. (sigmoid score) in
      sigma := !sigma +. (err *. err)
    done;
    !sigma /. (float_of_int n)
  end else begin
    let chunk_size = n / num_domains in
    let results = Array.init num_domains (fun d ->
      Domain.spawn (fun () ->
        let position = create_position () in
        let search_tables = Libs.Move_ordering.create_search_tables () in
        let start_idx = d * chunk_size in
        let end_idx = if d = num_domains - 1 then n else start_idx + chunk_size in
        let local_sigma = ref 0. in
        for i = start_idx to end_idx - 1 do
          position_of_fen data.(i).fen_string position;
          let score = float_of_int (Libs.Quiescence.quiescence_search position search_tables 0 0 (-max_int) max_int * (- 2 * position.white_to_move + 1)) in
          let err = data.(i).result -. (sigmoid score) in
          local_sigma := !local_sigma +. (err *. err)
        done;
        !local_sigma
      )
    ) in
    let total_sigma = Array.fold_left (fun acc dom -> acc +. Domain.join dom) 0. results in
    total_sigma /. (float_of_int n)
  end

let save_weights filename weights =
  let oc = open_out filename in
  Printf.fprintf oc "let weights = [| ";
  Array.iteri (fun i w ->
    if i > 0 then Printf.fprintf oc "; ";
    Printf.fprintf oc "%i" (int_of_float w)
  ) weights;
  Printf.fprintf oc " |]\n";
  close_out oc;
  Printf.printf "-> Poids sauvegardés dans %s (Meilleure validation)\n" filename

let get_random_batch data batch_size =
  let n = Array.length data in
  if n <= batch_size then data
  else Array.init batch_size (fun _ -> data.(Random.int n))

let gradient_descent initial_weights max_epochs train_data val_data =
  let dim = Array.length initial_weights in
  let weights = Array.copy initial_weights in
  let best_weights = Array.copy initial_weights in
  
  let total_n = Array.length train_data in
  let batch_size = min 100000 total_n in 
  
  (* Évaluation initiale sur le jeu de validation *)
  let best_val_error = ref (calculate_error weights val_data) in
  
  Printf.printf "Erreur MSE initiale sur la validation : %f\n" !best_val_error;
  Printf.printf "Nombre de cœurs CPU utilisés : %d\n" (Domain.recommended_domain_count ());
  flush stdout;

  Random.self_init ();
  let c = 2. in 
  let a = 10000. in 

  for epoch = 1 to max_epochs do
    let current_batch = get_random_batch train_data batch_size in
    
    let ak = a /. (Float.pow (float_of_int epoch +. 50.) 0.602) in
    let ck = c /. (Float.pow (float_of_int epoch) 0.101) in

    let delta = Array.init dim (fun _ -> if Random.bool () then 1. else -1.) in
    
    let weights_plus = Array.init dim (fun i -> weights.(i) +. (ck *. delta.(i))) in
    let weights_minus = Array.init dim (fun i -> weights.(i) -. (ck *. delta.(i))) in

    let err_plus = calculate_error weights_plus current_batch in
    let err_minus = calculate_error weights_minus current_batch in

    let diff = err_plus -. err_minus in
    for i = 0 to dim - 1 do
      let g_i = diff /. (2. *. ck *. delta.(i)) in
      if not (Float.is_nan g_i) then
        weights.(i) <- max (-20000.) (min 20000. (weights.(i) -. (ak *. g_i)));
    done;

    let current_val_error = calculate_error weights val_data in
    
    if current_val_error < !best_val_error then begin
      best_val_error := current_val_error;
      Array.blit weights 0 best_weights 0 dim;
      Printf.printf "Epoch %d/%d | ⭐ Nouveau record Validation MSE : %f\n" epoch max_epochs !best_val_error;
      save_weights "weights_tuned.txt" best_weights;
    end;

    if epoch mod 100 = 0 || epoch = 1 then begin
      let current_train_error = calculate_error weights train_data in
      Printf.printf "Epoch %d/%d | Train MSE : %f | Val MSE : %f | Meilleure Val MSE : %f\n" 
        epoch max_epochs current_train_error current_val_error !best_val_error;
      flush stdout;
    end;
  done;
  best_weights

let () =
  let initial_weights = Array.init (Array.length weights) (fun i -> float_of_int weights.(i)) in

  print_endline "Chargement des données d'entraînement...";
  let train_data = read_file "Training_HCE_pompée.txt" in
  Printf.printf "Nombre de positions d'entraînement : %d\n" (Array.length train_data);

  print_endline "Chargement des données de validation...";
  let val_data = read_file "Validation_HCE_pompée.txt" in
  Printf.printf "Nombre de positions de validation : %d\n" (Array.length val_data);
  
  print_endline "Début de la descente de gradient SPSA (avec validation)...";
  let _ = gradient_descent initial_weights 100000 train_data val_data in
  
  print_endline "Optimisation terminée !"