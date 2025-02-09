open Libs.Plateau
open Config
open Libs.Interfaces
open Libs.Config

let main plateau =
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

let () = main plateau


  (*  affiche position_de_depart;
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



  let roi_blanc_roque = position_de_depart.(!depart_roi_blanc) = 6 in
  let roi_noir_roque = position_de_depart.(!depart_roi_noir) = (-6) in
  let tour_blanc_petit_roque = position_de_depart.(!depart_tour_blanche_pr) = 4 in
  let tour_blanc_grand_roque = position_de_depart.(!depart_tour_blanche_gr) = 4 in
  let tour_noir_petit_roque = position_de_depart.(!depart_tour_noire_pr) = (-4) in
  let tour_noir_grand_roque = position_de_depart.(!depart_tour_noire_gr) = (-4) in
  droit_au_roque :=
  (String.contains roques 'K') && roi_blanc_roque && tour_blanc_petit_roque,
  (String.contains roques 'Q') && roi_blanc_roque && tour_blanc_grand_roque,
  (String.contains roques 'k') && roi_noir_roque && tour_noir_petit_roque,
  (String.contains roques 'q') && roi_noir_roque && tour_noir_grand_roque*)