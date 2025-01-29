open Libs.Interfaces
open Libs.Config

(*Fonction permettant de lancer une partie d'échec, joueur contre joueur, joueur contre programme, ou programme contre programme*)
let launcher =
  print_newline ();
  print_string "Tapez 1 si vous voulez jouez une partie, 2 si vous voulez lancer un ensemble de parties : ";
  flush stdout;
  let mode = input_line stdin in
  if mode = "1" then begin
    partie_unique ()
  end
  else if mode = "2" then begin
    print_string "Voulez vous tester chaque ouverture du répertoire désigné? : ";
    flush stdout;
    let all_openings = if est_oui (input_line stdin)then true else false in
    let nombre = ref 0 in
    let anti_repet = ref false in
    if not all_openings then begin
      print_string "Tapez le nombre de parties désirées : ";
      flush stdout;
      nombre := (try int_of_string (input_line stdin) with _ -> exit 0);
      print_string "Voulez-vous prévenir l'utilisation d'une même ouverture plusieurs fois? : ";
      flush stdout;
      if est_oui (input_line stdin) then begin 
        anti_repet := true 
      end
    end;
    print_string "Voulez vous afficher les coups? : ";
    flush stdout;
    let affichage = est_oui (input_line stdin) in
    ensemble all_openings !anti_repet !nombre affichage
  end
let main () =
  launcher;
  exit 0

let () = main ()