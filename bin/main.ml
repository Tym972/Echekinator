open Libs.Interfaces
open Libs.Config

(*Fonction permettant de lancer une partie d'échec, joueur contre joueur, joueur contre programme, ou programme contre programme*)
let launcher =
  let mode = lire_entree "Tapez 1 si vous voulez jouez une partie, 2 si vous voulez lancer un ensemble de parties : " in
  if mode = "1" then begin
    partie_unique ()
  end
  else if mode = "2" then begin
    partie_multiple ()
  end
let main () =
  launcher;
  exit 0

let () = main ()