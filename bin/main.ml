open Libs.Plateau
open Libs.Uci
open Libs.Traduction1

(*Fonction permettant de lancer le moteur*)
let launcher () =
  while true do
    let config = lire_entree "" true in
    if (try List.hd (detecte_mots config) with _ -> "") = "uci" then begin
      uci ();
      echekinator ()
    end
  done

let () = launcher ()