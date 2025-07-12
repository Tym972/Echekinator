open Libs.Uci
open Libs.Of_algebraic

(*Fonction permettant de lancer le moteur*)
let launcher () =
  let config = lire_entree "" true in
  if (try List.hd (word_detection config) with _ -> "") = "uci" then begin
    uci ();
    echekinator ()
  end

let () = launcher ()