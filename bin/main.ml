open Libs.Plateau
open Libs.Interfaces
open Libs.Config
open Libs.Uci
open Libs.Traduction1


let config_j1 = config_pf_quiescent 4
let config_j2 = config_pf_quiescent 4

(*Fonction permettant de lancer une partie d'échec, joueur contre joueur, joueur contre programme, ou programme contre programme*)
let launcher () =
  let config = lire_entree "" true in
  if (try List.hd (detecte_mots config) with _ -> "") = "uci" then begin
    uci ();
    echekinator ()
  end
  else begin
    let mode = lire_entree "Tapez 1 si vous voulez jouez une partie, 2 si vous voulez lancer un ensemble de parties : " true in
    if mode = "1" then begin
      partie_unique ()
    end
    else if mode = "2" then begin
      partie_multiple config_j1 config_j2
    end
  end

let () = launcher ()