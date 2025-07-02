(*Module récupérant les ouvertures non doublées, d'une longueur minimale, d'un fichier d'ouverture*)

open Ouvertures

let supprime_doublon biggie n=
  let table = Hashtbl.create 5000 in
  let result = ref [] in
  let detecte_ligne chaine =
    let ligne = Str.split (Str.regexp "\n") chaine in ligne
  in let tupac = detecte_ligne biggie
  in let detecte_mots chaine =
    let mots = Str.split (Str.regexp " +") chaine in mots
  in let rec ajoute liste = match liste with
    |[] -> ()
    |h::t -> Hashtbl.add table liste (if Hashtbl.mem table liste then ((Hashtbl.find table liste) + 1) else 1); if t <> [] then begin let rev = List.rev t in ajoute (h :: (List.rev (List.tl rev))) end
  in let rec expand liste = match liste with
    |[] -> []
    |h::t -> (detecte_mots h) :: expand t
  in let rec fonc1 liste = match liste with
    |[] -> ()
    |h :: t -> ajoute h; fonc1 t
  in let rec fonc2 liste = match liste with
    |[] -> ()
    |h :: t -> if ((Hashtbl.find table h) = 1 && (List.length h) > n) then result := h ::!result; fonc2 t
  in fonc1 (expand tupac);
  fonc2 (expand tupac);
  List.rev !result

let rec string_of_list liste = match liste with
  |h::t when t <> [] -> h ^ " " ^ string_of_list t
  |h::_ -> h
  |_ -> ""

let reduction chaine n =
  let x = supprime_doublon chaine n in
  let rec fonc liste = match liste with
    |[] -> ""
    |h::t -> (string_of_list h) ^ "\n" ^ fonc t
in fonc x

let f n =
  let fichier_sortie = open_out "mon_fichier.txt" in
  let ma_chaine = reduction chess_openings_exhaustif n in
  output_string fichier_sortie ma_chaine;
  close_out fichier_sortie

let main () = f 20

let () = main ()