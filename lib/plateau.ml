(*Module implémentant le type Mouvement, les constantes et les fonctions de bases du programme*)

(*Version du programme*)
let nom_du_projet = "Echekinator"

(*Définition d'un type représentant un mouvement de pièce quelconque*)
type mouvement =
  |Roque of {sorte : int}
  |Enpassant of  {depart : int; arrivee : int}
  |Classique of {piece : int; depart : int; arrivee : int; prise : int}
  |Promotion of {depart : int; arrivee : int; promotion : int; prise : int}
  |Aucun

(*Tableau des coordonées d'un échiquier*)
let coord = [|
  "a8"; "b8"; "c8"; "d8"; "e8"; "f8"; "g8"; "h8";
  "a7"; "b7"; "c7"; "d7"; "e7"; "f7"; "g7"; "h7";
  "a6"; "b6"; "c6"; "d6"; "e6"; "f6"; "g6"; "h6";
  "a5"; "b5"; "c5"; "d5"; "e5"; "f5"; "g5"; "h5";
  "a4"; "b4"; "c4"; "d4"; "e4"; "f4"; "g4"; "h4";
  "a3"; "b3"; "c3"; "d3"; "e3"; "f3"; "g3"; "h3";
  "a2"; "b2"; "c2"; "d2"; "e2"; "f2"; "g2"; "h2";
  "a1"; "b1"; "c1"; "d1"; "e1"; "f1"; "g1"; "h1"
|]

(*Dictionnaire associant une coordonées de l'échiquier à son indice dans le tableau coord*)
let dicocoord =
  let ht = Hashtbl.create 64 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ("a8", 0);  ("b8", 1);  ("c8", 2);  ("d8", 3);  ("e8", 4);  ("f8", 5);  ("g8", 6);  ("h8", 7);
      ("a7", 8);  ("b7", 9);  ("c7", 10); ("d7", 11); ("e7", 12); ("f7", 13); ("g7", 14); ("h7", 15);
      ("a6", 16); ("b6", 17); ("c6", 18); ("d6", 19); ("e6", 20); ("f6", 21); ("g6", 22); ("h6", 23);
      ("a5", 24); ("b5", 25); ("c5", 26); ("d5", 27); ("e5", 28); ("f5", 29); ("g5", 30); ("h5", 31);
      ("a4", 32); ("b4", 33); ("c4", 34); ("d4", 35); ("e4", 36); ("f4", 37); ("g4", 38); ("h4", 39);
      ("a3", 40); ("b3", 41); ("c3", 42); ("d3", 43); ("e3", 44); ("f3", 45); ("g3", 46); ("h3", 47);
      ("a2", 48); ("b2", 49); ("c2", 50); ("d2", 51); ("e2", 52); ("f2", 53); ("g2", 54); ("h2", 55);
      ("a1", 56); ("b1", 57); ("c1", 58); ("d1", 59); ("e1", 60); ("f1", 61); ("g1", 62); ("h1", 63)];
  ht

(*Dictionnaire associant la représentation graphique des pièces à leur représentation dans le tableau-échiquier*)
let dicorespondance =
  let ht = Hashtbl.create 12 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ('t', (-4)); ('T', 4); ('c', (-2)); ('C', 2); ('f', (-3)); ('F', 3);
      ('d', (-5)); ('D', 5); ('r', (-6)); ('R', 6); ('p', (-1)); ('P', 1)];
  ht

(*Dictionnaire liant représentation affichée à l'utilisateur des roques et valeur correspondante dans les fonctions*)
let dicoroque =
  let ht = Hashtbl.create 4 in
  List.iter (fun (key, value) -> Hashtbl.add ht key value)
    [ ("PR", 1); ("GR", 2); ("pr", 3); ("gr", 4)];
  ht

(*Fonction condensant l'écriture d'un roque*)
let castle sorte =
  Roque {sorte = if (Hashtbl.mem dicoroque sorte) then (Hashtbl.find dicoroque sorte) else 0}

(*Fonction condensant l'écriture d'une prise en passant*)
let prise_en_passant (depart, arrivee) =
  Enpassant {depart = if (Hashtbl.mem dicocoord depart) then (Hashtbl.find dicocoord depart) else 0; arrivee = if (Hashtbl.mem dicocoord arrivee) then (Hashtbl.find dicocoord arrivee) else 0}

(*Fonction condensant l'écriture d'un coup classique*)
let classic (piece, depart, arrivee) =
  Classique {piece = if (Hashtbl.mem dicorespondance piece) then (Hashtbl.find dicorespondance piece) else 0; depart = if (Hashtbl.mem dicocoord depart) then (Hashtbl.find dicocoord depart) else 0; arrivee = if (Hashtbl.mem dicocoord arrivee) then (Hashtbl.find dicocoord arrivee) else 0; prise =  0}

(*Fonction condensant l'écriture d'une promotion*)
let estac (depart, arrivee, promotion) =
  Promotion {depart = if (Hashtbl.mem dicocoord depart) then (Hashtbl.find dicocoord depart) else 0; arrivee = if (Hashtbl.mem dicocoord arrivee) then (Hashtbl.find dicocoord arrivee) else 0; promotion = if (Hashtbl.mem dicorespondance promotion) then (Hashtbl.find dicorespondance promotion) else 0; prise = 0}

(*Tableau de 120 éléments, où les -1 représentent l'extérieur de l'échiquier*)
let tab120 = [| 
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1;  0;  1;  2;  3;  4;  5;  6;  7; -1;
  -1;  8;  9; 10; 11; 12; 13; 14; 15; -1;
  -1; 16; 17; 18; 19; 20; 21; 22; 23; -1;
  -1; 24; 25; 26; 27; 28; 29; 30; 31; -1;
  -1; 32; 33; 34; 35; 36; 37; 38; 39; -1;
  -1; 40; 41; 42; 43; 44; 45; 46; 47; -1;
  -1; 48; 49; 50; 51; 52; 53; 54; 55; -1;
  -1; 56; 57; 58; 59; 60; 61; 62; 63; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
  -1; -1; -1; -1; -1; -1; -1; -1; -1; -1
|]

(*Tableau de 64 éléments, les nombres correspondent à l'élément d'indice correspondant dans le tableau tab120*)
let tab64 = [| 
  21; 22; 23; 24; 25; 26; 27; 28;
  31; 32; 33; 34; 35; 36; 37; 38;
  41; 42; 43; 44; 45; 46; 47; 48;
  51; 52; 53; 54; 55; 56; 57; 58;
  61; 62; 63; 64; 65; 66; 67; 68;
  71; 72; 73; 74; 75; 76; 77; 78;
  81; 82; 83; 84; 85; 86; 87; 88;
  91; 92; 93; 94; 95; 96; 97; 98
|]

(*Directions possible de déplacement d'une tour dans le tableau tab64*)
let vect_tour = [|(-10); 10; (-1); 1|]

(*Directions possible de déplacement d'un fou dans le tableau tab64*)
let vect_fou = [|(-11); 11; (-9); 9|]

(*Déplacements possible d'un cavalier dans le tableau tab64*)
let vect_cavalier = [|(-8); 8; (-12); 12; (-19); 19; (-21); 21|]

(*Déplacements possible d'un roi dans le tableau tab64*)
let vect_roi = [|(-10); 10; (-1); 1; (-11); 11; (-9); 9|]

(*Representation d'un échiquier avant le premier coup. Les pièces sont représentées par des nombres, positif pour les blancs, négatifs pour les noirs. case vide : 0; pion : 1; cavalier : 2; fou : 3; tour : 4; dame : 5; roi : 6*)
let echiquier = [|
  (-4); (-2); (-3); (-5); (-6); (-3); (-2); (-4);
  (-1); (-1); (-1); (-1); (-1); (-1); (-1); (-1);
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0;
  1; 1; 1; 1; 1; 1; 1; 1;
  4; 2; 3; 5; 6; 3; 2; 4
  |]

let tab_affiche = [|"   |"; " P |"; " C |"; " F |"; " T |"; " D |"; " R |"; " p |"; " c |"; " f |"; " t |"; " d |"; " r |"; " X |"|]

(*Fonction permettant l'affichage d'une grille représentant l'échiquier*)
let affiche plateau =
  let affichage = ref "   +---+---+---+---+---+---+---+---+\n" in
  for i = 0 to 7 do
    let k_list = ref [] in
    let k = string_of_int (8 - i) ^ "  |" in
    for j = 8 * i to 8 + 8 * i - 1 do
      let piece = plateau.(j) in
      k_list := tab_affiche.(if piece >= 0 then piece else (6 - piece)) :: !k_list;
    done;
    k_list := List.rev !k_list;
    let k_str = String.concat "" !k_list in
    affichage := !affichage ^ (k ^ k_str ^ "\n" ^"   +---+---+---+---+---+---+---+---+\n");
  done;
  print_endline (!affichage ^ "     a   b   c   d   e   f   g   h\n")

(*Fonction renvoyant la valeur de gagne associée à une défaite du joueur*)
let defaite joueur_est_blanc = if joueur_est_blanc then (-1) else 1

(*Fonction renvoyant la représentation du roi du joueur dans le tableau-échiquier*)
let roi joueur_est_blanc = if joueur_est_blanc then 6 else (-6)

(*Fonction renvoyant la représentation d'une dame du joueur dans le tableau-échiquier*)
let dame joueur_est_blanc = if joueur_est_blanc then 5 else (-5)

(*Fonction renvoyant la représentation d'un cavalier du joueur dans le tableau-échiquier*)
let cavalier joueur_est_blanc = if joueur_est_blanc then 2 else (-2)

(*Fonction renvoyant la représentation d'un fou du joueur dans le tableau-échiquier*)
let fou joueur_est_blanc = if joueur_est_blanc then 3 else (-3)

(*Fonction renvoyant la représentation d'un pion du joueur dans le tableau-échiquier*)
let pion joueur_est_blanc = if joueur_est_blanc then 1 else (-1)

(*Fonction renvoyant la représentation d'une tour du joueur dans le tableau-échiquier*)
let tour joueur_est_blanc = if joueur_est_blanc then 4 else (-4)

let rec select liste n = match liste with
  |[] -> []
  |h::t -> if n = 0 then [] else h :: select t (n - 1)

(*Fonction renvoyant l'indice de la première occurence d'un élément dans un tableau*)
let index_tableau tableau element =
  if element < 0 then begin
    let rec aux i =
      if tableau.(i) = element then i else aux (i + 1)
    in aux 0
  end
  else begin
    let rec aux i =
      if tableau.(i) = element then i else aux (i - 1)
    in aux 63
  end

(*Tri fusion*)
let tri_fusion l =
  let rec divise l = match l with
    |[] -> [], []
    |[x] -> [x] , []
    |h::g::t -> let tg, td = divise t in h::tg, g::td
  in let rec fusionne l1 l2 = match (l1, l2) with
    |[], l | l, [] -> l
    |h1 :: t1, h2 :: t2 -> if h1 >= h2 then h1 :: fusionne t1 l2 else h2 :: fusionne t2 l1
  in let rec tri_f l = match l with
    |[] | [_] -> l
    |_ -> let lg, ld = divise l in fusionne (tri_f lg) (tri_f ld)
  in tri_f l