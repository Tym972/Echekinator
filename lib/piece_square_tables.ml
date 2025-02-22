(*Module des piece square tables*)

let tab_pion_blanc = 
  [| 0;  0;  0;  0;  0;  0;  0;  0;
    50; 50; 50; 50; 50; 50; 50; 50;
    10; 10; 20; 30; 30; 20; 10; 10;
     5;  5; 10; 25; 25; 10;  5;  5;
     0;  0;  0; 20; 20;  0;  0;  0;
     5; -5;-10;  0;  0;-10; -5;  5;
     5; 10; 10;-20;-20; 10; 10;  5;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_pion_noir =
  [| 0;  0;  0;  0;  0;  0;  0;  0;
     5; 10; 10;-20;-20; 10; 10;  5;
     5; -5;-10;  0;  0;-10; -5;  5;
     0;  0;  0; 20; 20;  0;  0;  0;
     5;  5; 10; 25; 25; 10;  5;  5;
    10; 10; 20; 30; 30; 20; 10; 10;
    50; 50; 50; 50; 50; 50; 50; 50;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_chevalier_blanc = 
  [|-50;-40;-30;-30;-30;-30;-40;-50;
    -40;-20;  0;  0;  0;  0;-20;-40;
    -30;  0; 10; 15; 15; 10;  0;-30;
    -30;  5; 15; 20; 20; 15;  5;-30;
    -30;  0; 15; 20; 20; 15;  0;-30;
    -30;  5; 10; 15; 15; 10;  5;-30;
    -40;-20;  0;  5;  5;  0;-20;-40;
    -50;-40;-30;-30;-30;-30;-40;-50;
  |]

let tab_chevalier_noir =
  [|-50;-40;-30;-30;-30;-30;-40;-50;
    -40;-20;  0;  5;  5;  0;-20;-40;
    -30;  5; 10; 15; 15; 10;  5;-30;
    -30;  0; 15; 20; 20; 15;  0;-30;
    -30;  5; 15; 20; 20; 15;  5;-30;
    -30;  0; 10; 15; 15; 10;  0;-30;
    -40;-20;  0;  0;  0;  0;-20;-40;
    -50;-40;-30;-30;-30;-30;-40;-50;
  |]

let tab_fou_blanc =
  [|-20;-10;-10;-10;-10;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5; 10; 10;  5;  0;-10;
    -10;  5;  5; 10; 10;  5;  5;-10;
    -10;  0; 10; 10; 10; 10;  0;-10;
    -10; 10; 10; 10; 10; 10; 10;-10;
    -10;  5;  0;  0;  0;  0;  5;-10;
    -20;-10;-10;-10;-10;-10;-10;-20;
  |]

let tab_fou_noir =
  [|-20;-10;-10;-10;-10;-10;-10;-20;
    -10;  5;  0;  0;  0;  0;  5;-10;
    -10; 10; 10; 10; 10; 10; 10;-10;
    -10;  0; 10; 10; 10; 10;  0;-10;
    -10;  5;  5; 10; 10;  5;  5;-10;
    -10;  0;  5; 10; 10;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10;-10;-10;-10;-10;-20
  |]

let tab_tour_blanche_ouverture =
  [| 10; 0;  0; 10; 10;  0;  0; 10;
     5; 10; 10; 10; 10; 10; 10;  5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     0;  0;  0;  5;  5;  0;  0;  0;
  |]

let tab_tour_noire_ouverture =
  [| 0;  0;  0;  5;  5;  0;  0;  0;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     5; 10; 10; 10; 10; 10; 10;  5;
     10; 0; 0;  10; 10;  0;  0;  10
  |]

let tab_tour_blanche_mdg =
  [| 0;  0;  0;  0;  0;  0;  0;  0;
     5; 10; 10; 10; 10; 10; 10;  5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     0;  0;  0;  5;  5;  0;  0;  0;
  |]

let tab_tour_noire_mdg =
  [| 0;  0;  0;  5;  5;  0;  0;  0;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
    -5;  0;  0;  0;  0;  0;  0; -5;
     5; 10; 10; 10; 10; 10; 10;  5;
     0;  0;  0;  0;  0;  0;  0;  0
  |]

let tab_dame_blanche_ouverture =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5;  5;  5;  5;  0;-10;
     -5;  0;  5;  5;  5;  5;  0; -5;
      0;  0;  5;  5;  5;  5;  0; -5;
    -10;  5;  5;  5;  5;  5;  0;-10;
    -10;  0;  5;  0;  0;  0;  0;-10;
    -20;-10;-10; 20; -5;-10;-10;-20;
  |]

let tab_dame_noire_ouverture =
  [|-20;-10;-10; -5; 20;-10;-10;-20;
    -10;  0;  0;  0;  0;  5;  0;-10;
    -10;  0;  5;  5;  5;  5;  5;-10;
     -5;  0;  5;  5;  5;  5;  0;  0;
     -5;  0;  5;  5;  5;  5;  0; -5;
    -10;  0;  5;  5;  5;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20
  |]

let tab_dame_blanche_mdg =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -10;  0;  5;  5;  5;  5;  0;-10;
     -5;  0;  5;  5;  5;  5;  0; -5;
      0;  0;  5;  5;  5;  5;  0; -5;
    -10;  5;  5;  5;  5;  5;  0;-10;
    -10;  0;  5;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20;
  |]

let tab_dame_noire_mdg =
  [|-20;-10;-10; -5; -5;-10;-10;-20;
    -10;  0;  0;  0;  0;  5;  0;-10;
    -10;  0;  5;  5;  5;  5;  5;-10;
     -5;  0;  5;  5;  5;  5;  0;  0;
     -5;  0;  5;  5;  5;  5;  0; -5;
    -10;  0;  5;  5;  5;  5;  0;-10;
    -10;  0;  0;  0;  0;  0;  0;-10;
    -20;-10;-10; -5; -5;-10;-10;-20
  |]

let tab_roi_blanc_mdg =
  [|-30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -20;-30;-30;-40;-40;-30;-30;-20;
    -10;-20;-20;-20;-20;-20;-20;-10;
     20; 20;  0;  0;  0;  0; 20; 20;
     20; 30; 10;  0;  0; 10; 30; 20
  |]

let tab_roi_noir_mdg =
  [| 20; 30; 10;  0;  0; 10; 30; 20;
     20; 20;  0;  0;  0;  0; 20; 20;
    -10;-20;-20;-20;-20;-20;-20;-10;
    -20;-30;-30;-40;-40;-30;-30;-20;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30;
    -30;-40;-40;-50;-50;-40;-40;-30
  |]

let tab_roi_blanc_finale =
  [|-50;-40;-30;-20;-20;-30;-40;-50;
    -30;-20;-10;  0;  0;-10;-20;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-30;  0;  0;  0;  0;-30;-30;
    -50;-30;-30;-30;-30;-30;-30;-50
  |]

let tab_roi_noir_finale =
  [|-50;-30;-30;-30;-30;-30;-30;-50;
    -30;-30;  0;  0;  0;  0;-30;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 30; 40; 40; 30;-10;-30;
    -30;-10; 20; 30; 30; 20;-10;-30;
    -30;-20;-10;  0;  0;-10;-20;-30;
    -50;-40;-30;-20;-20;-30;-40;-50|]

let tab_pieces_blanches_ouverture = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_mdg|]

let tab_pieces_blanches_mdg = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_mdg|]

let tab_pieces_blanches_finale = [|tab_pion_blanc; tab_chevalier_blanc; tab_fou_blanc; tab_tour_blanche_mdg; tab_dame_blanche_mdg; tab_roi_blanc_finale|]

let tab_pieces_noires_ouverture = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_mdg|]

let tab_pieces_noires_mdg = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_mdg|]

let tab_pieces_noires_finale = [|tab_pion_noir; tab_chevalier_noir; tab_fou_noir; tab_tour_noire_mdg; tab_dame_noire_mdg; tab_roi_noir_finale|]

let tab_ouverture = tab_pieces_blanches_ouverture, tab_pieces_noires_ouverture

let tab_mdg = tab_pieces_blanches_mdg, tab_pieces_noires_mdg

let tab_finale = tab_pieces_blanches_finale, tab_pieces_noires_finale