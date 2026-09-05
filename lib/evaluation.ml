open Bitboards
open Miscellaneous let _ = print_board
(*Module implémentant des fonctions d'évaluation*)

let distance from to_ =
  max (abs ((from land 7) - (to_ land 7))) (abs ((from / 8) - (to_ / 8)))

let ahead = Array.init 128
  (fun i ->
    let file = (i mod 64) land 7 in
    let rank = (i mod 64) / 8 in
    let bitboard = ref 0L in
    let acc = ref 0L in
    if i < 64 then begin
      for j = rank + 1 to 7 do
        acc := !acc ||| ranks.(j)
      done
    end
    else begin
      for j = rank - 1 downto 0 do
        acc := !acc ||| ranks.(j)
      done
    end;
    for j = max 0 (file - 1) to min 7 (file + 1) do
      bitboard := !bitboard ||| (!acc &&& files.(j)) 
    done;
    !bitboard
  )

let mg_pst = [| 82; 82; 82; 82; 82; 82; 82; 82; 47; 81; 62; 59; 67; 106; 120; 60; 56; 78; 78; 72; 85; 85; 115; 70; 55; 80; 77; 94; 99; 88; 92; 57; 68; 95; 88; 103; 105; 94; 99; 59; 76; 89; 108; 113; 147; 138; 107; 62; 180; 216; 143; 177; 150; 208; 116; 71; 82; 82; 82; 82; 82; 82; 82; 82; 232; 316; 279; 304; 320; 309; 318; 314; 308; 284; 325; 334; 336; 355; 323; 318; 314; 328; 349; 347; 356; 354; 362; 321; 324; 341; 353; 350; 365; 356; 358; 329; 328; 354; 356; 390; 374; 406; 355; 359; 290; 397; 374; 402; 421; 466; 410; 381; 264; 296; 409; 373; 360; 399; 344; 320; 170; 248; 303; 288; 398; 240; 322; 230; 332; 362; 351; 344; 352; 353; 326; 344; 369; 380; 381; 365; 372; 386; 398; 366; 365; 380; 380; 380; 379; 392; 383; 375; 359; 378; 378; 391; 399; 377; 375; 369; 361; 370; 384; 415; 402; 402; 372; 363; 349; 402; 408; 405; 400; 415; 402; 363; 339; 381; 347; 352; 395; 424; 383; 318; 336; 369; 283; 328; 340; 323; 372; 357; 458; 464; 478; 494; 493; 484; 440; 451; 433; 461; 457; 468; 476; 488; 471; 406; 432; 452; 461; 460; 480; 477; 472; 444; 441; 451; 465; 476; 486; 470; 483; 454; 453; 466; 484; 503; 501; 512; 469; 457; 472; 496; 503; 513; 494; 522; 538; 493; 504; 509; 535; 539; 557; 544; 503; 521; 509; 519; 509; 528; 540; 486; 508; 520; 1024; 1007; 1016; 1035; 1010; 1000; 994; 975; 990; 1017; 1036; 1027; 1033; 1040; 1022; 1026; 1011; 1027; 1014; 1023; 1020; 1027; 1039; 1030; 1016; 999; 1016; 1015; 1023; 1021; 1028; 1022; 998; 998; 1009; 1009; 1024; 1042; 1023; 1026; 1012; 1008; 1032; 1033; 1054; 1081; 1072; 1082; 1001; 986; 1020; 1026; 1009; 1082; 1053; 1079; 997; 1025; 1054; 1037; 1084; 1069; 1068; 1070; -15; 36; 12; -54; 8; -28; 24; 14; 1; 7; -8; -64; -43; -16; 9; 8; -14; -14; -22; -46; -44; -30; -15; -27; -49; -1; -27; -39; -46; -44; -33; -51; -17; -20; -12; -27; -30; -25; -14; -36; -9; 24; 2; -16; -20; 6; 22; -22; 29; -1; -20; -7; -8; -4; -38; -29; -65; 23; 16; -15; -56; -34; 2; 13; 82; 82; 82; 82; 82; 82; 82; 82; 180; 216; 143; 177; 150; 208; 116; 71; 76; 89; 108; 113; 147; 138; 107; 62; 68; 95; 88; 103; 105; 94; 99; 59; 55; 80; 77; 94; 99; 88; 92; 57; 56; 78; 78; 72; 85; 85; 115; 70; 47; 81; 62; 59; 67; 106; 120; 60; 82; 82; 82; 82; 82; 82; 82; 82; 170; 248; 303; 288; 398; 240; 322; 230; 264; 296; 409; 373; 360; 399; 344; 320; 290; 397; 374; 402; 421; 466; 410; 381; 328; 354; 356; 390; 374; 406; 355; 359; 324; 341; 353; 350; 365; 356; 358; 329; 314; 328; 349; 347; 356; 354; 362; 321; 308; 284; 325; 334; 336; 355; 323; 318; 232; 316; 279; 304; 320; 309; 318; 314; 336; 369; 283; 328; 340; 323; 372; 357; 339; 381; 347; 352; 395; 424; 383; 318; 349; 402; 408; 405; 400; 415; 402; 363; 361; 370; 384; 415; 402; 402; 372; 363; 359; 378; 378; 391; 399; 377; 375; 369; 365; 380; 380; 380; 379; 392; 383; 375; 369; 380; 381; 365; 372; 386; 398; 366; 332; 362; 351; 344; 352; 353; 326; 344; 509; 519; 509; 528; 540; 486; 508; 520; 504; 509; 535; 539; 557; 544; 503; 521; 472; 496; 503; 513; 494; 522; 538; 493; 453; 466; 484; 503; 501; 512; 469; 457; 441; 451; 465; 476; 486; 470; 483; 454; 432; 452; 461; 460; 480; 477; 472; 444; 433; 461; 457; 468; 476; 488; 471; 406; 458; 464; 478; 494; 493; 484; 440; 451; 997; 1025; 1054; 1037; 1084; 1069; 1068; 1070; 1001; 986; 1020; 1026; 1009; 1082; 1053; 1079; 1012; 1008; 1032; 1033; 1054; 1081; 1072; 1082; 998; 998; 1009; 1009; 1024; 1042; 1023; 1026; 1016; 999; 1016; 1015; 1023; 1021; 1028; 1022; 1011; 1027; 1014; 1023; 1020; 1027; 1039; 1030; 990; 1017; 1036; 1027; 1033; 1040; 1022; 1026; 1024; 1007; 1016; 1035; 1010; 1000; 994; 975; -65; 23; 16; -15; -56; -34; 2; 13; 29; -1; -20; -7; -8; -4; -38; -29; -9; 24; 2; -16; -20; 6; 22; -22; -17; -20; -12; -27; -30; -25; -14; -36; -49; -1; -27; -39; -46; -44; -33; -51; -14; -14; -22; -46; -44; -30; -15; -27; 1; 7; -8; -64; -43; -16; 9; 8; -15; 36; 12; -54; 8; -28; 24; 14|]
let eg_pst = [| 94; 94; 94; 94; 94; 94; 94; 94; 107; 102; 102; 104; 107; 94; 96; 87; 98; 101; 88; 95; 94; 89; 93; 86; 107; 103; 91; 87; 87; 86; 97; 93; 126; 118; 107; 99; 92; 98; 111; 111; 188; 194; 179; 161; 150; 147; 176; 178; 272; 267; 252; 228; 241; 226; 259; 281; 94; 94; 94; 94; 94; 94; 94; 94; 252; 230; 258; 266; 259; 263; 231; 217; 239; 261; 271; 276; 279; 261; 258; 237; 258; 278; 280; 296; 291; 278; 261; 259; 263; 275; 297; 306; 297; 298; 285; 263; 264; 284; 303; 303; 303; 292; 289; 263; 257; 261; 291; 290; 280; 272; 262; 240; 256; 273; 256; 279; 272; 256; 257; 229; 223; 243; 268; 253; 250; 254; 218; 182; 274; 288; 274; 292; 288; 281; 292; 280; 283; 279; 290; 296; 301; 288; 282; 270; 285; 294; 305; 307; 310; 300; 290; 282; 291; 300; 310; 316; 304; 307; 294; 288; 294; 306; 309; 306; 311; 307; 300; 299; 299; 289; 297; 296; 295; 303; 297; 301; 289; 293; 304; 285; 294; 284; 293; 283; 283; 276; 286; 289; 290; 288; 280; 273; 503; 514; 515; 511; 507; 499; 516; 492; 506; 506; 512; 514; 503; 503; 501; 509; 508; 512; 507; 511; 505; 500; 504; 496; 515; 517; 520; 516; 507; 506; 504; 501; 516; 515; 525; 513; 514; 513; 511; 514; 519; 519; 519; 517; 516; 509; 507; 509; 523; 525; 525; 523; 509; 515; 520; 515; 525; 522; 530; 527; 524; 524; 520; 517; 903; 908; 914; 893; 931; 904; 916; 895; 914; 913; 906; 920; 920; 913; 900; 904; 920; 909; 951; 942; 945; 953; 946; 941; 918; 964; 955; 983; 967; 970; 975; 959; 939; 958; 960; 981; 993; 976; 993; 972; 916; 942; 945; 985; 983; 971; 955; 945; 919; 956; 968; 977; 994; 961; 966; 936; 927; 958; 958; 963; 963; 955; 946; 956; -53; -34; -21; -11; -28; -14; -24; -43; -27; -11; 4; 13; 14; 4; -5; -17; -19; -3; 11; 21; 23; 16; 7; -9; -18; -4; 21; 24; 27; 23; 9; -11; -8; 22; 24; 27; 26; 33; 26; 3; 10; 17; 23; 15; 20; 45; 44; 13; -12; 17; 14; 17; 17; 38; 23; 11; -74; -35; -18; -18; -11; 15; 4; -17; 94; 94; 94; 94; 94; 94; 94; 94; 272; 267; 252; 228; 241; 226; 259; 281; 188; 194; 179; 161; 150; 147; 176; 178; 126; 118; 107; 99; 92; 98; 111; 111; 107; 103; 91; 87; 87; 86; 97; 93; 98; 101; 88; 95; 94; 89; 93; 86; 107; 102; 102; 104; 107; 94; 96; 87; 94; 94; 94; 94; 94; 94; 94; 94; 223; 243; 268; 253; 250; 254; 218; 182; 256; 273; 256; 279; 272; 256; 257; 229; 257; 261; 291; 290; 280; 272; 262; 240; 264; 284; 303; 303; 303; 292; 289; 263; 263; 275; 297; 306; 297; 298; 285; 263; 258; 278; 280; 296; 291; 278; 261; 259; 239; 261; 271; 276; 279; 261; 258; 237; 252; 230; 258; 266; 259; 263; 231; 217; 283; 276; 286; 289; 290; 288; 280; 273; 289; 293; 304; 285; 294; 284; 293; 283; 299; 289; 297; 296; 295; 303; 297; 301; 294; 306; 309; 306; 311; 307; 300; 299; 291; 300; 310; 316; 304; 307; 294; 288; 285; 294; 305; 307; 310; 300; 290; 282; 283; 279; 290; 296; 301; 288; 282; 270; 274; 288; 274; 292; 288; 281; 292; 280; 525; 522; 530; 527; 524; 524; 520; 517; 523; 525; 525; 523; 509; 515; 520; 515; 519; 519; 519; 517; 516; 509; 507; 509; 516; 515; 525; 513; 514; 513; 511; 514; 515; 517; 520; 516; 507; 506; 504; 501; 508; 512; 507; 511; 505; 500; 504; 496; 506; 506; 512; 514; 503; 503; 501; 509; 503; 514; 515; 511; 507; 499; 516; 492; 927; 958; 958; 963; 963; 955; 946; 956; 919; 956; 968; 977; 994; 961; 966; 936; 916; 942; 945; 985; 983; 971; 955; 945; 939; 958; 960; 981; 993; 976; 993; 972; 918; 964; 955; 983; 967; 970; 975; 959; 920; 909; 951; 942; 945; 953; 946; 941; 914; 913; 906; 920; 920; 913; 900; 904; 903; 908; 914; 893; 931; 904; 916; 895; -74; -35; -18; -18; -11; 15; 4; -17; -12; 17; 14; 17; 17; 38; 23; 11; 10; 17; 23; 15; 20; 45; 44; 13; -8; 22; 24; 27; 26; 33; 26; 3; -18; -4; 21; 24; 27; 23; 9; -11; -19; -3; 11; 21; 23; 16; 7; -9; -27; -11; 4; 13; 14; 4; -5; -17; -53; -34; -21; -11; -28; -14; -24; -43|]

let gamephase_table = [|0; 1; 1; 2; 4; 0|]

let mob_center = [|  0; -4; -6; -6; -13; 0|]
let mob_step_mg = [| 0; 0; 5; 3;  2; 0|]
let mob_step_eg = [| 0; 1; 5; 3;  5; 0|]

let shield_value =  [| 0; 27; 16; 4; -5; 12; 51; -21 |]
let hidden_squares = [| 0xE7L; 0xE700000000000000L|]

let pp_mg = [| 0; 7; -6; -7; 8; 40; 106; 0|]
let pp_eg = [| 0; -14; 1; 34; 62; 98; 57; 0|]
let pp_blocked_mg = [| 0; -7; -20; -7; 14; 61; 117; 0|]
let pp_blocked_eg = [| 0; -4; 2; 9; 2; -7; -60; 0|]

let hce position =
  let mg_score = ref 0 in
  let eg_score = ref 0  in
  let gamephase = ref 0 in
  let pieces_bitboard = position.pieces in
  let occupancy = position.occupancy in
  let total_occupancy = occupancy.(0) ||| occupancy.(1) in
  let not_occupancy = Array.map (fun bitboard -> Int64.lognot bitboard) occupancy in
  let [@inline] add_score mg eg sign =
    mg_score := !mg_score + sign * mg;
    eg_score := !eg_score + sign * eg
  in let [@inline] mobility piece from color = match piece with
    |1 -> population_count ((generate_knight_attacks from) &&& not_occupancy.(color))
    |2 -> population_count ((generate_bishop_attacks from total_occupancy) &&& not_occupancy.(color))
    |3 -> population_count ((generate_rook_attacks from total_occupancy) &&& not_occupancy.(color))
    |4 -> population_count ((generate_queen_attacks from total_occupancy) &&& not_occupancy.(color))
    |_ -> 0
  in let [@inline] evaluate_pieces piece piece_type sign =
    let bitboard = ref pieces_bitboard.(piece) in
    while !bitboard <> 0L do
      let from, other_pieces_bitboard = pop_lsb !bitboard in
      bitboard := other_pieces_bitboard;
      let index =  (if sign > 0 then from else from lxor 56)  + 64 * (piece - (if sign > 0 then 1 else 7)) in
      add_score mg_pst.(index) eg_pst.(index) sign;
      gamephase := !gamephase + gamephase_table.(piece_type);
      let mob = mob_center.(piece_type) + mobility piece_type from (if sign > 0 then 0 else 1) in
      add_score (mob_step_mg.(piece_type) * mob) (mob_step_eg.(piece_type) * mob) sign
    done
  in for piece = 1 to 6 do
    evaluate_pieces piece (piece - 1) 1
  done;
  for piece = 7 to 12 do
    evaluate_pieces piece (piece - 7) (-1)
  done;
  for side = 0 to 1 do
    let pawns_bitboard = pieces_bitboard.(pawn + 6 * side) in
    let file_pawns_tab = Array.init 8 (fun file -> files.(file) &&& pawns_bitboard) in
    if population_count pieces_bitboard.(bishop + 6 * side) > 1 then begin
      add_score 30 60 (-2 * side + 1)
    end;
    if pieces_bitboard.(king + 6 * side) &&& hidden_squares.(side) <> 0L then begin
      let shield = ref 0 in
      let king_file = lsb_index (pieces_bitboard.(king + 6 * side)) land 7 in
      let min_file = max 0 (king_file - 1) in
      let max_file = min 7 (king_file + 1) in
      for file = min_file to max_file do
        let file_pawns = file_pawns_tab.(file) in
        if file_pawns <> 0L then begin
          let rank =
            if side = 0 then
              (lsb_index file_pawns) / 8
            else begin 
              7 - (msb_index file_pawns / 8) end
          in shield := !shield + shield_value.(rank)
        end
        else begin
          shield := !shield + shield_value.(7);
          if files.(file) &&& pieces_bitboard.(pawn + 6 * (side lxor 1)) = 0L then begin
            shield := !shield - 5
          end
        end
      done;
      mg_score := !mg_score + (-2 * side + 1) * !shield;
    end;
    let rooks_bitboard = ref pieces_bitboard.(rook + 6 * side) in
    while !rooks_bitboard <> 0L do
      let from, other_rook = pop_lsb !rooks_bitboard in
      rooks_bitboard := other_rook;
      let file = from land 7 in
      if file_pawns_tab.(file) = 0L then begin
        if pieces_bitboard.(pawn + 6 * (side lxor 1)) &&& files.(file) = 0L then begin
          add_score 44 (-3) (-2 * side + 1)
        end
        else begin
          add_score 21 6 (-2 * side + 1)
        end
      end
    done;
    for file = 0 to 7 do
      let file_pawns = file_pawns_tab.(file) in
      let pawn_number = population_count (file_pawns) in
      if pawn_number > 0 then begin
        if pawn_number > 1 then begin
          add_score ((pawn_number - 1) * (-12)) ((pawn_number - 1) * (-20)) (-2 * side + 1)
        end;
        if (file = 0 || file_pawns_tab.(file - 1) = 0L) && (file = 7 || file_pawns_tab.(file + 1) = 0L) then begin
          add_score (-15) (-12) (-2 * side + 1)
        end
      end
    done;
    let bb = ref pawns_bitboard in
    while !bb <> 0L do
      let from, other_pawns = pop_lsb !bb in
      bb := other_pawns;
      let file = from land 7 in
      let rank =
        if side = 0 then
          from / 8
        else begin 
          7 - (from / 8) end 
      in if (ahead.(from + 64 * side) &&& pieces_bitboard.(pawn + 6 * (side lxor 1)) = 0L) then begin
        let blocked = occupancy.(side lxor 1) &&& (ahead.(from + 64 * side) &&& files.(file)) <> 0L in
        let bonus_mg = ref (if blocked then pp_blocked_mg.(rank) else pp_mg.(rank)) in
        let bonus_eg = ref (if blocked then pp_blocked_eg.(rank) else pp_eg.(rank)) in
        bonus_eg := !bonus_eg + 12 * (distance from (lsb_index pieces_bitboard.(king + 6 * (side lxor 1))) - distance from (lsb_index pieces_bitboard.(king + 6 * side)));
        add_score !bonus_mg !bonus_eg (-2 * side + 1)
      end
    done
  done;
  let phase = min !gamephase 24 in
  (- 2 * position.white_to_move + 1) * ((!mg_score * phase + !eg_score * (24 - phase)) / 24)