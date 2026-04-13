open Libs.Bitboards

let random_magic () = Int64.logand (Random.int64 Int64.max_int) (Int64.logand (Random.int64 Int64.max_int) (Random.int64 Int64.max_int))

let bishop_file = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "magic_bishop.txt"
let rook_file = open_out_gen [Open_creat; Open_text; Open_append] 0o666 "magic_rook.txt"

let compute_magic file masks blockers moves =
  let rec try_magic mask blockers moves shift =
    let magic = random_magic () in
    if population_count (Int64.logand (Int64.mul magic mask) 0xFF00000000000000L) < 6 then begin
      try_magic mask blockers moves shift
    end
    else begin
      let table = Array.make (1 lsl (64 - shift)) (-1L) in 
      let ok = ref true in
      let i = ref 0 in
      while !ok && !i < Array.length blockers do
        let index = index magic blockers.(!i) shift in
        if table.(index) = (-1L) then begin
          table.(index) <- moves.(!i);
          incr i
        end
        else if table.(index) = moves.(!i) then begin
          incr i
        end
        else begin
          ok := false
        end
      done;
      if !ok then begin
        Some magic
      end
      else begin
        None
      end
    end
  in let find_magic mask blockers moves shift =
    let rec loop () =
      match try_magic mask blockers moves shift with
        |Some magic -> magic
        |None -> loop ()
    in loop ()
  in for square = 0 to 63 do
    let square_blockers = blockers.(square) in
    let square_moves = moves.(square) in
    let shift = 64 - population_count masks.(square)
    in let magic = find_magic masks.(square) square_blockers square_moves shift
    in output_string file (Printf.sprintf "%s\n" (Int64.to_string magic));
  done;
  close_out file

let () =
  compute_magic bishop_file bishop_masks bishop_blockers bishop_moves;
  compute_magic rook_file rook_masks rook_blockers rook_moves
