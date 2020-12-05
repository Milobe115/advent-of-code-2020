let readfile path =
  let lines = ref [] in
  let ic = open_in path in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines
;;

let explode s =
  let rec exp i l = match i with
    | -1 -> l 
    | n -> exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
;;

let rec process list =
  let rank string =
    let rec rank_aux iter row rows col cols = match iter with
      | [] -> row * 8 + col
      | hd :: tl when hd == 'F' -> rank_aux tl row (rows / 2) col cols
      | hd :: tl when hd == 'B' -> rank_aux tl (row + rows) (rows / 2) col cols
      | hd :: tl when hd == 'L' -> rank_aux tl row rows col (cols / 2)
      | hd :: tl when hd == 'R' -> rank_aux tl row rows (col + cols) (cols / 2)
      | _ -> 0 in
    rank_aux (explode string) 0 64 0 4 in
  match list with
  | [] -> 0
  | hd :: tl -> max (rank hd) (process tl)
;;

process (readfile "./input.txt");;

     
