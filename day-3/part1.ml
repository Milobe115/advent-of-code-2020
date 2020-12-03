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

let slide list =
  let rec slide_aux list x = match list with
    | [] -> ([], [])
    | hd :: tl ->
       let n = String.length hd in
       let (l1, l2) = slide_aux tl ((x + 3) mod n) in
       (hd.[x] :: l1, x :: l2) in
  slide_aux list 0
;;

let rec check_trees list = match list with
  | [] -> 0
  | hd :: tl when hd == '#' -> 1 + check_trees tl
  | hd :: tl -> check_trees tl
;;

let lines = readfile "./input_part1.txt";;
let (a,b) = slide lines;;
let b = check_trees a;;
