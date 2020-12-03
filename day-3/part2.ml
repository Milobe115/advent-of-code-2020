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

let slide list nbslopes =
  let rec slide_aux list x nbs = match list with
    | [] -> []
    | hd :: tl ->
       let n = String.length hd in
       hd.[x] :: slide_aux tl ((x + nbs) mod n) nbs in
  slide_aux list 0 nbslopes
;;

let slide_2 list nbslopes =
  let rec slide_aux list x nbs = match list with
    | [] -> []
    | hd :: he :: tl ->
       let n = String.length hd in
       hd.[x] :: slide_aux tl ((x + nbs) mod n) nbs
    | _ -> [] in
  slide_aux list 0 nbslopes
;;

let rec check_trees list = match list with
  | [] -> 0
  | hd :: tl when hd == '#' -> 1 + check_trees tl
  | hd :: tl -> check_trees tl
;;

let nb1s = check_trees(slide(readfile "./input_part1.txt") 1) in
let nb3s = check_trees(slide(readfile "./input_part1.txt") 3) in
let nb5s = check_trees(slide(readfile "./input_part1.txt") 5) in
let nb7s = check_trees(slide(readfile "./input_part1.txt") 7) in
let nb2s = check_trees(slide_2(readfile "./input_part1.txt") 1) in
nb1s * nb3s * nb5s * nb7s * nb2s;;
