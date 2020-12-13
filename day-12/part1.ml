type movement = {dir : char; num: int};;
type pos = {x: int; y: int};;

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

let rec parse l = match l with
  | [] -> []
  | hd :: tl -> {dir=hd.[0]; num=int_of_string (String.sub hd 1 ((String.length hd) - 1))} :: parse tl
;;

let find_new_dir mov dir = match mov.dir, mov.num, dir with
  | 'R',90,'N' | 'R',180, 'W' | 'R',270,'S' | 'L',90,'S' | 'L',180,'W' | 'L',270,'N' -> 'E'
  | 'R',90,'E' | 'R',180, 'N' | 'R',270,'W' | 'L',90,'W' | 'L',180,'N' | 'L',270,'E' -> 'S'
  | 'R',90,'S' | 'R',180, 'E' | 'R',270,'N' | 'L',90,'N' | 'L',180,'E' | 'L',270,'S' -> 'W'
  | 'R',90,'W' | 'R',180, 'S' | 'R',270,'E' | 'L',90,'E' | 'L',180,'S' | 'L',270,'W' -> 'N'
  | _ -> raise (Invalid_argument "args invalide")
;;

let rotate_aux mov dir =
  match mov.dir with
  | 'F' -> ({dir = dir; num = mov.num}, dir)
  | 'R' | 'L' ->
     let newdir = find_new_dir mov dir in
     ({dir= newdir; num = 0}, newdir)
  | _ -> (mov, dir)
;;

let rec rotate l dir = match l with
  | [] -> []
  | hd :: tl ->
     let (mov, dir) = rotate_aux hd dir in
     mov :: (rotate tl dir)
;;

let rec move l pos =
  let move_aux mov pos = match mov.dir with
    |'N' -> {x=pos.x;y=pos.y + mov.num}
    |'S' -> {x=pos.x;y=pos.y - mov.num}
    |'E' -> {x=pos.x + mov.num;y=pos.y}
    |'W' -> {x=pos.x - mov.num;y=pos.y}
    |_ -> raise (Invalid_argument "Argument mov.dir invalide") in
  match l with
  | [] -> pos
  | hd :: tl -> move tl (move_aux hd pos)
;;

let man_dist pos = abs pos.x + abs pos.y;;

man_dist (move (rotate (parse (readfile "./input.txt")) 'E') {x=0;y=0});;
