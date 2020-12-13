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

let rec move l pos wp =
  let move_aux mov pos wp =
    let rotate_wp mov wp = match mov.dir, mov.num with
      | _, 180 -> {x = -wp.x; y = -wp.y}
      | 'R',90 -> {x = wp.y; y = -wp.x}
      | 'L',270 -> {x = wp.y; y = -wp.x}
      | 'L',90 -> {x = -wp.y; y = wp.x}
      | 'R',270 -> {x = -wp.y; y = wp.x}
      | _ -> raise (Invalid_argument "Argument mov invalide") in
    match mov.dir with
    | 'N' -> (pos, {x=wp.x;y=wp.y + mov.num})
    | 'S' -> (pos, {x=wp.x;y=wp.y - mov.num})
    | 'E' -> (pos, {x=wp.x + mov.num;y=wp.y})
    | 'W' -> (pos, {x=wp.x - mov.num;y=wp.y})
    | 'L' -> (pos, rotate_wp mov wp)
    | 'R' -> (pos, rotate_wp mov wp)
    | 'F' -> ({x = pos.x + (mov.num * wp.x); y = pos.y + (mov.num * wp.y)}, wp)
    |_ -> raise (Invalid_argument "Argument mov.dir invalide") in
  match l with
  | [] -> pos
  | hd :: tl ->
     let (newpos, newwp) = move_aux hd pos wp in
     move tl newpos newwp
;;

let man_dist pos = abs pos.x + abs pos.y;;

man_dist (move (parse (readfile "./input.txt")) {x=0;y=0} {x=10;y=1});;
