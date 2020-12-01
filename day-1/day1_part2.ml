let readfile path =
  let lines = ref [] in
  let ic = open_in path in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    !lines
;;

let rec parse list = match list with
  | [] -> []
  | hd :: tl -> (int_of_string hd) :: (parse tl)
;;

let rec find_triple list = match list with
  | [] ->  (-1 , -1, -1)
  | hd :: he :: tl ->
     let rec fc_aux x y l m n = match l, m, n with
       | [], [], [] -> (-1, -1, -1)
       | [], hd::tl, _ -> fc_aux x hd tl l (y::n)
       | [], [], hd :: tl -> fc_aux y hd tl l m
       | hd :: tl, _, _ when x + y + hd == 2020 -> (x, y, hd)
       | hd :: tl, _, _ -> fc_aux x y tl (hd::m) n in
                    
     fc_aux hd he tl [] []
  | _ -> (-1, -1, -1)
;;

let (a,b,c) = find_triple (parse (readfile "./input_part2.txt")) in (a,b,c,a * b * c);;
