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

let rec find_couple list = match list with
  | [] ->  (-1 , -1)
  | hd :: tl ->
     let rec fc_aux x l m = match l,m with
       | [],[] -> (-1, -1)
       | [], hd :: tl -> fc_aux hd tl l
       | hd :: tl, _ when x + hd == 2020 -> (x, hd)
       | hd :: tl, _ -> fc_aux x tl (hd::m) in
     fc_aux hd tl []
;;

let (a,b) = find_couple (parse (readfile "./input")) in a * b;;
