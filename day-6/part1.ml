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

let rec size l = match l with
  | [] -> 0
  | hd::tl -> 1 + (size tl)
;;

let process list =
  let rec process_aux list acc = match list with
    | [] -> [acc]
    | hd :: tl when String.equal hd "" -> acc :: (process_aux tl [])
    | hd :: tl -> process_aux tl (hd :: acc) in
  process_aux list []
;;

let rec explode_all list =
  let rec explode_inner list = match list with
    | [] -> []
    | hd :: tl -> (explode hd) @ (explode_inner tl) in
  match list with
  | [] -> []
  | hd :: tl -> (explode_inner hd) :: (explode_all tl)
;;

let rec remove_doubles list = match list with
  | hd :: he :: tl when hd == he -> remove_doubles (he::tl)
  | hd :: tl -> hd :: (remove_doubles tl)
  | _ -> list
;;

let result = List.fold_left (fun x y -> x + y) 0 (List.map size (List.map remove_doubles (List.map (fun x -> List.sort compare x) (explode_all (process (readfile ("./input.txt"))))))) ;;
