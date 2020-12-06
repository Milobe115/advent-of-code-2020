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
  let rec process_aux list acc n = match list with
    | [] -> [n, acc]
    | hd :: tl when String.equal hd "" -> (n, acc) :: (process_aux tl [] 0)
    | hd :: tl -> process_aux tl (hd :: acc) (n + 1) in
  process_aux list [] 0
;;

let rec explode_all list =
  let rec explode_inner list = match list with
    | [] -> []
    | hd :: tl -> (explode hd) @ (explode_inner tl) in
  match list with
  | [] -> []
  | (n,hd) :: tl -> (n,explode_inner hd) :: (explode_all tl)
;;

let rec count list =
  let rec count_aux list n n0 c =
    let () = print_string (">>> " ^ (string_of_int n) ^ " " ^ (string_of_int n0) ^ " " ^ (Char.escaped c) ^ "\n") in
    match list, n with
    | [], _ -> 0
    | hd::tl, 1 ->
       let () = print_string "Jaaaj \n" in
       1 + count_aux tl n0 n0 hd
    | hd::tl, x when hd == c -> count_aux tl (n - 1) n0 c
    | hd::tl, x -> count_aux tl n0 n0 hd
  in match list with
  | [] -> []
  | (n,hd) :: tl -> count_aux hd n n (List.hd hd) :: (count tl)
;;

let processed = process (readfile ("./input.txt"));;
let exploded = explode_all processed;;
let sorted = List.map (fun (n,x) -> (n, List.sort compare x)) exploded ;;
let counted = count sorted;;
let result = List.fold_left (fun x y -> x + y) 0 counted;;
