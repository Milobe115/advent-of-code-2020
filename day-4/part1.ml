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

let rec size list = match list with
  | [] -> 0
  | hd :: tl -> 1 + size tl
;;

let parse list =
  let rec remove_blanks list = match list with
    | [] -> []
    | hd :: tl when (String.length hd) == 0 -> remove_blanks tl
    | hd :: tl -> hd :: remove_blanks tl in
  let split = fun str -> String.split_on_char ' ' str in
  let rec parse_aux list string =
    match list with
    | [] when string == ""-> []
    | [] ->  [string]
    | hd :: tl when (String.length hd) == 0 -> string :: (parse_aux tl "")
    | hd :: tl -> parse_aux tl (string ^ " " ^ hd) in
  List.map remove_blanks (List.map split (parse_aux list ""))
;;

let rec process list = match list with
  | [] -> 0
  | hd :: tl when (size hd) == 8 -> 1 + process tl
  | hd :: tl when (size hd) <= 6 -> process tl
  | hd :: tl ->
     let cid = fun str -> BatString.exists str "cid" in
     let has_cid = List.fold_left (fun x y -> x || y) false (List.map cid hd) in
     if has_cid
     then process tl
     else 1 + process tl
;;

let value = process (parse (readfile "./input.txt"));
