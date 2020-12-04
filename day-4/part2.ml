#load "str.cma"

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

let ctrl_data list =
  let rec ctrl_aux list =
    let ctrl_aux_rec str =
      match (String.sub str 0 3) with
      | "byr" ->
         let regex = Str.regexp "byr:\\([0-9]+\\)" in
         let value = Str.replace_first regex "\\1" str in
         let byr = int_of_string value in
         (byr <= 2002) && (byr >= 1920)
      | "iyr" ->
         let regex = Str.regexp "iyr:\\([0-9]+\\)" in
         let value = Str.replace_first regex "\\1" str in
         let iyr = int_of_string value in
         (iyr >= 2010) && (iyr <= 2020)
      | "eyr" -> 
         let regex = Str.regexp "eyr:\\([0-9]+\\)" in
         let value = Str.replace_first regex "\\1" str in
         let eyr = int_of_string value in
         (eyr >= 2020) && (eyr <= 2030) 
      | "ecl" ->
         BatString.exists str "amb" || BatString.exists str "blu" || BatString.exists str "brn" || BatString.exists str "gry" || BatString.exists str "grn" || BatString.exists str "hzl" || BatString.exists str "oth" 
      | "hcl" ->
         let regex = Str.regexp "hcl:#\\([A-Fa-f0-9]+\\)" in
         let value = Str.replace_first regex "\\1" str in
         (String.length value) == 6
      | "hgt" ->
         let regex = Str.regexp "hgt:\\([0-9]+\\)\\(cm\\|in\\|\\)" in
         let value = Str.replace_first regex "\\1" str in
         let hgt = int_of_string value in
         let unit = String.sub str (String.length str - 2) 2 in
         if String.equal unit "cm" then (hgt >= 150) && (hgt <= 193)
         else if String.equal unit "in" then (hgt >= 59) && (hgt <= 76)
         else false
      | "pid" ->
         let regex = Str.regexp "pid:\\([0-9]+\\)" in
         let value = Str.replace_first regex "\\1" str in
         (String.length value) == 9
      | "cid" -> true
      | _ -> false in
    match list with
    | [] -> []
    | hd :: tl ->
       ctrl_aux_rec hd :: ctrl_aux tl in
  let is_ok = List.fold_left (fun x y -> x && y) true (ctrl_aux list) in
  if is_ok then 1 else 0
;;

let rec process list = match list with
  | [] -> 0
  | hd :: tl when (size hd) == 8 ->
     ctrl_data hd + process tl
  | hd :: tl when (size hd) <= 6 -> process tl
  | hd :: tl ->
     let cid = fun str -> BatString.exists str "cid" in
     let has_cid = List.fold_left (fun x y -> x || y) false (List.map cid hd) in
     if has_cid
     then process tl
     else ctrl_data hd + process tl
;;

let value = process (parse (readfile "./input.txt"));;
