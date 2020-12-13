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

let play_and_move array index acc =
  let s = array.(index) in
  match String.sub s 0 3 with
  | "nop" -> (index + 1, acc)
  | "acc" -> (index + 1, acc + int_of_string (String.sub s 4 (String.length s - 4)))
  | "jmp" -> (index + int_of_string (String.sub s 4 (String.length s - 4)), acc)
  | _ -> (-1,-1)
;;

let process array = 
  let rec process_aux array index visited acc =
    let () = print_endline array.(index) in
    match List.exists (fun (x) -> (x == index)) visited with
    | true -> acc
    | false ->
       let (newindex, newacc) = play_and_move array index acc in
       process_aux array newindex (index::visited) newacc
  in
  process_aux array 0 [] 0
;;

process (Array.of_list (readfile "./input.txt"));;
