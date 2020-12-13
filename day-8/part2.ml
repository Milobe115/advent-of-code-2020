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

type instr =
  | NOP of int
  | JMP of int
  | ACC of int
  | NANI

let parse_instr array =
  let instr_arr = Array.init (Array.length array) (fun (x) -> NOP(x)) in
  for i = 0 to (Array.length array) - 1 do
    let s = array.(i) in
    match String.sub s 0 3 with
    | "nop" -> instr_arr.(i) <- NOP(int_of_string (String.sub s 4 (String.length s - 4)))
    | "acc" -> instr_arr.(i) <- ACC(int_of_string (String.sub s 4 (String.length s - 4)))
    | "jmp" -> instr_arr.(i) <- JMP(int_of_string (String.sub s 4 (String.length s - 4)))
    | _ -> instr_arr.(i) <- NANI
  done;
  instr_arr
;;

let play_and_move array index acc =
  match array.(index) with
  | NOP(x) -> (index + 1, acc)
  | ACC(x) -> (index + 1, acc + x)
  | JMP(x) -> (index + x, acc)
  | _ -> (-1,-1)
;;

let run array = 
  let rec run_aux array index visited acc =
    match List.exists (fun (x) -> (x == index)) visited with
    | true -> -9001
    | false ->
       if (index < Array.length array) then
         let (newindex, newacc) = play_and_move array index acc in
         run_aux array newindex (index::visited) newacc
       else
         acc
  in
  run_aux array 0 [] 0
;;

let change_instr arr index = match arr.(index) with
  | NOP(x) -> arr.(index) <- JMP(x); arr
  | JMP(x) -> arr.(index) <- NOP(x); arr
  | _ ->  arr

let find_instruction array =
  let rec find_instruction_aux array i =
    let new_arr = Array.copy array in
    let new_acc = run (change_instr new_arr i) in
    match new_acc with
    | -9001 -> find_instruction_aux array (i+1)
    | _ -> new_acc in
  find_instruction_aux array 0
;;
 
find_instruction (parse_instr (Array.of_list (readfile "./input.txt")));;
