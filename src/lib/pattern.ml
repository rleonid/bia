
let ia f = Printf.kprintf (fun s -> raise (Invalid_argument s)) f ;;

let char_to_int = function
  | 'A' -> 0
  | 'C' -> 1
  | 'G' -> 2
  | 'T' -> 3
  | x   -> ia "char_to_int: %c" x

let int_to_char = function
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | x -> ia "int_to_char: %d" x

let pat_to_int s =
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    r := 4 * !r  + char_to_int s.[i];
  done;
  !r

let int_to_pat ~k p =
  let rec loop kp a =
    if kp = 0
    then if List.length a < k
         then loop kp ('A'::a)
         else String.of_char_list a
    else loop (kp / 4) ((int_to_char (kp mod 4))::a)
  in loop p []

let pat_to_int_sub text ~pos ~len =
  let rec loop i acc =
    if i = len then acc
    else loop (i + 1) (acc * 4 + char_to_int text.[pos + i])
  in
  loop 0 0

  (*
open Core.Std
let pat_to_int2 s =
  let rec loop p = function [] -> p | h :: t -> loop (p * 4 + char_to_int h) t in
  loop 0 (String.to_list s)


*)
