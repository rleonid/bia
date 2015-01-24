let update_skew_count c = function
    | 'C' -> (c - 1)
    | 'G' -> (c + 1)
    | 'A'
    | 'T' -> c
    | x   -> raise (Invalid_argument (Printf.sprintf "skew: %c" x))

(* Notes
 * - Not a generic implementation as it is tailored for computing C - G.
 * - Includes the 0 starting value.
 *)
let skew str =
  let n = String.length str in
  let a = Array.make (n + 1) 0 in
  for i = 0 to (n - 1) do
    a.(i+1) <- update_skew_count a.(i) str.[i];
  done;
  a

let skew_min str =
  let n = String.length str in
  let rec loop count min_count pos_lst i =
    if i >= n then
      (min_count, pos_lst)
    else
      let new_count = update_skew_count count str.[i] in
      if new_count < min_count then
        loop new_count new_count [i+1] (i + 1)
      else if new_count = min_count then
        loop new_count min_count (i+1::pos_lst) (i + 1)
      else
        loop new_count min_count pos_lst (i + 1)
  in
  loop 0 0 [0] 0

(* Tests *)
let test_skew () =
  assert (skew "ACGGC" =  [|0; 0; -1; 0; 1; 0|])

let test_skew_min () =
  assert (skew_min "ACGGC" = (-1,[2]))
