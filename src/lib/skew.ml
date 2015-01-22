open Core.Std

let skew str =
  let n = String.length str in
  let rec loop i c acc =
    if i >= n then
      List.rev (c :: acc)
    else
      let nc = 
        match str.[i] with 
        | 'C' -> (c - 1)
        | 'G' -> (c + 1) 
        | 'A' | 'T' -> c
        | x   -> raise (Invalid_argument (sprintf "skew: %c" x))
      in
      loop (i + 1) nc (c :: acc)
  in
  loop 0 0 []


let skew_min str =
  let sa = skew str in
  let ma = List.mapi ~f:(fun i s -> (i, s)) sa in
  let so = List.sort ~cmp:(fun (_,s1) (_,s2) -> compare s1 s2) ma in
  let (_, m) = List.hd_exn so in
  let rec loop acc = function
    | []           -> List.rev acc
    | (i,m2) :: tl -> if m2 = m 
                      then loop (i :: acc) tl
                      else List.rev acc
  in
  (m, loop [] so)


(* Tests *)
let test_skew () =
  assert (skew "ACGGC" =  [0; 0; -1; 0; 1; 0])

let test_skew_min () =
  assert (skew_min "ACGGC" = (-1,[2]))
