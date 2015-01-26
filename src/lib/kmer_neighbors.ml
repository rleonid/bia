open Pattern

let ia f = Printf.kprintf (fun s -> raise (Invalid_argument s)) f ;;

(* It's inefficient to compute neighbors by creating strings, so we'll use
   their 'pattern' representation. *)
(* Return the other possible nucleotides. *)

let pow4 y =
  let rec loop i a =
    if i > y then a
    else loop (i + 1) (a * 4)
  in loop 1 1

let coeff ~k ~pattern ~pos = (pattern / (pow4 (k - pos - 1))) mod 4;;

let others = function
  | 0 -> [| 1; 2; 3|]
  | 1 -> [| 0; 2; 3|]
  | 2 -> [| 0; 1; 3|]
  | 3 -> [| 0; 1; 2|]
  | x   -> ia "others: %d" x

let neigh_at ~k ~pattern ~pos =
  let c = coeff ~k ~pattern ~pos in
  let p = pow4 (k - pos - 1) in
  let a = pattern - c * p in
  Array.map (fun i -> a + i * p) (others c)

let neigh_at_codes ~k ~pattern ~pos =
  neigh_at ~k ~pattern ~pos
  |> Array.map (int_to_pat ~k)

(* Immediate neighbors *)
let immediate str =
  let k   = String.length str in
  let pat = pat_to_int str in
  let res = Array.make (k * 3) 0 in
  for i = 0 to (k - 1) do
    Array.blit (neigh_at ~k ~pattern:pat ~pos:i) 0 res (i * 3) 3;
  done;
  Array.map (int_to_pat ~k) res

module IntSet = Set.Make (struct 
  type t = int
  let compare = compare
  end)

let neighbor_patterns ~k ~pat ~bound =
  let rec loop pos changes acs pat =
    if changes = 0 then
      acs
    else if pos >= k then
      acs
    else
      let change_later = loop (pos + 1) changes acs pat in
      let others_chan  =
        neigh_at ~k ~pattern:pat ~pos
        |> Array.map (fun npat -> 
            loop (pos + 1) (changes - 1) (IntSet.add npat acs) npat)
        |> Array.fold_left IntSet.union change_later
      in
      IntSet.union change_later others_chan
  in
  loop 0 bound (IntSet.singleton pat) pat
  |> IntSet.elements

let neighbors str bound =
  let k   = String.length str in
  let pat = pat_to_int str in
  neighbor_patterns ~k ~pat ~bound
  |> List.map (int_to_pat ~k)

let test_immediate () =
  assert (immediate "ACGT" =
    [| "CCGT"; "GCGT"; "TCGT"
     ; "AAGT"; "AGGT"; "ATGT"
     ; "ACAT"; "ACCT"; "ACTT"
     ; "ACGA"; "ACGC"; "ACGG"
    |])

(* Almost the same but include the original string. *)
let test_neighbors () =
  assert (neighbors "ACGT" 1 =
    [ "AAGT"; "ACAT"; "ACCT"
    ; "ACGA"; "ACGC"; "ACGG"
    ; "ACGT"
    ; "ACTT"; "AGGT"; "ATGT"
    ; "CCGT"; "GCGT"; "TCGT"
    ])
