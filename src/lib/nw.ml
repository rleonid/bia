
type dir = Left
         | TopLeft
         | Top

let tmax ~tl ~l ~t =
  let against_top d ds =
    if d = t
    then (Top :: ds), d
    else if d < t
         then [Top], t
         else ds, d
  in
  if tl = l
  then against_top l [Left; TopLeft]
  else if tl < l
       then against_top l [Left]
       else against_top tl  [TopLeft]

(* Construct the Needleman-Wunsch grid *)
let needleman_wunsch_grid gap p s1 s2 =
  let x = String.length s1
  and y = String.length s2 in
  let m = ArrayLabels.make_matrix ~dimx:(x+1) ~dimy:(y+1) ([TopLeft], 0) in
  for i = 1 to x do m.(i).(0) <- ([Top], gap * i) done;
  for j = 1 to y do m.(0).(j) <- ([Left], gap * j) done;
  for i = 1 to x do
    for j = 1 to y do
      let i1 = i - 1
      and j1 = j - 1 in
      let tl = (snd m.(i1).(j1)) + p s1.[i1] s2.[j1]
      and t  = (snd m.(i1).(j)) + gap
      and l  = (snd m.(i).(j1)) + gap in
      m.(i).(j) <- tmax ~tl ~t ~l
    done;
  done;
  m, m.(x).(y)

let traceback m row col =
  let make_strings n (l, r) =
    let ll = String.make n '-'
    and rr = String.make n '-' in
    let _n = List.fold_left2 (fun idx lc rc ->
                ll.[idx] <- lc;
                rr.[idx] <- rc;
                idx + 1) 0 l r
    in
    (ll, rr)
  in
  let add cl cr (ll,rr) = cl::ll, cr::rr in
  let ladd cl cr = List.map (add cl cr) in
  let rec loop n i j acc =
    if i = 0 && j = 0
    then List.map (make_strings n) acc
    else
      let i1 = i - 1 and j1 = j - 1 in
      let dirs, _ = m.(i).(j) in
      dirs
      |> List.map (function
            | Left    -> loop (n + 1) i  j1 (ladd '-'      col.[j1] acc)
            | Top     -> loop (n + 1) i1 j  (ladd row.[i1] '-'      acc)
            | TopLeft -> loop (n + 1) i1 j1 (ladd row.[i1] col.[j1] acc))
      |> List.flatten
  in
  loop 0 (Array.length m - 1) (Array.length m.(0) - 1) [[],[]]

let nw gap p s1 s2 =
  let m, (_, score) = needleman_wunsch_grid gap p s1 s2 in
  traceback m s1 s2, score

(* Canonical values. *)
let c_gap = -1
let c_score x y = if x = y then 1 else -1

(* Canonical Needleman-Wunsch *)
let c_nw = nw c_gap c_score

module NWGrid_printer = Aligned_array_array_printer (struct
  type t = dir list * int

  let to_string (dl, n) =
    Printf.sprintf "%s : %d"
      (String.concat "; "
        (List.map (function
            | Left    -> "Left"
            | TopLeft -> "TopLeft"
            | Top     -> "Top") dl))
      n

  let width p = to_string p |> String.length

  let printer frmttr p = Format.pp_print_string frmttr (to_string p)
end)

(* #install_printer NWGrid_printer.printer *)
