
type path = Match
          | Delete
          | Insert
          | MatDel
          | MatIns
          | DelIns
          | AllThree

let tmax ~mat ~del ~ins =
  let comp2 x y eq lt gt = 
    if x = y
    then eq, x
    else if x < y
         then lt, y
         else gt, x
  in
  if mat = del
  then comp2 del ins AllThree Insert MatDel
  else if mat < del
       then comp2 del ins DelIns Insert Delete
       else (* mat > del *)
            comp2 mat ins MatIns Insert Match

let needleman_wunsch_grid gap p s1 s2 =
  let x = String.length s1
  and y = String.length s2 in
  let m = ArrayLabels.make_matrix ~dimx:(x+1) ~dimy:(y+1) 0 in
  let tmax x y z = if x > y then max x z else max y z in
  for i = 1 to x do m.(i).(0) <- gap * i done;
  for j = 1 to y do m.(0).(j) <- gap * j done;
  for i = 1 to x do
    for j = 1 to y do
      let i1 = i - 1
      and j1 = j - 1 in
      let mat = m.(i1).(j1) + p s1.[i1] s2.[j1]
      and del = m.(i1).(j) + gap
      and ins = m.(i).(j1) + gap in
      m.(i).(j) <- tmax mat del ins
    done;
  done;
  m

let rec take f = function
  | []     -> []
  | h :: t -> if f h 
              then h :: take f t
              else []

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
  let rec loop i j n acc =
    match (i, j) with
    | (0, 0) -> List.map (make_strings n) acc
    | (0, _) -> loop 0 (j - 1) (n + 1) (ladd '-' col.[j-2] acc)
    | (_, 0) -> loop (i - 1) 0 (n + 1) (ladd row.[i-2] '-' acc)
    | _      ->
        let i1 = i - 1 and j1 = j - 1 in
        let rc = row.[i1] and cc = col.[j1] in
        (*Printf.printf "-%d %d\n" i j; *)
        let weights = 
          [ ((i1,j1), (rc,  cc)); 
            ((i1,j),  (rc,  '-'));
            ((i,j1),  ('-', cc)) ] 
          |> List.map (fun ((x,y) as p,c) -> (m.(x).(y), p, c))
          |> List.sort (fun (w1, _, _) (w2, _, _) -> -1 * compare w1 w2)
        in
        let w, _, _ = List.hd weights in
        take (fun (w1,_, _) -> w1 = w) weights
        |> List.map (fun (_, (x,y), (r,c)) -> loop x y (n + 1) (ladd r c acc))
        |> List.flatten 
  in
  loop (Array.length m - 1) (Array.length m.(0) - 1) 0 [[],[]]
       

let can_nw = needleman_wunsch_grid (-1) (fun x y -> if x = y then 1 else -1)
let s1 = "ACTG"
let s2 = "ACTG"
let t1 = "GATTACA" 
let t2 = "GCATGCU" 
let t3 = "GCATGCUG" 
