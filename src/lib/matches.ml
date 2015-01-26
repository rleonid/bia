open String_util

let matches_sub ~pattern ~text init f p =
  let nt = String.length text in
  let np = String.length pattern in
  let rec loop acc pos =
    if np + pos > nt
    then acc
    else if p pos
         then loop (f acc pos) (pos + 1)
         else loop acc (pos + 1)
  in
  loop init 0

let matches ~pattern ~text =
  matches_sub ~pattern ~text []
    (fun lst pos -> pos :: lst)
    (fun pos -> contained_at ~pattern pos ~text)
  |> List.rev

let num_matches ~pattern ~text =
  matches_sub ~pattern ~text 0
    (fun acc _ -> acc + 1)
    (fun pos -> contained_at ~pattern pos ~text)

let approx_matches ~pattern ~text bound =
  matches_sub ~pattern ~text []
    (fun lst pos -> pos :: lst)
    (fun pos -> hamming_bound ~text ~pos ~text2:pattern ~bound)
  |> List.rev

let num_approx_matches ~pattern ~text bound =
  matches_sub ~pattern ~text 0
    (fun acc _ -> acc + 1)
    (fun pos -> hamming_bound ~text ~pos ~text2:pattern ~bound)

