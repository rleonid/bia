
let contained_at ~pattern pos ~text =
  let m = String.length pattern in
  let n = String.length text in
  let rec loop i =
    i = m || (text.[pos + i] = pattern.[i]) && loop (i + 1)
  in
  m + pos <= n && loop 0

let hamming str1 str2 =
  let n = String.length str1 in
  let rec loop i h =
    if i >= n then
      h
    else if str1.[i] = str2.[i] then
      loop (i + 1) h
    else
      loop (i + 1) (h + 1)
  in
  loop 0 0

let hamming_bound ~text ~pos ~text2 ~bound =
  let n = String.length text in
  let m = String.length text2 in
  let rec loop i h =
    if h > bound then
      false
    else if i + pos >= n || i >= m then
      true
    else if text.[i + pos] = text2.[i] then
      loop (i + 1) h
    else
      loop (i + 1) (h + 1)
  in
  loop 0 0

