open Pattern 

let frequency_array text len =
  let n = String.length text in
  let t = int_of_float (4.0 ** (float_of_int len)) in
  let arr = Array.create t 0 in
  for pos = 0 to n - len do
    let i  = pat_to_int_sub text ~pos ~len:len in
    arr.(i) <- arr.(i) + 1;
  done;
  arr

let annotated_frequency_array text len =
  frequency_array text len
  |> Array.mapi (fun index freq -> (int_to_pat len index, freq))

let sorted_frequency_array text len =
  let res = annotated_frequency_array text len in
  Array.sort (fun (_,f1) (_,f2) -> compare f2 f1) res;
  res
