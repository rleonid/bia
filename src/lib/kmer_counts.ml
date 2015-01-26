open Pattern

let frequency_array text len =
  let n = String.length text in
  let t = int_of_float (4.0 ** (float_of_int len)) in
  let arr = Array.create t 0 in
  for pos = 0 to n - len do
    let i = pat_to_int_sub text ~pos ~len:len in
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

let clumps ~k ~at_least ~window_length genome =
  let fst_window = String.sub genome 0 window_length in
  let freq_arr   = frequency_array fst_window k in
  let clump_arr  = Array.mapi (fun i f -> f >= at_least) freq_arr in
  let n = String.length genome in
  for i = 0 to n - window_length do
    let start_pat = pat_to_int_sub genome ~pos:i ~len:k in
    freq_arr.(start_pat) <- freq_arr.(start_pat) - 1;
    let end_pat   = pat_to_int_sub genome ~pos:(i + window_length - k) ~len:k in
    freq_arr.(end_pat) <- freq_arr.(end_pat) + 1;
    if freq_arr.(end_pat) >= at_least then clump_arr.(end_pat) <- true;
  done;
  Array.fold_left
    (fun (index,acc) is_clump ->
      if is_clump then
        (index + 1, int_to_pat ~k index :: acc)
      else
        (index + 1, acc)) (0,[]) clump_arr
  |> snd
  |> List.rev


