#load "genomes.cmo";;
#load "pattern.cmo";;
#load "kmer_neighbors.cmo";;
#load "kmer_counts.cmo";;

open Core.Std ;;

let naive fast_fname ~k ~window ~bound ~reverse =
  let lst = In_channel.read_lines fast_fname in
  let sal = String.concat (List.tl_exn lst) in  (* ignore header *)
  let min_skew, pos_lst = Genomes.skew_min sal in
  printf "Minimum skew %d was achieved at positions: \n" min_skew;
  List.iter pos_lst ~f:(printf "\t %d\n");
  let min_pos = List.hd_exn pos_lst in
  printf "using %d\n" min_pos;
  let sub_sal = String.sub ~pos:(min_pos - window/2) ~len:window sal in
  let freq_ar = Kmer_counts.frequency_array ~bound ~reverse sub_sal k in
  let sorted  = Kmer_counts.sort_fa k freq_ar in
  let (_, tf) = Array.get sorted 0 in
  let subi,w  = Array.findi_exn sorted ~f:(fun _ (_,f) -> f < tf) in
  Array.sub ~pos:0 ~len:(subi - 1) sorted, w
