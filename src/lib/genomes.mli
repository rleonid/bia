(** Functions to manipulate genome strings. *)

val acc_upper : (int * int * int * int) -> char -> (int * int * int * int)
(** [acc_upper (num_a, num_c, num_g, num_t) n] increments the respective counts
    of the four nucleotides. *)

val counts_upper : string -> (int * int * int * int)
