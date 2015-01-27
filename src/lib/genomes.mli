(** Functions to manipulate genome strings. *)

val acc_upper : (int * int * int * int) -> char -> (int * int * int * int)
(** [acc_upper (num_a, num_c, num_g, num_t) n] increments the respective counts
    of the four nucleotides. *)

val counts_upper : string -> (int * int * int * int)
(** [counts_upper genome] returns a quadruple of the number of times
    'A','C','G' and 'T' are found in [genome]. *)

val skew : string -> int array
(** [skew sequence] returns the difference between the number of observe 'G' and
    'C' nucleotydes. *)

val skew_min : string -> (int * int list)
(** [skew_min] computes the value and locations of minimum skew in the passed
    sequence. *)
