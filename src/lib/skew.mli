
val skew : string -> int array
(** [skew sequence] returns the difference between the number of observe 'G' and
    'C' nucleotydes. *)

val skew_min : string -> (int * int list)
(** [skew_min] computes the value and locations of minimum skew in the passed
    sequence. *)
