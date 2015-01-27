
type frequency_array

(** [frequency_array text k] construct a frequency (# of observed instances)
    array for all [k]-mer's in [text].

    Optionally include all neighbors of [bound] distance away from a k-mer in
    that k-mer's count, by setting bound to a positive integer.
    Furthermore, you can include a k-mer's reverse complement's (ie. CGA -> TCG)
    counts in that k-mer's count.
    *)
val frequency_array : ?bound:int -> ?reverse:bool -> string -> int -> frequency_array

(** [annotate_fa k] annotate a frequency array of [k]-mers *)
val annotate_fa : int -> frequency_array -> (string * int) array

(** [sort_fa k] sort an annotated frequency array of [k]-mers in descending
    frequency. *)
val sort_fa : int -> frequency_array -> (string * int) array

(** [clumps k at_least window_length genome] finds all [k]-mers that occur at
    [at_least] times in a [window_length] in [genome].  *)
val clumps : k:int -> at_least:int -> window_length:int -> string -> string list
