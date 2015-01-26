
type frequency_array 

(** [frequency_array text k] construct a frequency (# of observed instances)
    array for all [k]-mer's in [text]. *)
val frequency_array : ?bound:int -> string -> int -> frequency_array

(** [annotate_fa k] annotate a frequency array of [k]-mers *)
val annotate_fa : int -> frequency_array -> (string * int) array

(** [sort_fa k] sort an annotated frequency array of [k]-mers in descending
    frequency. *)
val sort_fa : int -> frequency_array -> (string * int) array

(** [clumps k at_least window_length genome] finds all [k]-mers that occur at
    [at_least] times in a [window_length] in [genome].  *)
val clumps : k:int -> at_least:int -> window_length:int -> string -> string list
