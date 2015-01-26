
(** [frequency_array text k] construct a frequency (# of observed instances)
    array for all [k]-mer's in [text]. *)
val frequency_array : string -> int -> int array

(** [annotated_frequency_array text k] construct a frequency (# of observed
    instances) array for all [k]-mer's in [text], annotated by the pattern. *)
val annotated_frequency_array : string -> int -> (string * int) array

(** [sorted_frequency_array text k] construct a frequency (# of observed
    instances) array for all [k]-mer's in [text], annotated by the pattern,
    sorted in descending frequency. *)
val sorted_frequency_array : string -> int -> (string * int) array

(** [clumps k at_least window_length genome] finds all [k]-mers that occur at
    [at_least] times in a [window_length] in [genome].  *)
val clumps : k:int -> at_least:int -> window_length:int -> string -> string list
