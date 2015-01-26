
(** [matches ~pattern ~text] where does [pattern] occur in [text]?*)
val matches : pattern:string -> text:string -> int list

(** [num_matches ~pattern ~text] determine the number of time that [pattern]
    is found in [text] *)
val num_matches : pattern:string -> text:string -> int

(** [approx_matches ~text ~pattern bound] determines the positions in [text]
    where [pattern] has a hamming distance less than or equal to [bound].*)
val approx_matches : pattern:string -> text:string -> int -> int list

(** [num_approx_matches ~pattern ~text bound] the number of times that
    [pattern] is found in [text] with a hamming distance less than or equal to
    [bound]. *)
val num_approx_matches : pattern:string -> text:string -> int -> int

