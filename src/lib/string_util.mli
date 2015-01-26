
(** [contained_at ~pattern pos ~text] determines if [pattern] is found at [pos]
    of [text] *)
val contained_at : pattern:string -> int -> text:string -> bool

(** Compute the hamming distance between two strings *)
val hamming : string -> string -> int

(** [hamming_bound ~text ~pos ~text2 ~bound] determines if the hamming distance
    of [text] at [pos] to [text2] less than or equal to [bound]. *)
val hamming_bound : text:string -> pos:int -> text2:string -> bound:int -> bool

