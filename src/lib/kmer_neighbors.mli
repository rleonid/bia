
(** [immediate str] computes [neighbors str 1] *)
val immediate : string -> string array

(** [neighbors str bound] return all patterns that are at most [bound] hamming
    distance from [str].*)
val neighbors : string -> int -> string array

