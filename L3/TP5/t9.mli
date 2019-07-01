type t = { words : string list ; branches : (int * t) list }
val empty : t
val find : int list -> t -> string list
val find_word : string -> t -> int list
val add : int list -> string -> t -> t
