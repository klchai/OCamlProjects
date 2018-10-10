module type ELT  = 
  sig 
    type t 
    val compare : t -> t -> int
  end 

module type S = 
  sig
    type t
    val insertion : t -> t list -> t list
    val tri : t list -> t list
  end 

module type SL = 
  sig
    type t
    type tl
    val export : tl -> t list
    val insertion : t -> tl -> tl
    val tri : t list -> tl
  end

module T(E : ELT) : S with type t = E.t  
