type 'a t = F of 'a | N of 'a t * 'a t * 'a t * 'a t

val affiche : Graphics.color t -> unit
