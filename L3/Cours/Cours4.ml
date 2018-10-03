type op = Moins | Plus | Mult | Div
type t = 
    | Cst of int
    | MoinsUnaire of t
    | OpBinaire of t * op * t
