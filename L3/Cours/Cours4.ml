type t =
| Str of string * int * int
| App of t * t * int

let empty = Str ("", 0, 0)

let length = function
| Str ( , , n)
| App ( , , n) -> n

let of string s = Str (s, 0, String.length s)

let make n c = of string (String.make n c)

let rec unsafe get t i = match t with
| Str (s, ofs, ) ->
s.[ofs + i]
| App (t1, t2, ) ->

let n1 = length t1 in
if i < n1 then
unsafe get t1 i
else
unsafe get t2 (i - n1)

let get t i =
if i < 0 || i >= length t then invalid arg "get";
unsafe get t i

let append t1 t2 =
App (t1, t2, length t1 + length t2)