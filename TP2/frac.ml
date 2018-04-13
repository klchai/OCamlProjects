type frac = { num : int; denom : int}
;;

let rec pgcd x y = 
  if y = 0 then x
   else pgcd y (x mod y)
;;

let simpl q = 
  let gcd = pgcd q.num q.denom in
  {num = q.num / gcd; denom = q.denom / gcd}
;;

let ajoute_fracs q1 q2 = 
  simpl { num = q1.num * q2.denom + q2.num * q1.denom; 
          denom = q1.denom * q2.denom};;
;;

let inverse f =
   {num = f.denom; denom = f.num}
;;

let mult_constante c f = simpl {f with num = f.num * c}
;;

(* TESTS *)
let () =
  assert (pgcd 4 7 = 1 && pgcd 4 8 = 4 && pgcd 6 21 = 3);
  assert (simpl { num = 2; denom = 3 } = { num = 2; denom = 3 }
       && simpl { num = 2; denom = 4 } = { num = 1; denom = 2 });
  assert (ajoute_fracs { num = 2; denom = 3 } { num = 5; denom = 6 } = { num = 3; denom = 2 }
       && ajoute_fracs { num = 2; denom = 3 } { num = 1; denom = 3 } = { num = 1; denom = 1 }
       && ajoute_fracs { num = 2; denom = 3 } { num = 8; denom = 10 } = { num = 22; denom = 15 });
  assert (inverse { num = 2; denom = 3 } = { num = 3; denom = 2 }
       && inverse { num = 5; denom = 1 } = { num = 1; denom = 5 });
  assert (mult_constante 2 { num = 2; denom = 3 } = { num = 4; denom = 3 }
       && mult_constante 3 { num = 2; denom = 3 } = { num = 2; denom = 1 })

type expr = 
 | Int of int
 | Var of string
;;

type fracexpr = { enum : expr; edenom : expr}
;;

let simpl_expr q = match q.enum, q.edenom with
 | Int x, Int y ->
   let gcd = pgcd x y in { enum = Int (x / gcd); edenom = Int (y / gcd)}
 | _ -> q
;;  

(* TESTS *)
let () =
  assert (simpl_expr { enum = Int 2; edenom = Int 3 } = { enum = Int 2; edenom = Int 3 });
  assert (simpl_expr { enum = Int 2; edenom = Int 4 } = { enum = Int 1; edenom = Int 2 });
  assert (simpl_expr { enum = Int 2; edenom = Var "x" } = { enum = Int 2; edenom = Var "x" })

let regle_de_trois q1 q2 = match q1, q2 with
  | { enum = Int x; edenom = Int y }, { enum = Int z; edenom = Var v } ->
    simpl_expr { enum = Int (z * y); edenom = Int x }
  | { enum = Int x; edenom = Int y }, { enum = Var v; edenom = Int z } ->
    simpl_expr { enum = Int (z * x); edenom = Int y }
  | { enum = Int x; edenom = Var v }, { enum = Int y; edenom = Int z } ->
    simpl_expr { enum = Int (x * z); edenom = Int y }
  | { enum = Var v; edenom = Int x }, { enum = Int y; edenom = Int z } ->
    simpl_expr { enum = Int (x * y); edenom = Int z }
  | _ -> failwith "Règle de trois impossible"
;;
(* TESTS *)
let () =
    assert (regle_de_trois { enum = Int 2; edenom = Int 3 } { enum = Int 4; edenom = Var "x" } = { enum = Int 6; edenom = Int 1 });
    assert (regle_de_trois { enum = Int 4; edenom = Int 3 } { enum = Var "x"; edenom = Int 5 } = { enum = Int 20; edenom = Int 3 });
    assert (regle_de_trois { enum = Var "x"; edenom = Int 3 } { enum = Int 8; edenom = Int 10 } = { enum = Int 12; edenom = Int 5 })