open Core
open STLC

let run_tests () =

  let bind id t = { name = id ; value = t } in
  let v id = Var id in
  let (=) = String.(=) in
  let print = Printf.printf in
  let test e s =
    let s' = show_expression e in
    if s = s' then print "matched  : %s\n" s else print "expected : %s\nactual   : %s\n" s s' in

  let abs id t body = Abs (bind id t, body) in
  let app lhs rhs = App (lhs, rhs) in
  let add lhs rhs = Bin (Add, lhs, rhs) in
  let sub lhs rhs = Bin (Sub, lhs, rhs) in
  let mul lhs rhs = Bin (Mul, lhs, rhs) in
  let exp lhs rhs = Bin (Exp, lhs, rhs) in
  let lit i = Lit i in

  let identity = abs "x" Z64 (v "x") in
  let adder = abs "a" Z64 (abs "b" Z64 (add (v "a") (v "b"))) in
  let add_two_three = app (app adder (lit 2)) (lit 3) in

  let _ =
    let e = app (app (v "a") (v "b")) (v "c") in
    test e "a b c" in

  let _ =
    let e = app (v "a") (app (v "b") (v "c")) in
    test e "a (b c)" in

  let _ =
    let e = add identity (lit 1) in
    test e "(λ x : Z . x) + 1" in

  let _ =
    let e = add (lit 1) identity in
    test e "1 + (λ x : Z . x)" in

  let _ =
    let e = add identity identity in
    test e "(λ x : Z . x) + (λ x : Z . x)" in

  let _ =
    test adder "λ a : Z . λ b : Z . a + b" in

  let _ =
    test add_two_three "(λ a : Z . λ b : Z . a + b) 2 3" in

  let _ =
    let e = mul add_two_three add_two_three in
    test e "(λ a : Z . λ b : Z . a + b) 2 3 * (λ a : Z . λ b : Z . a + b) 2 3" in

  let _ =
    let e = add (add (lit 2) (lit 3)) (lit 4) in
    test e "2 + 3 + 4" in

  let _ =
    let e = sub (sub (lit 2) (lit 3)) (lit 4) in
    test e "2 - 3 - 4" in

  let _ =
    let e = add (lit 2) (add (lit 3) (lit 4)) in
    test e "2 + (3 + 4)" in

  let _ =
    let e = sub (lit 2) (sub (lit 3) (lit 4)) in
    test e "2 - (3 - 4)" in

  let _ =
    let e = mul (app (v "f") (v "x")) (v "y") in
    test e "f x * y" in

  let _ =
    let e = app (v "f") (mul (v "x") (v "y")) in
    test e "f (x * y)" in

  let _ =
    let e = mul (v "x") (app (v "f") (v "y")) in
    test e "x * f y" in

  let _ =
    let e = app (mul (v "x") (v "f")) (v "y") in
    test e "(x * f) y" in

  let _ =
    let e = exp (v "a") (exp (v "b") (v "c")) in
    test e "a ^ b ^ c" in

  let _ =
    let e = exp (exp (v "a") (v "b")) (v "c") in
    test e "(a ^ b) ^ c" in

  ()

let () = run_tests ()
