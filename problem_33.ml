(* Determine Whether Two Positive Integer Numbers Are Coprime *)
let coprime x y =
  let rec gcd x y = if x = y then x else
      if x > y then gcd y (x - y) else gcd x (y - x)
  in gcd x y = 1;
