(* Determine Whether Two Positive Integer Numbers Are Coprime *)
let coprime x y =
  let rec gcd x y =
    if y = 0 then x
    else gcd y (x mod y)
  in gcd x y = 1;
