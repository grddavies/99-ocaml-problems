 (* Determine the greatest common divisor of two positive integer numbers. *)
let rec gcd x y =
  if x = y then x
  else if x > y then gcd y (x - y)
  else gcd x (y - x)

