(* Euler's Totient Function *)
let phi n =
  let int_of_bool x = if x then 1 else 0 in
  let rec gcd x y =
    if y = 0 then x
    else gcd y (x mod y) in
  let coprime x y = gcd x y = 1 in
  let rec count_coprimes acc i = 
    if i = n then acc
    else count_coprimes (acc + int_of_bool (coprime i n)) (i + 1) in
  if n = 1 then 1 else count_coprimes 0 1

