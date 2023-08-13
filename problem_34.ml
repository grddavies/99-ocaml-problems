(* Euler's Totient Function *)
let phi n =
  let rec gcd x y =
    if y = 0 then x
    else gcd y (x mod y) in
  let coprime x y = gcd x y = 1 in
  let xs = List.init (n - 1) ((+) 1) in
  if n = 1 then 1 else (List.filter (coprime n) xs) |> List.length
