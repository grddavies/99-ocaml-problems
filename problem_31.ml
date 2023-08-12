(* Determine whether a given number is prime *)
let prime n =
  let lim = n |> float_of_int |> sqrt |> int_of_float in
  let rec seive i =
    i > lim || (n mod i <> 0 && seive (i+1))
  in n <> 1 && seive 2

