(* Determine whether a given number is prime *)
let prime n =
  let lim = n |> float_of_int |> sqrt |> int_of_float in
  let rec seive i =
    if n = 1 then false
    else if i > lim then true
    else if n mod i = 0 then false
    else seive (i+1)
  in seive 2

