(* Replicate the elements of a list a given number of times. *)
let replicate list n =
  let rec repeat n e acc = if n = 0 then acc else repeat (n - 1) e (e :: acc) in
  let rec aux n acc = function
    | [] -> acc
    | x :: xs -> aux n (acc @ repeat n x []) xs
  in
  aux n [] list
