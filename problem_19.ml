(* Rotate a list N places to the left. *)

let split list n =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | x :: xs as rest ->
        if i = 0 then (List.rev acc, rest) else aux (x :: acc) (i - 1) xs
  in
  aux [] n list

let rec rotate xs n =
  let i = n mod List.length xs in
  let h, t = split xs i in
  t @ h
