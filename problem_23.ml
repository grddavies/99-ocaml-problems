(* Extract a Given Number of Randomly Selected Elements From a List  *)
let rand_select ls n =
  let rec nth n l =
    match l with
    | [] -> assert false
    | x :: xs -> if n == 0 then x else nth (n - 1) xs
  in
  let len = List.length ls in
  let rec aux acc xs = function
    | 0 -> acc
    | n -> aux (nth (Random.int len) xs :: acc) xs (n - 1)
  in
  aux [] ls n
