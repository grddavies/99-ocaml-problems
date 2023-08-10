(* Extract a Given Number of Randomly Selected Elements From a List (with replacement) *)
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

  (* Extract a Given Number of Randomly Selected Elements From a List (without replacement) *)
let rand_pluck ls n =
  let rec nth p n l =
  match l with
  | [] -> assert false
  | x :: xs -> if n == 0 then (x, p@xs) else nth (x :: p) (n-1) xs
  in
  let len = List.length ls in
  let randth list len = nth [] (Random.int len) list in
  let rec aux n acc list len = 
    if n = 0 then acc else
    let choice, rest = randth list len in
    aux (n - 1) (choice::acc) rest (len - 1) in
  aux (min n len) [] ls len
