(* Draw N different random numbers from the set 1..M. *)
let range start finish =
  let d = if start < finish then 1 else -1 in
  let rec aux acc i finish =
    if i = finish then i :: acc else aux (i :: acc) (i + d) finish
  in
  List.rev (aux [] start finish)

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

let lotto_select n m = rand_pluck (range 1 m) n
