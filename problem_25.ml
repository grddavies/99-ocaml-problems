(* Generate a random permutation of the elements of a list *)
let permute ls =
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
  aux len [] ls len

