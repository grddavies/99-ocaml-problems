(* Split a list into two parts; the length of the first part is given. *)
(* If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
let drop n l =
  let rec aux i = function
    | [] -> []
    | x :: xs as rest -> if i = 0 then rest else aux (i - 1) xs
  in
  aux n l

let take n l =
  let rec aux acc i = function
    | [] -> acc
    | x :: xs -> if i = 0 then acc else aux (x :: acc) (i - 1) xs
  in
  List.rev (aux [] n l)

let slice list i k = drop i list |> take (k - 1)
