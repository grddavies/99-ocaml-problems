let prime n =
  let basis = [2; 3; 5] in
  let prod = List.fold_left ( * ) 1  basis in
  let range = List.init prod (fun x -> x + 1) in
  let wheel = List.filter (fun i -> List.for_all (fun y -> i mod y <> 0) basis) range in
  let wheel = List.tl wheel @ [1+prod] in
  let rec loop base = function
    (* No coprimes left in this turn of the wheel -> increment base *)
    | [] -> loop (base + prod) wheel
    | h :: t ->
        let i = base + h in
        i * i > n || n mod i <> 0 && loop base t
  in n > 1 && loop 0 (basis @ wheel)

let all_primes start finish =
    let rec aux acc i finish =
      let ps = if prime i then (i::acc) else acc in
      if i = finish then ps else aux ps (i + 1) finish
    in
  List.rev (aux [] start finish)

let goldbach n =
  if n mod 2 <> 0 then raise (Invalid_argument "Not an even number!") else
    let primes = all_primes 2 (n/2) in
    let rec aux = function
      | [] -> raise (Failure "Golbach was wrong")
      | p::ps -> if prime (n - p) then (p, n - p)
        else aux ps
    in
    aux primes

