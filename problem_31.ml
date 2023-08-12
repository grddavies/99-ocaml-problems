(* Determine whether a given number is prime *)
let prime n =
  let basis = [2; 3; 5] in
  let prod = List.fold_left ( * ) 1  basis in
  let range = List.init prod (fun x -> x + 1) in
  let wheel = List.filter (fun i -> List.for_all (fun y -> i mod y <> 0) basis) range in
  let wheel = List.tl wheel in
  let rec loop base = function
    (* No coprimes left in this turn of the wheel -> increment base *)
    | [] -> loop (base + prod) wheel
    | h :: t ->
        let i = base + h in
        i * i > n || n mod i <> 0 && loop base t
  in n > 1 && loop 0 (basis @ wheel)


