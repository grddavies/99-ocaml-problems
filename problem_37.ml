(* Calculate Euler's Totient Function Φ(m) (Improved)  *)
(* If the list of the prime factors of a number m is known in the form of the previous problem then the function phi(m) can be efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...] be the list of prime factors (and their multiplicities) of a given number m. Then φ(m) can be calculated with the following formula: *)
(* φ(m) = (p1 - 1) × p1^(m1 - 1) × (p2 - 1) × p2^(m2 - 1) × (p3 - 1) × p3^(m3 - 1) × ⋯ *)
let factors n =
  let basis = [2; 3; 5] in
  let prod = List.fold_left ( * ) 1  basis in
  let range = List.init prod (fun x -> x + 1) in
  let wheel = List.filter (fun i -> List.for_all (fun y -> i mod y <> 0) basis) range in
  let wheel = List.tl wheel @ [1+prod] in
  let rec loop acc x base = function
    (* No coprimes left in this turn of the wheel -> increment base *)
    | [] -> loop acc x (base + prod) wheel
    | h :: t ->
      let i = base + h in
      if x = 1 then acc
      else if x mod i = 0 then loop (i::acc) (x/i) base (basis@wheel)
      else loop acc x base t in
  let rec rle list =
    let rec aux n acc = function
      | [] -> []
      | [ x ] -> (n + 1, x) :: acc
      | a :: (b :: _ as t) ->
        if a = b then aux (n + 1) acc t else aux 0 ((n + 1, a) :: acc) t
    in aux 0 [] list
  in
  loop [] n 0 (basis@wheel) |> rle;;

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let phi_improved m =
  let facs = factors m in
  List.fold_left ( * ) 1 (List.map (fun (m, p) -> (p - 1) * pow p (m - 1)) facs)
