(* Construct a list containing the prime factors and their multiplicity. *)
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

