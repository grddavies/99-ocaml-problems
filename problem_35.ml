(* Determine the Prime Factors of a Given Positive Integer  *)
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
        else loop acc x base t
  in
  loop [] n 0 (basis@wheel) |> List.rev;;
