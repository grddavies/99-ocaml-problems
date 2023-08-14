(* Determine the Prime Factors of a Given Positive Integer  *)
let factors n =
  let rec aux acc d x =
    if x = 1 then acc
    else if x mod d = 0 then aux (d::acc) (d) (x/d)
    else aux acc (d+1) x
  in aux [] 2 n |> List.rev;;

