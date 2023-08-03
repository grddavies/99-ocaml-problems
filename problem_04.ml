(* length of a list *)
let length l = 
  let rec go xs n =
    match xs with
    | [] -> n
    | y :: ys -> go ys (n + 1)
  in go l 0;;

let () = Printf.printf "%d\n" (length ["a"; "b"; "c"]);;
let () = Printf.printf "%d\n" (length ["a"; "b"]);;
let () = Printf.printf "%d\n" (length ["a"]);;
let () = Printf.printf "%d\n" (length []);;