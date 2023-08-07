(* run length encoding *)
let rec rle (h::t) =
  let rec aux acc n e l = match l with
  | [] -> (n, e) :: acc
  | x :: xs -> if x = e then aux acc (n + 1) e xs else aux ((n, e)::acc) 1 x xs
in aux [] 1 h t |> List.rev

(* --- *)

let pp_tuple (a, b) = Printf.sprintf "(%d, %s)" a b

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x::xs ->  (List.fold_left (fun s e -> s ^ Printf.sprintf "; %s " e) (Printf.sprintf "[ %s" x) xs) ^ "]"

let xs = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let main () = rle xs |> List.map pp_tuple |> list_to_str |> print_endline;;

main
