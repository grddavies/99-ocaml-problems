(* Pack consecutive duplicates of list elements into sublists. *)
let pack list =
  let rec aux acc run xs = match xs with
  | [] -> acc @ [run]
  | y :: ys -> if run == [] || y = (List.hd run) then aux acc (y :: run) ys else aux (acc @ [run]) [y] ys
in aux [] [] list

let xs = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;


let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x::xs ->  (List.fold_left (fun s e -> s ^ Printf.sprintf "; %s " e) (Printf.sprintf "[ %s" x) xs) ^ "]"

let () = pack xs |> List.map list_to_str |> list_to_str |> print_endline;;