(* Replicate the elements of a list a given number of times. *)
let replicate list n =
  let rec repeat n e acc = if n = 0 then acc else repeat (n - 1) e (e :: acc) in
  let rec aux n acc = function
    | [] -> acc
    | x :: xs -> aux n (acc @ repeat n x []) xs
  in
  aux n [] list

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x :: xs ->
      List.fold_left
        (fun s e -> s ^ Printf.sprintf "; %s " e)
        (Printf.sprintf "[ %s" x) xs
      ^ "]"

let () = replicate [ "a"; "b"; "c" ] 3 |> list_to_str |> print_endline
