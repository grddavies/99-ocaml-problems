(* Drop every N'th element from a list. *)
let drop list n =
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> if i = 1 then aux acc n t else aux (h :: acc) (i - 1) t
  in
  aux [] n list |> List.rev

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x :: xs ->
      List.fold_left
        (fun s e -> s ^ Printf.sprintf "; %s " e)
        (Printf.sprintf "[ %s" x) xs
      ^ "]"

let () =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  |> list_to_str |> print_endline
