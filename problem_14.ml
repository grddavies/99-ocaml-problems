(* Duplicate the elements of a list. *)
let rec duplicate = function [] -> [] | x :: xs -> x :: x :: duplicate xs

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x :: xs ->
      List.fold_left
        (fun s e -> s ^ Printf.sprintf "; %s " e)
        (Printf.sprintf "[ %s" x) xs
      ^ "]"

let xs = [ "a"; "b"; "c"; "c"; "d" ]
let () = duplicate xs |> list_to_str |> print_endline
