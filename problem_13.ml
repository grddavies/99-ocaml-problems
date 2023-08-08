(* run length encoding (Direct Solution) *)
let rec rle list =
  let rec aux n acc = function
    | [] -> []
    | [ x ] -> (n + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (n + 1) acc t else aux 0 ((n + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)

(* --- *)

let pp_tuple (a, b) = Printf.sprintf "(%d, %s)" a b

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x :: xs ->
      List.fold_left
        (fun s e -> s ^ Printf.sprintf "; %s " e)
        (Printf.sprintf "[ %s" x) xs
      ^ "]"

let xs =
  [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]

let () = rle xs |> List.map pp_tuple |> list_to_str |> print_endline
