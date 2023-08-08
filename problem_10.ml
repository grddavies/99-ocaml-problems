(* Run length encoding (using pack) *)

let pack list =
  let rec aux acc run xs =
    match xs with
    | [] -> acc @ [ run ]
    | y :: ys ->
        if run == [] || y = List.hd run then aux acc (y :: run) ys
        else aux (acc @ [ run ]) [ y ] ys
  in
  aux [] [] list

let encode = function
  | [] -> []
  | xs -> pack xs |> List.map (fun xs -> (List.length xs, List.hd xs))

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

let () = encode xs |> List.map pp_tuple |> list_to_str |> print_endline
