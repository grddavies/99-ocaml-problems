(* Decode a run length encoded list *)
type 'a rle = One of 'a | Many of int * 'a

let xs =
  [
    Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e");
  ]

let decode list =
  let repeat n x = Seq.repeat x |> Seq.take n |> List.of_seq in
  let expand = function One x -> [ x ] | Many (n, x) -> repeat n x in
  let rec aux acc = function
    | [] -> []
    | [ x ] -> acc @ expand x
    | x :: xs -> aux (acc @ expand x) xs
  in
  aux [] list

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x :: xs ->
      List.fold_left
        (fun s e -> s ^ Printf.sprintf "; %s " e)
        (Printf.sprintf "[ %s" x) xs
      ^ "]"

let () = decode xs |> list_to_str |> print_endline
