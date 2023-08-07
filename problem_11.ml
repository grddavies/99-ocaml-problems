(* modified run length encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let rlencode list =
  let as_rle n elem =
    if n = 0 then One elem
    else Many (n, elem) in
  let rec aux n acc = function
  | [] -> []
  | [x] -> (as_rle (n + 1) x) :: acc
  | a :: (b :: _ as t) -> if a = b then aux (n + 1) acc t
                          else aux 0 ((as_rle (n + 1) a) :: acc) t in
  List.rev (aux 0 [] list);;


let pp_rle = function
| One s -> Printf.sprintf "One \"%s\"" s
| Many (n, s) -> Printf.sprintf "Many (%d, \"%s\")" n s

let list_to_str = function
  | [] -> "[]"
  | [ x ] -> Printf.sprintf "[ %s ]" x
  | x::xs ->  (List.fold_left (fun s e -> s ^ Printf.sprintf "; %s " e) (Printf.sprintf "[ %s" x) xs) ^ "]"

let xs = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let () = rlencode xs |> List.map pp_rle |> list_to_str |> print_endline;;