(* eliminate consecutive duplicates in a list *)
let rec drop_while pred l = match l with
| [] -> []
| x :: xs -> if pred x then drop_while pred xs else x::xs;;

let compress l =
  let rec aux acc xs = match xs with
  | [] -> List.rev acc
  | x::xs -> aux (x :: acc) (drop_while (fun y -> y = x) xs)
in aux [] l;;

let xs = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let print_string_list list = 
  match list with
  | [] -> print_string "[]\n"
  | [ x ] -> Printf.printf "[ %s ]\n" x
  | x::xs -> Printf.printf "[ %s" x; List.iter (Printf.printf "; %s ") xs; print_string "]\n";;

let () = compress xs |> print_string_list