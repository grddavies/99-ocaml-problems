open Printf

(* reverse a list *)
let rev list =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x::xs -> aux (xs) (x::acc)
  in aux list [];;

let print_list list = 
  match list with
  | [] -> print_string "[]\n"
  | [ x ] -> printf "[ %d ]\n" x
  | x::xs -> printf "[ %d" x; List.iter (printf "; %d ") xs; print_string "]\n";;

let () = print_list (rev [3; 2; 1])
let () = print_list (rev [1])
let () = print_list (rev [])