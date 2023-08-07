(* flatten a nested list structure *)

open Printf

type 'a node = 
  | One of 'a
  | Many of 'a node list

let xs = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let flatten nodelist =
  let rec aux xs acc = match xs with
  | [] -> acc
  | (One y) :: ys -> aux ys (acc @ [y])
  | (Many ys) :: rest -> aux rest (aux ys acc)
in aux nodelist [];;

let print_list list = 
  match list with
  | [] -> print_string "[]\n"
  | [ x ] -> printf "[ %s ]\n" x
  | x::xs -> printf "[ %s" x; List.iter (printf "; %s ") xs; print_string "]\n";;

let () = flatten xs |> print_list
