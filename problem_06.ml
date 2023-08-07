(* is a list a palindrome *)

let rev list =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x::xs -> aux (xs) (x::acc)
  in aux list [];;

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq

let is_palindrome list = List.equal (fun x y -> x == y) list (rev list)

let print_bool x = print_endline (if x then "true" else "false")

let () = is_palindrome ["a"; "b"; "b"; "c"] |> Bool.to_string |> print_endline

let () = "raccoon" |> string_to_char_list |> is_palindrome |> Bool.to_string |> print_endline

let () = "racecar" |> string_to_char_list |> is_palindrome |> Bool.to_string |> print_endline

let () = "yobananaboy" |> string_to_char_list |> is_palindrome |> print_bool

