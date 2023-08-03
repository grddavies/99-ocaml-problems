let rec nth n l = 
  match l with
  | [] -> None
  | x :: xs -> if n == 0 then Some x else nth (n-1) xs 

let str_from_option opt =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" x


let () = print_endline (str_from_option ( nth 0 ["a" ; "b"; "c"] ));;
let () = print_endline (str_from_option ( nth 1 ["a" ; "b"; "c"] ));;
let () = print_endline (str_from_option ( nth 2 ["a" ; "b"; "c"] ));;
let () = print_endline (str_from_option ( nth 3 ["a" ; "b"; "c"] ));;
let () = print_endline (str_from_option ( nth 3 [] ));;
