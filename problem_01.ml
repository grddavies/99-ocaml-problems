(* Find the last element of a list *)
let rec last xs = 
  match xs with
  | [] -> None
  | [ x ] -> Some x
  | x::xs -> last xs;;

let str_from_option opt =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %s" x

let () = print_endline (str_from_option ( last ["a" ; "b"; "c"] ));;

let () = print_endline (str_from_option ( last [] ));;