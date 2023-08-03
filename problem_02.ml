let rec last_two xs =
  match xs with
  | [] | [ _ ] -> None
  | x :: [ y ] -> Some (x, y)
  | y::ys -> last_two ys;;


let str_from_option opt pfunc =
  match opt with
  | None -> "None"
  | Some x -> Printf.sprintf "Some %a" pfunc x

let pp_tuple (a, b) = Printf.sprintf "(%s, %s)" a b


let () = print_endline (str_from_option ( last_two ["a" ; "b"; "c"; "d"] ) (fun () -> pp_tuple));;

let () = print_endline (str_from_option ( last_two ["a"; "b"] ) (fun () -> pp_tuple));;

let () = print_endline (str_from_option ( last_two ["a";] ) (fun () -> pp_tuple));;
