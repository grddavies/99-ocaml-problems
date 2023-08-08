(* Create a List Containing All Integers Within a Given Range *)
(* If first argument is greater than second, produce a list in decreasing order. *)
let range start finish =
  let d = if start < finish then 1 else -1 in
  let rec aux acc i finish =
    if i = finish then i :: acc else aux (i :: acc) (i + d) finish
  in
  List.rev (aux [] start finish)
