(* Insert an Element at a Given Position Into a List *)
let rec insert_at e i = function
  | [] -> [ e ]
  | h :: t -> if i = 0 then e :: h :: t else h :: insert_at e (i - 1) t
