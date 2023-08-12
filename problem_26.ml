(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List  *)
let rec choose k list =
  if k = 0 then [[]]
  else match list with
    | [] -> []
    | h :: t ->
        let with_h = List.map (fun l -> h :: l) (choose (k - 1) t) in
        let without_h = choose k t in
        with_h @ without_h

