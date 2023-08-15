(* Take the average execution time (seconds) of 5 runs of a function call *)
let timeit f a =
  let n = 5 in
  let tcall f a =
    let t0 = Sys.time() in
    ignore (f a);
    let t1 = Sys.time() in
  (* notice trailing dot for floating point op *)
  t1 -. t0 in
  let res = List.of_seq (Seq.take n (Seq.repeat (tcall f a))) in
  List.fold_left ( +. ) 0.0 res /. float_of_int (List.length res)
