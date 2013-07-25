
module L = BatList

open Printf

(* FBR: put this one in BatList *)
let i_to_j i j =
  if i > j then invalid_arg (sprintf "i_to_j: %d %d" i j);
  let rec loop acc k =
    if i = k then
      k :: acc
    else
      loop (k :: acc) (k - 1)
  in
  loop [] j

let n_times = 1_000_000

(* many lists of increasing length *)
let lists =
  (L.map
     (fun n -> L.make (int_of_float (2.**n)) 1)
     (L.map float_of_int (i_to_j 0 14)))

let main () =
  printf "#list_length f_runtime g_runtime\n%!";
  L.iter
    (fun l ->
      (* time running the function f n times *)
      let f_start = Unix.gettimeofday () in
      for i = 1 to n_times do
        ignore(L.hd l)
      done;
      let f_stop = Unix.gettimeofday () in
      (* time running the function g n times *)
      let g_start = Unix.gettimeofday () in
      for i = 1 to n_times do
        ignore(L.tl l)
      done;
      let g_stop = Unix.gettimeofday () in
      (* output in a format ready for gnuplot *)
      printf "%d %f %f\n%!"
        (L.length l) (f_stop -. f_start) (g_stop -. g_start)
    )
    lists
;;

main()
