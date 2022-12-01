(* Defining some custom definitions for my parsed evaluations *)

(* Passing in a list and a comparison function, find the value that dominates the others *)
let compare args func = 
  let rec compare_inner args dominator =
    match args with
    | [] -> dominator
    | value :: rest ->
    match dominator with
    | No -> compare_inner rest (Yes value)
    | Yes dom -> if func value dom then compare_inner rest (Yes value) else compare_inner rest dominator
  in match compare_inner args No with
  | Yes value -> value
  | No -> 0.


let min args = compare args (<);;
let max args = compare args (>);;

(* Magnitude of a variable amount of arguments *)
let mag2 args =
  let rec mag2_inner args acc =
    match args with
    | [] -> acc
    | hd :: tl -> mag2_inner tl (acc +. hd *. hd)
  in mag2_inner args 0.;;

let mag args = sqrt (mag2 args);;


