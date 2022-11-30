let compare l func = 
  let rec compare_inner l mval =
    match l with
    | [] -> mval
    | a :: rest ->
    match mval with
    | No -> compare_inner rest (Yes a)
    | Yes b -> if func a b then compare_inner rest (Yes a) else compare_inner rest mval
  in match compare_inner l No with
  | Yes v -> v
  | No -> 0.

let min l = compare l (<);;
let max l = compare l (>);;

let mag2 l =
  let rec mag2_inner l acc =
    match l with
    | [] -> acc
    | hd :: tl -> mag2_inner tl (acc +. hd *. hd)
  in mag2_inner l 0.;;

let mag l = sqrt (mag2 l);;


