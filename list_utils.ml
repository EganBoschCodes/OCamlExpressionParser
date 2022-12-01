(* Monad Utilities *)
type ('exists) maybe =
  | Yes of 'exists
  | No

let is a = match a with | Yes _ -> true | No -> false

(* Takes a list of maybes and returns a list of the values held by the Yeses *)
(* Ex: [Yes 1; No 3; Yes 1] -> [1; 2] *)
let rec unwrap l = 
  match l with
  | [] -> []
  | a :: b ->
  match a with
  | Yes value -> value :: (unwrap b)
  | No -> unwrap b




(* Returns a list where a function is applied to every element of a given list. *)
let rec apply_to_all f a =
  match a with
  | [] -> []
  | hd :: tl -> (f hd) :: (apply_to_all f tl);;

let reverse_list input =
  let rec reverse_internal inp out =  
    match inp with
    | [] -> out
    | hd :: tl -> reverse_internal tl (hd :: out)
  in reverse_internal input [];;

(* Count number of occurances of an element in a list *)
let count arr element =
  let rec count_acc arr element acc =
    match arr with
    | [] -> acc
    | first :: rest ->
    match first = element with
    | true -> count_acc rest element (acc + 1)
    | false -> count_acc rest element acc
  in count_acc arr element 0

(* Given a list of items and a function that takes in two vals from the list and outputs another item of the list type, collapse to the end. Useful for && and || on boolean arrays. *)
let rec collapse_list f a =
  match a with 
  | hd :: [] -> hd
  | hd :: tl -> f hd (collapse_list f tl);;