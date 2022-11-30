type ('exists) maybe =
  | Yes of 'exists
  | No


(* Most Basic String Utilities *)

(* Convert string into char list *)
let rec split str = 
  match String.length str with
  | 0 -> []
  | _ ->
  match str.[0] with
  | ' ' -> split (String.sub str 1 ((String.length str) - 1))
  | c -> c :: split (String.sub str 1 ((String.length str) - 1))

let rec length str = 
  match str with
  | [] -> 0
  | a :: b -> 1 + (length b)

(* Returns a list where a function is applied to every element of a given list. *)
let rec apply_to_all f a =
  match a with
  | [] -> []
  | hd :: tl -> (f hd) :: (apply_to_all f tl);;

let rec starts_with str op =
  match op with
  | [] -> true
  | op_start :: op_rest ->
    match str with
    | [] -> false
    | str_start :: str_rest ->
      match op_start = str_start with
      | true -> starts_with str_rest op_rest
      | false -> false

let rec contains str op = 
  match str with
  | [] -> false
  | head :: rest -> 
    match starts_with str op with
    | true -> true
    | false -> contains rest op

let index_of str op =
  let rec index_of_int str index =
    match starts_with str op with
    | true -> Yes index
    | false -> 
      match str with
      | [] -> No
      | _ :: rest -> index_of_int rest (index + 1)
  in index_of_int str 0

let reverse_list input =
  let rec reverse_internal inp out =  
    match inp with
    | [] -> out
    | hd :: tl -> reverse_internal tl (hd :: out)
  in reverse_internal input [];;

(* Substrings *)

let trim_before str start =
  let rec accumulate str start index =
    match str with
    | [] -> []
    | c :: rest ->
    match index >= start with
    | true -> c :: (accumulate rest start (index + 1))
    | false -> accumulate rest start (index + 1)
  in accumulate str start 0

let trim_after str ending =
  let rec accumulate str ending index =
    match str with
    | [] -> []
    | c :: rest ->
    match index < ending with
    | true -> c :: (accumulate rest ending (index + 1))
    | false -> accumulate rest ending (index + 1)
  in accumulate str ending 0

let substring str start ending = trim_before (trim_after str ending) start
  
let split_on str delimiter =
  let rec accumulate str splits current depth =
    match str with
    | [] -> current :: splits
    | c :: rest ->
    match c = '(' with
    | true -> accumulate rest splits (c :: current) (depth + 1)
    | false -> 
    match c = ')' with
    | true -> accumulate rest splits (c :: current) (depth - 1)
    | false -> 
    match depth = 0 && starts_with str delimiter with
    | false -> accumulate rest splits (c :: current) depth
    | true -> accumulate (trim_before str (length delimiter)) (current :: splits) [] depth
  in reverse_list (apply_to_all reverse_list (accumulate str [] [] 0))

(* Finds which in a list of operators occurs first outside of parenthesis, if they occur at all *)
let first_occurance str ops =
  let rec first_occurance_index str ops index depth = 
    let rec starts_on_ops str ops =
      match ops with
      | [] -> No
      | op :: rest ->
        match starts_with str op with
        | true -> Yes op
        | false -> starts_on_ops str rest
    in match depth with
    | 0 ->
      (match starts_on_ops str ops with
        | Yes op -> if index > 0 then Yes (op, index) else (match str with | [] -> No | start :: rest -> first_occurance_index rest ops (index + 1) depth)
        | No -> 
          match str with
          | [] -> No
          | start :: rest -> 
          match start with
          | '(' -> first_occurance_index rest ops (index + 1) (depth + 1)
          | ')' -> first_occurance_index rest ops (index + 1) (depth - 1)
          | _ -> first_occurance_index rest ops (index + 1) depth)
    | _ -> 
    (match str with
      | [] -> No
      | start :: rest -> 
      match start with 
      | '(' -> first_occurance_index rest ops (index + 1) (depth + 1)
      | ')' -> first_occurance_index rest ops (index + 1) (depth - 1)
      | _ -> first_occurance_index rest ops (index + 1) depth)
        
  in first_occurance_index str ops 0 0

(* Removes first and last characters of a string *)
let strip_ends str =
  let rec strip_last str =
    match str with
    | [] -> []
    | [a] -> []
    | a :: b -> a :: (strip_last b)
  in match str with
  | [] -> []
  | a :: b -> strip_last b







(* Given a list of items and a function that takes in two vals from the list and outputs another item of the list type, collapse to the end. Useful for && and || on boolean arrays. *)
let rec collapse_list f a =
  match a with 
  | hd :: [] -> hd
  | hd :: tl -> f hd (collapse_list f tl);;

let is_alpha c =
  let code = Char.code(c) in (code > 96 && code < 123) || (code > 64 && code < 91)

let is_numeric c =
  let code = Char.code(c) in (code > 47 && code < 58)

let is_alpha_numeric c = (is_alpha c) || (is_numeric c)

let is_valid_variable str = collapse_list (&&) (apply_to_all is_alpha_numeric str);;


(* Parsing Floats *)

let count str c =
  let rec count_acc str c acc =
    match str with
    | [] -> acc
    | a :: b ->
    match a = c with
    | true -> count_acc b c (acc + 1)
    | false -> count_acc b c acc
  in count_acc str c 0

let rec can_be_parsed str =
  match str with
  | [] -> false
  | start :: rest -> 
  match start with
  | '-' -> (match index_of rest ['-'] with | No -> (can_be_parsed rest) | _ -> false)
  | _ ->
  match index_of str ['.'] with
  | No -> collapse_list (&&) (apply_to_all is_numeric str)
  | Yes decimal_index -> (count str '.') = 1 && (decimal_index > 0) && (decimal_index < (length str) - 1) && (collapse_list (&&) (apply_to_all (fun c -> (is_numeric c) || (c = '.')) str))

let parse_char c = float_of_int (Char.code(c) - 48)

let rec parse_float str = 
  match can_be_parsed str with
  | false -> No
  | true ->
  match str with
  | '-' :: tl -> (match parse_float tl with | No -> No | Yes f -> Yes (-1. *. f))
  | _ ->
  let decimal_index = (match index_of str ['.'] with
  | No -> length str
  | Yes di -> di) in
  let rec accumulate str index acc =
    match str with
    | [] -> acc
    | c :: rest ->
    match c = '.' with
    | true -> accumulate rest index acc
    | false ->
    let place = parse_char c in
    let pow = decimal_index - index - 1 in
      accumulate rest (index + 1) (acc +. place *. (10.0 ** (float_of_int pow)))
  in Yes (accumulate str 0 0.0)



(* Parsing function-formatted strings into (functionName, arguments) *)
let parse_function str =
  match index_of str ['('] with
  | No -> No
  | Yes index ->
  match reverse_list str with
  | [] -> No
  | last_char :: _ ->
  match last_char = ')' with
  | false -> No
  | true -> 
  let function_name = trim_after str index in
  let args = strip_ends (trim_before str (index)) in
  Yes (function_name, split_on args [',']);;