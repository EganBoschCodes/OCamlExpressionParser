#use "string_utils.ml"
#use "math.ml"

type ('const, 'symbol) expression =
  | Constant of 'const
  | Symbol of 'symbol
  | Operator of (float maybe -> float maybe -> float maybe) * ('const, 'symbol) expression * ('const, 'symbol) expression
  | Function of (float maybe list -> float maybe) * (('const, 'symbol) expression list)


let rec strip_paren str = 
  let is_just_paren str =
    match str with 
    | [] -> false
    | a :: b ->
    match a with
    | '(' -> (
      let rec rec_track_depth str depth =
        match depth with
        | 0 -> false
        | _ ->
        match str with
        | [] -> false
        | [')'] -> true
        | a :: b ->
        match a with
        | '(' -> rec_track_depth b (depth + 1)
        | ')' -> rec_track_depth b (depth - 1)
        | _ -> rec_track_depth b depth
      in rec_track_depth b 1
    )
    | _ -> false
  in match is_just_paren str with
  | true -> strip_paren (strip_ends str)
  | false -> str

let is a = match a with | Yes _ -> true | No -> false

let wrap_single func = fun a -> match a with | Yes value -> Yes (func value) | No -> No
let wrap_double func = fun a b -> match a with | Yes aval -> (match b with | Yes bval -> Yes (func aval bval) | No -> No) | No -> No

let operator op = match op with
  | ['+'] -> wrap_double (fun a b -> a +. b)
  | ['-'] -> wrap_double (fun a b -> a -. b)
  | ['*'] -> wrap_double (fun a b -> a *. b)
  | ['/'] -> wrap_double (fun a b -> a /. b)
  | ['^'] -> wrap_double (fun a b -> a ** b)
  | ['%'] -> wrap_double (fun a b -> a -. (Float.floor (a /. b)) *. b)
  | ['&';'&'] -> wrap_double (fun a b -> match (a != 0. && b != 0.) with | true -> 1. | false -> 0.)
  | ['|';'|'] -> wrap_double (fun a b -> match (a != 0. || b != 0.) with | true -> 1. | false -> 0.)
  | ['<'] -> wrap_double (fun a b -> match (a < b) with | true -> 1. | false -> 0.)
  | ['>'] -> wrap_double (fun a b -> match (a > b) with | true -> 1. | false -> 0.)
  | _ -> fun a b -> No

let rec unwrap l = 
  match l with
  | [] -> []
  | a :: b ->
  match a with
  | Yes value -> value :: (unwrap b)
  | No -> unwrap b

(* Take a (float list -> float) and make it (float maybe list -> float maybe) *)
let wrap_list func = fun l -> 
  match collapse_list (&&) (apply_to_all is l) with
  | true -> Yes (func (unwrap l))
  | false -> No

(* Take a (float -> float) and make it (float maybe list -> float maybe) *)
let wrap_into_list_single func = fun l ->
  match unwrap l with
  | [] -> No
  | a :: [] -> Yes (func a)
  | _ -> No

let wrap_into_list_double func = fun l ->
  match unwrap l with
  | [] -> No
  | a :: (b :: []) -> Yes (func a b)
  | _ -> No

let functions f = match f with
  | ['m';'a';'x'] -> wrap_list max
  | ['m';'i';'n'] -> wrap_list min
  | ['a';'b';'s'] -> wrap_into_list_single abs_float
  | ['s';'q';'r';'t'] -> wrap_into_list_single sqrt
  | ['s';'i';'n'] -> wrap_into_list_single sin
  | ['c';'o';'s'] -> wrap_into_list_single cos
  | ['e';'x';'p'] -> wrap_into_list_single exp
  | ['l';'n'] -> wrap_into_list_single log
  | ['a';'t';'a';'n'] -> wrap_into_list_single atan
  | ['a';'t';'a';'n';'2'] -> wrap_into_list_double atan2
  | ['m';'a';'g'] -> wrap_list mag
  | ['m';'a';'g';'2'] -> wrap_list mag2
  | _ -> fun _ -> No

let rec parse str =
  let expr = strip_paren str in
  match length expr with
  | 0 -> Constant (No)
  | _ ->
  match parse_float expr with
  | Yes f -> Constant (Yes f)
  | No -> 
  match is_valid_variable expr with
  | true -> Symbol expr
  | false -> 

  let rec search_for_operators operators = 
    (match operators with
    | [] -> No
    | ops :: rest ->
    match first_occurance expr ops with
    | Yes (op, index) -> Yes (Operator ((operator op), (parse (trim_after expr index)), (parse (trim_before expr (index + (length op))))))
    | No -> search_for_operators rest)

  in match search_for_operators [[split "&&";split "||"]; [['<'];['>']]; [['+'];['-']]] with
  | Yes op -> op
  | No -> 
  match starts_with expr ['-'] with
  | true -> Operator ((operator ['*']), Constant (Yes (-1.)), (parse (trim_before expr 1)))
  | false -> 
  match search_for_operators [[['*'];['/']]; [['^']]; [['%']]] with
  | Yes op -> op
  | No ->
  match parse_function str with
  | Yes (name, args) -> Function ((functions name), apply_to_all parse args)
  | No -> Constant (No)

let rec evaluate expr variables =
  match expr with
  | Constant value -> value
  | Symbol sym -> Yes (variables sym)
  | Operator (op, left, right) -> op (evaluate left variables) (evaluate right variables)
  | Function (func, args) -> func (apply_to_all (fun arg -> evaluate arg variables) args)