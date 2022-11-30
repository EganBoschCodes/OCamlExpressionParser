#use "expression_parser.ml"

let basic_variables a = 
  match a with
  | ['e'] -> 2.71828182845904509
  | ['p';'i'] -> 3.14159265358979
  | ['x'] -> 3.
  | ['y'] -> 4.
  | ['z'] -> 5.

let expr = parse (split "e^(-pi/2)")
let value = evaluate expr basic_variables