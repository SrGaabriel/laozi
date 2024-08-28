type binary_op =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Power

type node_type = 
  | Root
  | Text of string
  | Number of string * bool
  | BinaryOp of binary_op

type node = {
  node_type: node_type;
  children: node list;
}

let to_string_binary_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Power -> "^"

let to_string_node_type = function
  | Root -> "Root"
  | Text string -> string
  | Number (string, is_float) -> string ^ (if is_float then "f" else "")
  | BinaryOp op -> to_string_binary_op op


let rec to_string_node node =
  let type_str = to_string_node_type node.node_type in
  let children_str = List.map to_string_node node.children in
  match children_str with
  | [] -> type_str
  | _ -> type_str ^ "(" ^ (String.concat ", " children_str) ^ ")"

let new_node node_type = {
  node_type = node_type;
  children = [];
}