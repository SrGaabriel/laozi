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

val to_string_binary_op : binary_op -> string

val to_string_node_type : node_type -> string

val to_string_node : node -> string

val new_node : node_type -> node