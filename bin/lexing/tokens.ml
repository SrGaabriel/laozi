type token =
  | INT of int
  | IDENTIFIER of string
  | PLUS
  | MINUS
  | TIMES
  | DIVISION
  | OPENING_PARENTHESIS
  | CLOSING_PARENTHESIS
  | OPENING_BRACKET
  | CLOSING_BRACKET
  | SEMICOLON
  | COMMA
  | IF
  | ELSE
  | TRUE
  | FALSE
  | EOF

let string_of_token = function
  | INT i -> "INT " ^ string_of_int i
  | IDENTIFIER s -> "IDENTIFIER " ^ s
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | DIVISION -> "DIVISION"
  | OPENING_PARENTHESIS -> "OPENING_PARENTHESIS"
  | CLOSING_PARENTHESIS -> "CLOSING_PARENTHESIS"
  | OPENING_BRACKET -> "OPENING_BRACKET"
  | CLOSING_BRACKET -> "CLOSING_BRACKET"
  | SEMICOLON -> "SEMICOLON"
  | COMMA -> "COMMA"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | EOF -> "EOF"