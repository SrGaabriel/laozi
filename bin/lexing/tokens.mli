type token =
  | INT of int
  | IDENTIFIER of string
  | STRING of string
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

val string_of_token : token -> string