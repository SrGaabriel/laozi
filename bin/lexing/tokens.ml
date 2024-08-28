type token_kind =
  | INT
  | IDENTIFIER
  | STRING
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

type token = {
  kind: token_kind;
  value: string;
  position: int;
}

let string_of_token_kind = function
  | INT -> "INT"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
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

let string_of_token = fun token ->
  Printf.sprintf "{ kind: %s, value: %s, position: %d }"
    (string_of_token_kind token.kind)
    token.value
    token.position