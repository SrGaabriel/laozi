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

val string_of_token_kind : token_kind -> string

val string_of_token : token -> string