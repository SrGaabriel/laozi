let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let rec lexer input pos =
  if pos >= String.length input then
    [Tokens.EOF]
  else
    let char = String.get input pos in
    match char with
    | ' ' | '\n' | '\t' -> lexer input (pos + 1)
    | '(' -> Tokens.OPENING_PARENTHESIS :: lexer input (pos + 1)
    | ')' -> Tokens.CLOSING_PARENTHESIS :: lexer input (pos + 1)
    | '[' -> Tokens.OPENING_BRACKET :: lexer input (pos + 1)
    | ']' -> Tokens.CLOSING_BRACKET :: lexer input (pos + 1)
    | ';' -> Tokens.SEMICOLON :: lexer input (pos + 1)
    | ',' -> Tokens.COMMA :: lexer input (pos + 1)
    | '+' -> Tokens.PLUS :: lexer input (pos + 1)
    | '-' -> Tokens.MINUS :: lexer input (pos + 1)
    | '*' -> Tokens.TIMES :: lexer input (pos + 1)
    | '/' -> Tokens.DIVISION :: lexer input (pos + 1)
    | _ ->
      if is_digit char then
        let rec read_number acc pos =
          if pos < String.length input && is_digit(String.get input pos) then
            read_number (acc ^ String.make 1 (String.get input pos)) (pos + 1)
          else
            acc, pos
        in
        let number, new_pos = read_number (String.make 1 char) (pos + 1) in
        Tokens.INT(int_of_string number) :: lexer input new_pos
      else if is_letter char then
        let rec read_identifier acc pos =
          if pos < String.length input && (is_letter (String.get input pos) || is_digit (String.get input pos)) then
            read_identifier (acc ^ String.make 1 (String.get input pos)) (pos + 1)
          else
            acc, pos
        in
        let identifier, new_pos = read_identifier (String.make 1 char) (pos + 1) in
        match identifier with
        | "if" -> Tokens.IF :: lexer input new_pos
        | "else" -> Tokens.ELSE :: lexer input new_pos
        | "true" -> Tokens.TRUE :: lexer input new_pos
        | "false" -> Tokens.FALSE :: lexer input new_pos
        | _ -> Tokens.IDENTIFIER identifier :: lexer input new_pos
      else
        failwith ("Unexpected character: " ^ String.make 1 char)