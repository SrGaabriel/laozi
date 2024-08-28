let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let rec lexer input pos =
  if pos >= String.length input then
    [{ Tokens.kind = EOF; value = ""; position = pos }]
  else
    let char = String.get input pos in
    match char with
    | ' ' | '\n' | '\t' -> lexer input (pos + 1)
    | '(' -> { Tokens.kind = OPENING_PARENTHESIS; value = "("; position = pos } :: lexer input (pos + 1)
    | ')' -> { Tokens.kind = CLOSING_PARENTHESIS; value = ")"; position = pos } :: lexer input (pos + 1)
    | '[' -> { Tokens.kind = OPENING_BRACKET; value = "["; position = pos } :: lexer input (pos + 1)
    | ']' -> { Tokens.kind = CLOSING_BRACKET; value = "]"; position = pos } :: lexer input (pos + 1)
    | ';' -> { Tokens.kind = SEMICOLON; value = ";"; position = pos } :: lexer input (pos + 1)
    | ',' -> { Tokens.kind = COMMA; value = ","; position = pos } :: lexer input (pos + 1)
    | '+' -> { Tokens.kind = PLUS; value = "+"; position = pos } :: lexer input (pos + 1)
    | '-' -> { Tokens.kind = MINUS; value = "-"; position = pos } :: lexer input (pos + 1)
    | '*' -> { Tokens.kind = TIMES; value = "*"; position = pos } :: lexer input (pos + 1)
    | '/' -> { Tokens.kind = DIVISION; value = "/"; position = pos } :: lexer input (pos + 1)
    | '"' ->
      let rec read_string acc pos =
        if pos < String.length input && String.get input pos <> '"' then
          read_string (acc ^ String.make 1 (String.get input pos)) (pos + 1)
        else
          acc, pos
      in
      let string, new_pos = read_string "" (pos + 1) in
      { Tokens.kind = STRING; value = string; position = pos } :: lexer input (new_pos + 1)
    | _ ->
      if is_digit char then
        let rec read_number acc pos =
          if pos < String.length input && is_digit (String.get input pos) then
            read_number (acc ^ String.make 1 (String.get input pos)) (pos + 1)
          else
            acc, pos
        in
        let number, new_pos = read_number (String.make 1 char) (pos + 1) in
        { Tokens.kind = INT; value = number; position = pos } :: lexer input new_pos
      else if is_letter char then
        let rec read_identifier acc pos =
          if pos < String.length input && (is_letter (String.get input pos) || is_digit (String.get input pos)) then
            read_identifier (acc ^ String.make 1 (String.get input pos)) (pos + 1)
          else
            acc, pos
        in
        let identifier, new_pos = read_identifier (String.make 1 char) (pos + 1) in
        let kind = match identifier with
          | "if" -> Tokens.IF
          | "else" -> Tokens.ELSE
          | "true" -> Tokens.TRUE
          | "false" -> Tokens.FALSE
          | _ -> Tokens.IDENTIFIER
        in
        { Tokens.kind = kind; value = identifier; position = pos } :: lexer input new_pos
      else
        failwith ("Unexpected character: " ^ String.make 1 char)