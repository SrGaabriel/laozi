
let read_file_text filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    ""
  with End_of_file ->
    close_in chan;
    String.concat "\n" !lines

let () =
  let filename = "example.lao" in
  let text = read_file_text filename in
  let lexed = Lexing.Lexer.lexer text 0 in
  List.iter (fun token -> print_endline (Lexing.Tokens.string_of_token token)) lexed;;
  
  let root = Parsing.Nodes.new_node (Parsing.Nodes.Text "hello, world!") in
  let str = Parsing.Nodes.to_string_node root in
  print_endline str