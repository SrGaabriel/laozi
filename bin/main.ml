
let read_file filename =
  let in_channel = open_in filename in
  try
    let rec read_lines () =
      try
        let line = input_line in_channel in
        line :: read_lines ()
      with End_of_file -> []
    in
    let lines = read_lines () in
    close_in in_channel;
    lines
  with e ->
    close_in_noerr in_channel;
    raise e

let () =
  let filename = "example.lao" in
  let lines = read_file filename in
  List.iter print_endline lines
