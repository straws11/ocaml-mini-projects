open Mini_interpreter

let () =
    print_string "Input:";
    flush stdout;
    let str = read_line () in
    let chars = List.init (String.length str) (String.get str) in
    let tokens = tokenize chars in
    let (ast, rem) = parse_expr tokens in
    let res = evaluate ast in

    let outstr = "[" ^ String.concat "; " (List.map string_of_op tokens) ^ "]" in
    print_endline ("Tokenization Output: " ^ outstr);
    print_endline ("Parsing Output: " ^ string_of_expr ast);
    print_endline ("Final Output: " ^ string_of_int res)
