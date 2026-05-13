open Mini_interpreter

let gen_ast str =
  let chars = List.init (String.length str) (String.get str) in
  let (ast, _) = parse_expr (tokenize (chars)) in
  ast


let test_simple () =
  assert (evaluate (gen_ast "1+2") = 3)

let test_2simple () =
  assert (evaluate (gen_ast "3+2") = 5)

let () =
  test_simple ();
  test_2simple ()
