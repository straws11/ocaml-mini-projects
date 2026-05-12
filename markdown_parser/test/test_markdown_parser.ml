open Markdown_parser.Parser

let test_get_heading_size_1 () =
    let (size, heading_text) = get_heading_size ("### Heading" |> String.to_seq |> List.of_seq) in
    assert ( size = 3 )

let () =
    test_get_heading_size_1 ()
