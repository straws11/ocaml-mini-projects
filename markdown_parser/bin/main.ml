open Markdown_parser.Parser
open Markdown_parser.Ast
open Markdown_parser.Renderer

let input =
    "# Hello\n\nThis is text**This is bold text**\nhi."

let () =
    print_endline ("Input: " ^ input ^ "\n");

    let ast = parse_input input in
    print_endline (string_of_block_list ast);
    let html = String.concat "\n" (List.map render_block ast) in
    print_endline ("HTML output:" ^ html);

