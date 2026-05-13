open Markdown_parser.Parser
open Markdown_parser.Ast
open Markdown_parser.Renderer

let read_file name = In_channel.with_open_text name In_channel.input_lines

let write_file name content = Out_channel.with_open_text name (fun oc ->
        Out_channel.output_string oc content)

let () =
    let file_name = if Array.length Sys.argv > 1 then
        Sys.argv.(1)
    else
        failwith "Provide markdown file path to run"
    in

    let lines = read_file file_name in

    let ast = parse_input lines in
    print_endline ("Generated AST");
    print_endline ("---------------");
    print_endline (string_of_block_list ast);
    print_endline ("---------------\n");
    let html = String.concat "\n" (List.map render_block ast) in

    let output_name = if Array.length Sys.argv > 2 then
        Sys.argv.(2)
    else
        let parts = String.split_on_char '.' file_name in
        List.hd parts ^ ".html"
    in

    print_endline ("HTML output:" ^ html);

    write_file output_name html;

    print_endline ("File saved to " ^ output_name);
