open Ast
(* helper *)

let add_tag tag content = Printf.sprintf "<%s>%s</%s>" tag content tag

(* core *)

let rec render_inline inline =
    let content = match inline with
        | Bold x | Italic x -> render_inline_list x
        | Text x -> x
    in
    match tag_of_inline inline with
        | Some tag -> add_tag tag content
        | None -> content

and render_inline_list items = match items with
    | h :: t -> render_inline h ^ render_inline_list t
    | [] -> ""

let rec render_unordered_list items =
    String.concat "\n" (List.map render_unordered_list_item items)

and render_unordered_list_item item =
    let content = render_inline_list item in
    add_tag "li" content

let render_block block = let tag = tag_of_block block in
    let rendered_sub = match block with
        | Heading (_, x) | Paragraph x -> render_inline_list x
        | UnorderedList x -> render_unordered_list x
    in
    match block with
        | Heading _ | Paragraph _ | UnorderedList _ ->
                add_tag tag rendered_sub

