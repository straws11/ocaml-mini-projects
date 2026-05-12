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

let render_block block = let tag = tag_of_block block in
    match block with
        | Heading (_, x) | Paragraph x ->
                add_tag tag (render_inline_list x)

