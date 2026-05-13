type inline =
    | Text of string
    | Bold of inline list
    | Italic of inline list
    | Code of string

type list_item = inline list

type block =
    | Heading of int * inline list
    | Paragraph of inline list
    | UnorderedList of list_item list
    | CodeBlock of string option * string

type block_kind =
    | HeadingKind
    | ParagraphKind
    | UnorderedListKind
    | CodeBlockKind

let tag_of_inline inline = match inline with
    | Text _ -> None
    | Bold _ -> Some "strong"
    | Italic _ -> Some "i"
    | Code _ -> Some "code"

let tag_of_block block = match block with
    | Heading (size, _) -> "h" ^ string_of_int size
    | Paragraph _ -> "p"
    | UnorderedList _ -> "ul"
    | CodeBlock _ -> "code"

let rec string_of_inline inline = match inline with
    | Text x -> "Text(" ^ x ^ ")"
    | Bold x -> "Bold(" ^ string_of_inline_list x ^ ")"
    | Italic x -> "Italic(" ^ string_of_inline_list x ^ ")"
    | Code x -> "InlineCode(" ^ x ^ ")"

and string_of_inline_list x =
    "[" ^ String.concat ", " (List.map string_of_inline x) ^ "]"

and string_of_list_item list_item =
    "ListItem(" ^ string_of_inline_list list_item ^ ")"

let string_of_block block = match block with
    | Heading (num, inline_list) -> "Heading(size=" ^ string_of_int num ^ ", "
        ^ string_of_inline_list inline_list ^ ")"
    | Paragraph x -> "Paragraph(" ^ string_of_inline_list x ^ ")"
    | UnorderedList x -> "UnorderedList(" ^ (String.concat ", " (List.map string_of_list_item x)) ^ ")"
    | CodeBlock (lang, text) -> "Code(" ^ begin match lang with
        | Some name -> name
        | None -> ""
        end
        ^ ", " ^ text ^ ")"

let rec string_of_block_list blocks =
    String.concat "\n" (List.map string_of_block blocks)
