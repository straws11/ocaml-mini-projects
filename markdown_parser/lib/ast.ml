type inline =
    | Text of string
    | Bold of string (* was inline list *)
    | Italic of string (* was inline list *)

type block =
    | Heading of int * inline list
    | Paragraph of inline list

let tag_of_inline inline = match inline with
    | Text _ -> None
    | Bold _ -> Some "strong"
    | Italic _ -> Some "i"

let tag_of_block block = match block with
    | Heading (size, _) -> "h" ^ string_of_int size
    | Paragraph _ -> "p"

let rec string_of_inline inline = match inline with
    | Text x -> "Text(" ^ x ^ ")"
    | Bold x -> "Bold(" ^ x ^ ")"
    | Italic x -> "Italic(" ^ x ^ ")"

and string_of_inline_list x =
    "[" ^ String.concat ", " (List.map string_of_inline x) ^ "]"

let string_of_block block = match block with
    | Heading (num, inline_list) -> "Heading(size=" ^ string_of_int num ^ ", "
        ^ string_of_inline_list inline_list ^ ")"
    | Paragraph x -> "Paragraph(" ^ string_of_inline_list x ^ ")"

let rec string_of_block_list blocks =
    String.concat "\n" (List.map string_of_block blocks)
