open Ast
(* helper *)

let is_blank str = String.length str = 0

let first_char str = str.[0]

let string_of_char_list chars = chars |> List.to_seq |> String.of_seq

let explode str = str |> String.to_seq |> List.of_seq

let consume_prefix prefix chars =
    let rec loop consums chrs =
        match consums, chrs with
            | [], t -> Some t
            | x :: xs, y :: ys when x = y -> loop xs ys
            | _ -> None
        in
    match loop prefix chars with
        | Some rest -> rest
        | None -> chars

let rec starts_with prefix chars = (* prefix: char list, chars: char list *)
    match (prefix, chars) with
        | [], _ -> true
        | x :: xs, y :: ys when x = y -> starts_with xs ys
        | _ -> false

let consume_until delim chars =
    let rec loop current rem =
        if starts_with delim rem then
            (List.rev current, rem)
        else
            match rem with
                | h :: t -> loop (h :: current) t
                | [] -> failwith "Delimiter not found"
        in
        loop [] chars

let get_block_kind line = match first_char line with
    | '#' -> HeadingKind
    | '-' -> UnorderedListKind
    | _ -> ParagraphKind

(* core *)

(* parsing inlines *)

let rec parse_inline_list chars = match chars with
    | '*' :: '*' :: '*' :: t -> let (bold_italic, rem) = parse_bold_italic t in
        bold_italic :: parse_inline_list rem
    | '*' :: '*' :: t -> let (bold, rem) = parse_bold t in
        bold :: parse_inline_list rem
    | '*' :: t -> let (italic, rem) = parse_italic t in
        italic :: parse_inline_list rem
    | _ :: t -> let (text, rem) = parse_text chars in
        text :: parse_inline_list rem
    | [] -> []

and parse_bold_italic chars =
    let (inside, rem) = consume_until ['*'; '*'; '*'] chars in
    let t = consume_prefix ['*'; '*'; '*'] rem in
    (Bold ([Italic (parse_inline_list inside)]), t)


and parse_bold chars =
    let (inside, rem) = consume_until ['*'; '*'] chars in
    let t = consume_prefix ['*'; '*'] rem in
    (Bold (parse_inline_list inside), t)


and parse_italic chars =
    let (inside, rem) = consume_until ['*'] chars in
    let t = consume_prefix ['*'] rem in
    (Italic (parse_inline_list inside), t)


and parse_text chars =
    let rec loop current rem = match rem with
        | '*' :: t -> (Text (string_of_char_list (List.rev current)), rem) (* end but DON'T consume the * *)
        | x :: t -> loop (x :: current) t
        | [] -> (Text (string_of_char_list (List.rev current)), rem)
    in
    loop [] chars

(* parse blocks *)

let get_heading_size chars = 
    let rec loop count remaining = match remaining with
        | '#' :: t -> loop (count + 1) t
        | _ -> (count, remaining)
    in
    loop 0 chars

let parse_heading line =
    let (size, rest) = get_heading_size (explode line) in
    let rem = consume_prefix [' '] rest in
        Heading (size, parse_inline_list rem)

let parse_paragraph lines =
    let rec loop acc rem = match rem with
        | "" :: t -> (acc, t)
        | h :: t -> begin match get_block_kind h with
            | ParagraphKind -> loop (h :: acc) t
            | _ -> (acc, rem)
            end
        | [] -> (acc, rem)
    in
    let (to_parse, remaining) = loop [] lines in
    let text = String.concat " " (List.rev to_parse) in
    (Paragraph (parse_inline_list (explode text)), remaining)

(* will receive lines with first line starting with '-' *)
let parse_unordered_list_item lines =
    let rec loop acc rem = match rem with
        | "" :: t -> (acc, rem) (* blank line so we're done *)
        | h :: t -> begin match first_char h with
            | '-' -> (acc, rem) (* new list item so we're done *)
            | _ -> loop (h :: acc) t (* include this line as part of the item *)
            end
        | [] -> (acc, rem)
    in

    (* remove dash from first line *)
    let (first, remaining) = match lines with
        | h :: t -> (h, t)
        | [] -> failwith "Parse error in unordered list item"
    in
    let first_line = first |> explode |> (consume_prefix ['-'; ' ']) |> string_of_char_list in
    let (item_lines, rem) = loop [first_line] remaining in
    let text = String.concat " " (List.rev item_lines) in
    (parse_inline_list (explode text), rem)


let parse_unordered_list lines =
    let rec loop parsed rem = match rem with
        | "" :: t -> (parsed, rem)
        | h :: t -> begin match first_char h with
            | '-' -> let (parsed_item, rest) = parse_unordered_list_item rem in
                loop (parsed_item :: parsed) rest
            | _ -> (parsed, rem)
            end
        | [] -> (parsed, rem)
    in
    let (parsed_items, rem) = loop [] lines in
    (UnorderedList (List.rev parsed_items), rem)

let parse_block lines = match lines with
    | h :: t -> begin match first_char h with (* identify block by starting char *)
        | '#' -> (parse_heading h, t)
        | '-' -> parse_unordered_list lines
        | _ -> parse_paragraph lines
            end
    | [] -> failwith "Cannot parse empty block - should be impossible"

let rec parse_blocks lines = match lines with
    | "" :: t -> parse_blocks t
    | [] -> []
    | _ -> let (block, rem) = parse_block lines in
        block :: parse_blocks rem

let parse_input lines =
    parse_blocks lines

