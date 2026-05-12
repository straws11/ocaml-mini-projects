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

(* core *)

(* parsing inlines *)
let parse_bold chars =
    let (inside, rem) = consume_until ['*'; '*'] chars in
    (* for now assume bold can only have text in it *)
    let t = consume_prefix ['*'; '*'] rem in
    (Bold (string_of_char_list inside), t)


let parse_italic chars =
    let (inside, rem) = consume_until ['*'] chars in
    let t = consume_prefix ['*'] rem in
    (Italic (string_of_char_list inside), t)


let parse_text chars =
    let rec loop current rem = match rem with
        | '*' :: t -> (Text (string_of_char_list (List.rev current)), rem) (* end but DON'T consume the * *)
        | x :: t -> loop (x :: current) t
        | [] -> (Text (string_of_char_list (List.rev current)), rem)
    in
    loop [] chars

let rec parse_inline_list chars = match chars with
    | '*' :: '*' :: t -> let (bold, rem) = parse_bold t in
        bold :: parse_inline_list rem
    | '*' :: t -> let (italic, rem) = parse_italic t in
        italic :: parse_inline_list rem
    | _ :: t -> let (text, rem) = parse_text chars in
        text :: parse_inline_list rem
    | [] -> []

(* parse blocks *)

let get_heading_size chars = 
    let rec loop count remaining = match remaining with
        | '#' :: t -> loop (count + 1) t
        | _ -> (count, remaining)
    in
    loop 0 chars

let parse_heading line =
    let (size, rest) = get_heading_size (explode line) in
        Heading (size, parse_inline_list rest)

let parse_paragraph line = Paragraph (parse_inline_list (explode line))

(* assume only non-empty lines *)
let parse_block line = match first_char line with
    | '#' -> parse_heading line
    (* add other cases here *)
    | _ -> parse_paragraph line

let rec parse_blocks lines = match lines with
    | "" :: t -> parse_blocks t
    | h :: t -> parse_block h :: parse_blocks t
    | [] -> []

let parse_input str =
    let lines = String.split_on_char '\n' str in
    parse_blocks lines

