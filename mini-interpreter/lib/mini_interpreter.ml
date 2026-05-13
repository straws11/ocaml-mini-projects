type expression =
    | Int of int
    | Add of expression * expression
    | Sub of expression * expression
    | Mul of expression * expression

let rec string_of_expr expr = match expr with
  | Int x -> "Int(" ^ string_of_int x ^ ")"
  | Add (x, y) -> "Add(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ")"
  | Sub (x, y) -> "Sub(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ")"
  | Mul (x, y) -> "Mul(" ^ string_of_expr x ^ ", " ^ string_of_expr y ^ ")"

type op =
    | IntTok of int
    | Plus
    | Minus
    | MulTok
    | LParen
    | RParen

let string_of_op op = match op with
    | IntTok x -> "IntTok " ^ string_of_int x
    | Plus -> "Plus"
    | Minus -> "Minus"
    | MulTok -> "MulTok"
    | LParen -> "LParen"
    | RParen -> "RParen"

let rec tokenize_int chars = match chars with
    | '0'..'9' as c :: t -> let (tokenized, modified_chars) = tokenize_int t in
        (String.make 1 c ^ tokenized, modified_chars)
    | _ -> ("", chars)

let rec tokenize chars = match chars with
    | ' ' :: t -> tokenize t
    | '(' :: t -> LParen :: tokenize t
    | ')' :: t -> RParen :: tokenize t
    | '+' :: t -> Plus :: tokenize t
    | '-' :: t -> Minus :: tokenize t
    | '*' :: t -> MulTok :: tokenize t
    | '0'..'9' as c :: t -> let (num, rest) = tokenize_int t in
        IntTok (int_of_string (String.make 1 c ^ num)) :: tokenize rest
    | [] -> []
    | _ :: t -> tokenize t

let rec help_parse_term parsed tokens = match tokens with
    | MulTok :: t -> let (p, rem) = parse_factor t in
      let thing = Mul(parsed, p) in
      help_parse_term thing rem

    | _ -> (parsed, tokens)

and help_parse_expr parsed tokens = match tokens with
  | Plus :: t -> let (p, rem) = parse_term t in
    let thing = Add(parsed, p) in
    help_parse_expr thing rem

  | Minus :: t -> let (p, rem) = parse_term t in
    let thing = Sub(parsed, p) in
    help_parse_expr thing rem 

  | _ -> (parsed, tokens)

(* expr := term ( ( '+' | '-' ) term )* *)
and parse_expr tokens = let (parsed, remaining) = parse_term tokens in
  help_parse_expr parsed remaining

(* term := factor ( ( '*' | '/' ) factor )* *)
and parse_term tokens = let (parsed, remaining) = parse_factor tokens in
    help_parse_term parsed remaining

(* factor := INT | '(' expr ')' *)
and parse_factor tokens = match tokens with
    | IntTok x :: t -> (Int x, t)
    | LParen :: t -> let (parsed, remaining) = parse_expr t in
        begin match remaining with
            | RParen :: rest -> (parsed, rest)
            | _ -> failwith "Parse error"
        end
    | _ -> failwith "Parse error"

let rec evaluate ast = match ast with
  | Add (x, y) -> evaluate x + evaluate y
  | Sub (x, y) -> evaluate x - evaluate y
  | Mul (x, y) -> evaluate x * evaluate y
  | Int x -> x

