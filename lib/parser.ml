(* Json Parser *)
type 'a parser = string -> (string, 'a * string) Typ.either

let ( %> ) (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
 fun input ->
  match p1 input with
  | Typ.Left err -> Typ.Left err
  | Typ.Right (_, rest) -> p2 rest

let ( <% ) (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
 fun input ->
  match p1 input with
  | Typ.Left err -> Typ.Left err
  | Typ.Right (res, rest) -> (
    match p2 rest with
    | Typ.Left err -> Typ.Left err
    | Typ.Right (_, rest) -> Typ.Right (res, rest))

let right v rest = Typ.Right (v, rest)
let left err = Typ.Left err

let mapParser (f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun input ->
  match p input with
  | Typ.Left err -> Typ.Left err
  | Typ.Right (res, rest) -> right (f res) rest

(* base type parsers*)
let pairParser (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
 fun input ->
  match p1 input with
  | Typ.Left err -> left err
  | Typ.Right (res, result) ->
    let fuseP = mapParser (fun y -> (res, y)) p2 in
    fuseP result

let charParser (ch : char) : char parser =
 fun input ->
  if String.rcontains_from input 0 ch
  then right ch (String.sub input 1 (String.length input - 1))
  else left (Printf.sprintf "character mismatch : %c at %s" ch input)

let stringParser (s : string) : string parser =
 fun input ->
  if String.starts_with ~prefix:s input
  then
    right
      (String.sub input 0 (String.length s))
      (String.sub input (String.length s) (String.length input - String.length s))
  else left "string not found"

(* string helpers *)
let findUntil s pred =
  let rec traverse_ pos =
    if pos >= String.length s
    then None
    else if pred (String.get s pos)
    then traverse_ (pos + 1)
    else Some pos
  in
  match traverse_ 0 with
  | None -> None
  | Some pos ->
    let left = String.sub s 0 pos in
    let right = String.sub s (pos + 1) (String.length s - pos - 1) in
    Some (left, right)

let condStringParser pred : string parser =
 fun input ->
  match findUntil input pred with
  | Some (l, r) -> right l r
  | None -> left "no predicate match found"

let quoteParser : string parser = charParser '\"' %> condStringParser (fun c -> c <> '\"')

(* chain parsers *)

let parseUntil (parser : 'a parser) : 'a list parser =
  let rec parseRec input acc =
    match parser input with
    | Typ.Left _err -> (acc, input)
    | Typ.Right (res, rest) -> parseRec rest (List.cons res acc)
  in
  fun input ->
    let res, rest = parseRec input [] in
    if List.length res == 0 then left "no list element found" else right res rest

let parseUntilSepBy (ch : char) (parser : 'a parser) : 'a list parser =
  let rec parseRec input acc =
    match parser input with
    | Typ.Left _err -> (acc, input)
    | Typ.Right (res, rest) -> (
      match charParser ch rest with
      | Typ.Left _err -> (List.cons res acc, rest)
      | Typ.Right (_r, rest) -> parseRec rest (List.cons res acc))
  in
  fun input ->
    let res, rest = parseRec input [] in
    if List.length res == 0 then left "no list element found" else right res rest

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
 fun input ->
  match p1 input with
  | Typ.Right (res, rest) -> right res rest
  | Typ.Left _err -> p2 input

(* json parsers *)

let stringJson : Typ.json parser = mapParser (fun x -> Typ.Str x) quoteParser

let parseJson : Typ.json parser =

  let parseJson_ref = ref (fun _ -> failwith "uninitialized parser") in

  let keyValueParser : (string * Typ.json) parser =
    let p1 = quoteParser <% charParser '=' in
    pairParser p1 (fun input -> !parseJson_ref input)
  in
  let listJson : Typ.json parser =
    let combinator = charParser '[' %> parseUntilSepBy ',' (fun input -> !parseJson_ref input) <% charParser ']' in
    mapParser (fun x -> Typ.Arr x) combinator
  in
  let objJson : Typ.json parser =
    let combinator = charParser '{' %> parseUntilSepBy ',' keyValueParser <% charParser '}' in
    mapParser (fun x -> Typ.Obj x) combinator
  in
  let parseReferer = stringJson <|> listJson <|> objJson
    in parseJson_ref := parseReferer;
    parseReferer 


let runParser (p : 'a parser) (input : string) : (string, 'a) Typ.either =
  match p input with
  | Typ.Right (res, _) -> Typ.Right res
  | Typ.Left err -> left err
