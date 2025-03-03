module P = Parler.Parser
module Typ = Parler.Typ

let () = 
  match P.runParser P.stringJson "\"hello there\"" with
  | Typ.Right (_json)  ->  print_endline "string parsed"
  | Typ.Left err -> print_endline err 
