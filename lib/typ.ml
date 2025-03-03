type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type json =
  | Num of int
  | Str of string
  | Arr of json list
  | Obj of (string * json) list
