type 'a cast = 'a -> string

(* Writers *)

let string_as cast v = cast v

let line_as cast ochan v =
  output_string ochan (string_as cast v);
  output_char ochan '\n'

(* Casts *)

let int = string_of_int
let bit = function true -> "1" | _ -> "0"
let float = Format.sprintf "%g"
let char = String.make 1
let string = Fun.id

let blank = " "

let list ?(sep = blank) c l = List.map c l |> String.concat sep
let seq ?sep c s = list ?sep c (List.of_seq s)
let array ?sep c a = list ?sep c (Array.to_list a)

let tuple_2 ?(sep = blank) c1 c2 (v1, v2) =
  c1 v1 ^ sep ^ c2 v2

let pair = tuple_2
let couple = tuple_2

let tuple_3 ?(sep = blank) c1 c2 c3 (v1, v2, v3) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3

let triple = tuple_3

let tuple_4 ?(sep = blank) c1 c2 c3 c4 (v1, v2, v3, v4) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4

let quadruple = tuple_4

let tuple_5 ?(sep = blank) c1 c2 c3 c4 c5 (v1, v2, v3, v4, v5) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5

let pentuple = tuple_5
let quintuple = tuple_5

let tuple_6 ?(sep = blank) c1 c2 c3 c4 c5 c6 (v1, v2, v3, v4, v5, v6) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6

let sextuple = tuple_6
let hextuple = tuple_6

let tuple_7 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 (v1, v2, v3, v4, v5, v6, v7) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7

let septuple = tuple_7
let heptuple = tuple_7

let tuple_8 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 (v1, v2, v3, v4, v5, v6, v7, v8) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7 ^ sep ^ c8 v8

let octuple = tuple_8

let tuple_9 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 c9 (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7 ^ sep ^ c8 v8 ^ sep ^ c9 v9

let nonuple = tuple_9
