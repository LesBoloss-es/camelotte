type 'a cast = string -> 'a

(* Readers *)

let string_as cast s = cast s

let line_as cast ichan = string_as cast (input_line ichan)

let lines_until_empty_as cast ichan =
  let rec aux acc =
    match input_line ichan with
    | exception End_of_file when acc = [] -> raise End_of_file
    | exception End_of_file -> List.rev acc
    | "" -> List.rev acc
    | line -> aux (string_as cast line :: acc)
  in
  aux []

(* Casts *)

let cast = Fun.id

let int s =
  match int_of_string_opt s with
  | None -> failwith "ExtRead.int"
  | Some n -> n

let bit s = int s = 1

let float s =
  match float_of_string_opt s with
  | None -> failwith "ExtRead.float"
  | Some f -> f

let char x =
  if String.length x <> 1 then
    failwith "ExtRead.char";
  x.[0]

let string s = s

let no_space_string s =
  if String.index_opt s ' ' <> None then
    failwith "ExtRead.no_space_string";
  s

let blank = Str.regexp "[ \t]+"

let list ?(sep = blank) cast input =
  Str.split sep input |> List.map cast

let non_empty_list ?sep cast input =
  let result = list ?sep cast input in
  if result = [] then failwith "ExtRead.non_empty_list";
  result

let seq ?sep cast input =
  list ?sep cast input |> List.to_seq

let non_empty_seq ?sep cast input =
  non_empty_list ?sep cast input |> List.to_seq

let array ?sep cast input =
  list ?sep cast input |> Array.of_list

let non_empty_array ?sep cast input =
  non_empty_list ?sep cast input |> Array.of_list

let tuple_2 ?(sep = blank) c1 c2 input =
  match Str.bounded_split sep input 2 with
  | [v1; v2] -> (c1 v1, c2 v2)
  | _ -> failwith "SpacedOut.Read.tuple_2"

let pair = tuple_2
let couple = tuple_2

let tuple_3 ?(sep = blank) c1 c2 c3 input =
  match Str.bounded_split sep input 3 with
  | [v1; v2; v3] -> (c1 v1, c2 v2, c3 v3)
  | _ -> failwith "SpacedOut.Read.tuple_3"

let triple = tuple_3

let tuple_4 ?(sep = blank) c1 c2 c3 c4 input =
  match Str.bounded_split sep input 4 with
  | [v1; v2; v3; v4] -> (c1 v1, c2 v2, c3 v3, c4 v4)
  | _ -> failwith "SpacedOut.Read.tuple_4"

let quadruple = tuple_4

let tuple_5 ?(sep = blank) c1 c2 c3 c4 c5 input =
  match Str.bounded_split sep input 5 with
  | [v1; v2; v3; v4; v5] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5)
  | _ -> failwith "SpacedOut.Read.tuple_5"

let quintuple = tuple_5
let pentuple = tuple_5

let tuple_6 ?(sep = blank) c1 c2 c3 c4 c5 c6 input =
  match Str.bounded_split sep input 6 with
  | [v1; v2; v3; v4; v5; v6] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6)
  | _ -> failwith "SpacedOut.Read.tuple_6"

let sextuple = tuple_6
let hextuple = tuple_6

let tuple_7 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 input =
  match Str.bounded_split sep input 7 with
  | [v1; v2; v3; v4; v5; v6; v7] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7)
  | _ -> failwith "SpacedOut.Read.tuple_7"

let septuple = tuple_7
let heptuple = tuple_7

let tuple_8 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 input =
  match Str.bounded_split sep input 8 with
  | [v1; v2; v3; v4; v5; v6; v7; v8] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7, c8 v8)
  | _ -> failwith "SpacedOut.Read.tuple_8"

let octuple = tuple_8

let tuple_9 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 c9 input =
  match Str.bounded_split sep input 9 with
  | [v1; v2; v3; v4; v5; v6; v7; v8; v9] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7, c8 v8, c9 v9)
  | _ -> failwith "SpacedOut.Read.tuple_9"

let nonuple = tuple_9
