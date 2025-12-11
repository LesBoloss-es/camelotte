open Alcotest

let result_testable ok_t =
  let pp fmt = function
    | Ok v -> Fmt.pf fmt "Ok(%a)" (Alcotest.pp ok_t) v
    | Error e -> Fmt.pf fmt "Error(%s)" (Printexc.to_string e)
  in
  let equal r1 r2 =
    match r1, r2 with
    | Ok v1, Ok v2 -> Alcotest.equal ok_t v1 v2
    | Error e1, Error e2 ->
      (
        match e1, e2 with
        | Failure _, Failure _ -> true
        | Invalid_argument _, Invalid_argument _ -> true
        | _ -> false
      )
    | _ -> false
  in
  testable pp equal

(* Basic parsers tests *)

let test_int_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as int "7") with exn -> Error exn in
  check (result_testable Alcotest.int) "7" (Ok 7) result

let test_int_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as int "L") with exn -> Error exn in
  check (result_testable Alcotest.int) "L" (Error (Failure "")) result

let test_bit_true () =
  let open SpacedOut.Read in
  let result = try Ok (string_as bit "1") with exn -> Error exn in
  check (result_testable Alcotest.bool) "1" (Ok true) result

let test_bit_false () =
  let open SpacedOut.Read in
  let result = try Ok (string_as bit "0") with exn -> Error exn in
  check (result_testable Alcotest.bool) "0" (Ok false) result

let test_bit_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as bit "T") with exn -> Error exn in
  check (result_testable Alcotest.bool) "T" (Error (Failure "")) result

let test_float_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as float "34.2") with exn -> Error exn in
  check (result_testable (Alcotest.float 0.001)) "34.2" (Ok 34.2) result

let test_float_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as float "TRUE") with exn -> Error exn in
  check (result_testable (Alcotest.float 0.001)) "TRUE" (Error (Failure "")) result

let test_char_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as char "Y") with exn -> Error exn in
  check (result_testable Alcotest.char) "Y" (Ok 'Y') result

let test_char_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as char "YO") with exn -> Error exn in
  check (result_testable Alcotest.char) "YO" (Error (Failure "")) result

let test_string_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as string "Bonjour") with exn -> Error exn in
  check (result_testable Alcotest.string) "Bonjour" (Ok "Bonjour") result

let test_no_space_string_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as no_space_string "Bonjour") with exn -> Error exn in
  check (result_testable Alcotest.string) "Bonjour" (Ok "Bonjour") result

let test_no_space_string_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as no_space_string "Bon jour") with exn -> Error exn in
  check (result_testable Alcotest.string) "Bon jour" (Error (Failure "")) result

let basic_parsers_tests = [
  test_case "int valid" `Quick test_int_valid;
  test_case "int invalid" `Quick test_int_invalid;
  test_case "bit true" `Quick test_bit_true;
  test_case "bit false" `Quick test_bit_false;
  test_case "bit invalid" `Quick test_bit_invalid;
  test_case "float valid" `Quick test_float_valid;
  test_case "float invalid" `Quick test_float_invalid;
  test_case "char valid" `Quick test_char_valid;
  test_case "char invalid" `Quick test_char_invalid;
  test_case "string valid" `Quick test_string_valid;
  test_case "no_space_string valid" `Quick test_no_space_string_valid;
  test_case "no_space_string invalid" `Quick test_no_space_string_invalid;
]

(* List and array parsers tests *)

let test_list_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (list int) "1 2 7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1 2 7" (Ok [1; 2; 7]) result

let test_list_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (list int) "1 L 7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1 L 7" (Error (Failure "")) result

let test_list_empty () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (list int) "") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "" (Ok []) result

let test_non_empty_list_empty () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (non_empty_list int) "") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "" (Error (Failure "")) result

let test_array_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (array int) "1 2 7") with exn -> Error exn in
  check (result_testable (Alcotest.array Alcotest.int)) "1 2 7" (Ok [|1; 2; 7|]) result

let test_array_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (array int) "1 L 7") with exn -> Error exn in
  check (result_testable (Alcotest.array Alcotest.int)) "1 L 7" (Error (Failure "")) result

let test_array_empty () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (array int) "") with exn -> Error exn in
  check (result_testable (Alcotest.array Alcotest.int)) "" (Ok [||]) result

let test_non_empty_array_empty () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (non_empty_array int) "") with exn -> Error exn in
  check (result_testable (Alcotest.array Alcotest.int)) "" (Error (Failure "")) result

let list_array_tests = [
  test_case "list valid" `Quick test_list_valid;
  test_case "list invalid" `Quick test_list_invalid;
  test_case "list empty" `Quick test_list_empty;
  test_case "non_empty_list empty" `Quick test_non_empty_list_empty;
  test_case "array valid" `Quick test_array_valid;
  test_case "array invalid" `Quick test_array_invalid;
  test_case "array empty" `Quick test_array_empty;
  test_case "non_empty_array empty" `Quick test_non_empty_array_empty;
]

(* Pair/tuple2 parsers tests *)

let test_pair_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int int) "7 8") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) "7 8" (Ok (7, 8)) result

let test_pair_invalid_second () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int int) "7 L") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) "7 L" (Error (Failure "")) result

let test_pair_too_few () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int int) "7") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) "7" (Error (Failure "")) result

let test_pair_too_many () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int int) "7 8 9") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) "7 8 9" (Error (Failure "")) result

let test_pair_mixed_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int float) "7 34.2") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int (Alcotest.float 0.001))) "7 34.2" (Ok (7, 34.2)) result

let test_tuple_2_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_2 int int) "8 9") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) "8 9" (Ok (8, 9)) result

let test_tuple_2_mixed_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_2 int float) "7 34.2") with exn -> Error exn in
  check (result_testable (Alcotest.pair Alcotest.int (Alcotest.float 0.001))) "7 34.2" (Ok (7, 34.2)) result

let pair_tests = [
  test_case "pair valid" `Quick test_pair_valid;
  test_case "pair invalid second element" `Quick test_pair_invalid_second;
  test_case "pair too few elements" `Quick test_pair_too_few;
  test_case "pair too many elements" `Quick test_pair_too_many;
  test_case "pair mixed types valid" `Quick test_pair_mixed_valid;
  test_case "tuple_2 valid" `Quick test_tuple_2_valid;
  test_case "tuple_2 mixed types valid" `Quick test_tuple_2_mixed_valid;
]

(* Tuple3 parsers tests *)

let test_tuple_3_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int int int) "7 8 9") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "7 8 9" (Ok (7, 8, 9)) result

let test_tuple_3_invalid_second () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int int int) "7 L 9") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "7 L 9" (Error (Failure "")) result

let test_tuple_3_too_few () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int int int) "7 8") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "7 8" (Error (Failure "")) result

let test_tuple_3_too_many () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int int int) "7 8 9 10") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "7 8 9 10" (Error (Failure "")) result

let test_tuple_3_mixed_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int float string) "7 8 9") with exn -> Error exn in
  let triple_testable =
    Alcotest.testable
      (fun fmt (a, b, c) -> Fmt.pf fmt "(%d, %g, %s)" a b c)
      (fun (a1, b1, c1) (a2, b2, c2) -> a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2)
  in
  check (result_testable triple_testable) "7 8 9" (Ok (7, 8., "9")) result

let test_tuple_3_mixed_too_few () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int float string) "7 8") with exn -> Error exn in
  let triple_testable =
    Alcotest.testable
      (fun fmt (a, b, c) -> Fmt.pf fmt "(%d, %g, %s)" a b c)
      (fun (a1, b1, c1) (a2, b2, c2) -> a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2)
  in
  check (result_testable triple_testable) "7 8" (Error (Failure "")) result

let test_tuple_3_mixed_with_string_rest () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int float string) "7 8 9 10") with exn -> Error exn in
  let triple_testable =
    Alcotest.testable
      (fun fmt (a, b, c) -> Fmt.pf fmt "(%d, %g, %s)" a b c)
      (fun (a1, b1, c1) (a2, b2, c2) -> a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2)
  in
  check (result_testable triple_testable) "7 8 9 10" (Ok (7, 8., "9 10")) result

let test_tuple_3_mixed_no_space_string_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_3 int float no_space_string) "7 8 9 10") with exn -> Error exn in
  let triple_testable =
    Alcotest.testable
      (fun fmt (a, b, c) -> Fmt.pf fmt "(%d, %g, %s)" a b c)
      (fun (a1, b1, c1) (a2, b2, c2) -> a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2)
  in
  check (result_testable triple_testable) "7 8 9 10" (Error (Failure "")) result

let tuple_3_tests = [
  test_case "tuple_3 valid" `Quick test_tuple_3_valid;
  test_case "tuple_3 invalid second element" `Quick test_tuple_3_invalid_second;
  test_case "tuple_3 too few elements" `Quick test_tuple_3_too_few;
  test_case "tuple_3 too many elements" `Quick test_tuple_3_too_many;
  test_case "tuple_3 mixed types valid" `Quick test_tuple_3_mixed_valid;
  test_case "tuple_3 mixed types too few" `Quick test_tuple_3_mixed_too_few;
  test_case "tuple_3 mixed with string consuming rest" `Quick test_tuple_3_mixed_with_string_rest;
  test_case "tuple_3 mixed no_space_string invalid" `Quick test_tuple_3_mixed_no_space_string_invalid;
]

(* Tuple4 parsers tests *)

let test_tuple_4_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_4 int int int int) "7 8 9 10") with exn -> Error exn in
  let tuple4_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %d, %d, %d)" a b c d)
      (=)
  in
  check (result_testable tuple4_testable) "7 8 9 10" (Ok (7, 8, 9, 10)) result

let test_tuple_4_mixed_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_4 int float char string) "7 8 9 10") with exn -> Error exn in
  let tuple4_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %g, %c, %s)" a b c d)
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2
      )
  in
  check (result_testable tuple4_testable) "7 8 9 10" (Ok (7, 8., '9', "10")) result

let test_tuple_4_mixed_with_string_rest () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_4 int float char string) "7 8 9 10 11") with exn -> Error exn in
  let tuple4_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %g, %c, %s)" a b c d)
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2
      )
  in
  check (result_testable tuple4_testable) "7 8 9 10 11" (Ok (7, 8., '9', "10 11")) result

let test_tuple_4_mixed_no_space_string_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_4 int float char no_space_string) "7 8 9 10 11") with exn -> Error exn in
  let tuple4_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %g, %c, %s)" a b c d)
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2
      )
  in
  check (result_testable tuple4_testable) "7 8 9 10 11" (Error (Failure "")) result

let tuple_4_tests = [
  test_case "tuple_4 valid" `Quick test_tuple_4_valid;
  test_case "tuple_4 mixed types valid" `Quick test_tuple_4_mixed_valid;
  test_case "tuple_4 mixed with string consuming rest" `Quick test_tuple_4_mixed_with_string_rest;
  test_case "tuple_4 mixed no_space_string invalid" `Quick test_tuple_4_mixed_no_space_string_invalid;
]

(* Tuple5 parsers tests *)

let test_tuple_5_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_5 int int int int int) "7 8 9 10 11") with exn -> Error exn in
  let tuple5_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %d, %d, %d, %d)" a b c d e)
      (=)
  in
  check (result_testable tuple5_testable) "7 8 9 10 11" (Ok (7, 8, 9, 10, 11)) result

let test_tuple_5_mixed_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_5 int float bit char string) "7 8 1 9 10") with exn -> Error exn in
  let tuple5_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %g, %b, %c, %s)" a b c d e)
      (fun (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2 && e1 = e2
      )
  in
  check (result_testable tuple5_testable) "7 8 1 9 10" (Ok (7, 8., true, '9', "10")) result

let test_tuple_5_mixed_with_string_rest () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_5 int float bit char string) "7 8 0 9 10 11") with exn -> Error exn in
  let tuple5_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %g, %b, %c, %s)" a b c d e)
      (fun (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2 && e1 = e2
      )
  in
  check (result_testable tuple5_testable) "7 8 0 9 10 11" (Ok (7, 8., false, '9', "10 11")) result

let test_tuple_5_mixed_no_space_string_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (tuple_5 int float bit char no_space_string) "7 8 0 9 10 11") with exn -> Error exn in
  let tuple5_testable =
    Alcotest.testable
      (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %g, %b, %c, %s)" a b c d e)
      (fun (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) ->
        a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2 && e1 = e2
      )
  in
  check (result_testable tuple5_testable) "7 8 0 9 10 11" (Error (Failure "")) result

let tuple_5_tests = [
  test_case "tuple_5 valid" `Quick test_tuple_5_valid;
  test_case "tuple_5 mixed types valid" `Quick test_tuple_5_mixed_valid;
  test_case "tuple_5 mixed with string consuming rest" `Quick test_tuple_5_mixed_with_string_rest;
  test_case "tuple_5 mixed no_space_string invalid" `Quick test_tuple_5_mixed_no_space_string_invalid;
]

(* Nested parsers tests *)

let test_nested_pair_list_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int (list string)) "7 8 9 10") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt (a, lst) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.list string) lst)
      (=)
  in
  check (result_testable nested_testable) "7 8 9 10" (Ok (7, ["8"; "9"; "10"])) result

let test_nested_pair_list_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int (list string)) "7") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt (a, lst) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.list string) lst)
      (=)
  in
  check (result_testable nested_testable) "7" (Error (Failure "")) result

let test_nested_pair_array_valid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int (array string)) "7 8 9 10") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt (a, arr) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.array string) arr)
      (=)
  in
  check (result_testable nested_testable) "7 8 9 10" (Ok (7, [|"8"; "9"; "10"|])) result

let test_nested_pair_array_invalid () =
  let open SpacedOut.Read in
  let result = try Ok (string_as (pair int (array string)) "7") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt (a, arr) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.array string) arr)
      (=)
  in
  check (result_testable nested_testable) "7" (Error (Failure "")) result

let nested_tests = [
  test_case "pair int list valid" `Quick test_nested_pair_list_valid;
  test_case "pair int list invalid" `Quick test_nested_pair_list_invalid;
  test_case "pair int array valid" `Quick test_nested_pair_array_valid;
  test_case "pair int array invalid" `Quick test_nested_pair_array_invalid;
]

(* Custom separators tests *)

let test_list_comma_sep_basic () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (list ~sep: comma int) "1,2,7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1,2,7" (Ok [1; 2; 7]) result

let test_list_comma_sep_with_spaces () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (list ~sep: comma int) "1, 2 ,7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1, 2 ,7" (Ok [1; 2; 7]) result

let test_list_comma_sep_with_more_spaces () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (list ~sep: comma int) "1   , 2 , 7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1   , 2 , 7" (Ok [1; 2; 7]) result

let test_list_comma_sep_double_comma () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (list ~sep: comma int) "1  , , 2 , 7") with exn -> Error exn in
  check (result_testable (Alcotest.list Alcotest.int)) "1  , , 2 , 7" (Error (Failure "")) result

let test_triple_comma_sep_basic () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (triple ~sep: comma int int int) "1,2,7") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "1,2,7" (Ok (1, 2, 7)) result

let test_triple_comma_sep_with_spaces () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (triple ~sep: comma int int int) "1, 2 ,7") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "1, 2 ,7" (Ok (1, 2, 7)) result

let test_triple_comma_sep_with_more_spaces () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (triple ~sep: comma int int int) "1   , 2 , 7") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "1   , 2 , 7" (Ok (1, 2, 7)) result

let test_triple_comma_sep_double_comma () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let result = try Ok (string_as (triple ~sep: comma int int int) "1  , , 2 , 7") with exn -> Error exn in
  check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) "1  , , 2 , 7" (Error (Failure "")) result

let test_nested_comma_dash_sep_basic () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let result = try Ok (string_as (list ~sep: comma (pair ~sep: dash int float)) "2-4,6-8") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
      (=)
  in
  check (result_testable nested_testable) "2-4,6-8" (Ok [(2, 4.); (6, 8.)]) result

let test_nested_comma_dash_sep_with_space () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let result = try Ok (string_as (list ~sep: comma (pair ~sep: dash int float)) "2-4, 6-8") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
      (=)
  in
  check (result_testable nested_testable) "2-4, 6-8" (Ok [(2, 4.); (6, 8.)]) result

let test_nested_comma_dash_sep_with_trailing_space () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let result = try Ok (string_as (list ~sep: comma (pair ~sep: dash int float)) "2-4  ,6-8") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
      (=)
  in
  check (result_testable nested_testable) "2-4  ,6-8" (Ok [(2, 4.); (6, 8.)]) result

let test_nested_comma_dash_sep_too_many_dashes () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let result = try Ok (string_as (list ~sep: comma (pair ~sep: dash int float)) "2-4-3  ,6-8") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
      (=)
  in
  check (result_testable nested_testable) "2-4-3  ,6-8" (Error (Failure "")) result

let test_nested_comma_dash_sep_wrong_sep () =
  let open SpacedOut.Read in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let result = try Ok (string_as (list ~sep: comma (pair ~sep: dash int float)) "2,4-5  ,6-8") with exn -> Error exn in
  let nested_testable =
    Alcotest.testable
      (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
      (=)
  in
  check (result_testable nested_testable) "2,4-5  ,6-8" (Error (Failure "")) result

let custom_separators_tests = [
  test_case "list comma separator basic" `Quick test_list_comma_sep_basic;
  test_case "list comma separator with spaces" `Quick test_list_comma_sep_with_spaces;
  test_case "list comma separator with more spaces" `Quick test_list_comma_sep_with_more_spaces;
  test_case "list comma separator double comma" `Quick test_list_comma_sep_double_comma;
  test_case "triple comma separator basic" `Quick test_triple_comma_sep_basic;
  test_case "triple comma separator with spaces" `Quick test_triple_comma_sep_with_spaces;
  test_case "triple comma separator with more spaces" `Quick test_triple_comma_sep_with_more_spaces;
  test_case "triple comma separator double comma" `Quick test_triple_comma_sep_double_comma;
  test_case "nested comma-dash separator basic" `Quick test_nested_comma_dash_sep_basic;
  test_case "nested comma-dash separator with space" `Quick test_nested_comma_dash_sep_with_space;
  test_case "nested comma-dash separator with trailing space" `Quick test_nested_comma_dash_sep_with_trailing_space;
  test_case "nested comma-dash separator too many dashes" `Quick test_nested_comma_dash_sep_too_many_dashes;
  test_case "nested comma-dash separator wrong separator" `Quick test_nested_comma_dash_sep_wrong_sep;
]

let tests = [
  "basic_parsers", basic_parsers_tests;
  "list_array", list_array_tests;
  "pair", pair_tests;
  "tuple_3", tuple_3_tests;
  "tuple_4", tuple_4_tests;
  "tuple_5", tuple_5_tests;
  "nested", nested_tests;
  "custom_separators", custom_separators_tests;
]
