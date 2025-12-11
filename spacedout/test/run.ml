open Alcotest

let tests = [
  ("Read", Test_read.tests);
  ("Write", Test_write.tests);
]

let () =
  run "SpacedOut" @@
    List.concat_map
      (fun (module_name, tests) ->
        List.map
          (fun (function_name, test) ->
            (Format.sprintf "%s.%s" module_name function_name, test)
          )
          tests
      )
      tests
