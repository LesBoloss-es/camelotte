# SpacedOut

SpacedOut is a library that helps you read and write space-separated data files.
I wrote it initially as a helper library to parse and produce files in the (now
discontinued) [Hash Code programming competition]. For instance, Hash Code 2021
contained the following example file:

```
6 4 5 2 1000
2 0 rue-de-londres 1
0 1 rue-d-amsterdam 1
3 1 rue-d-athenes 1
2 3 rue-de-rome 2
1 2 rue-de-moscou 3
4 rue-de-londres rue-d-amsterdam rue-de-moscou rue-de-rome
3 rue-d-athenes rue-de-moscou rue-de-londres
```

which can be read efficiently for the OCaml code:

```ocaml
open SpacedOut

type street = {
  begins : int;
  ends : int;
  name : string;
  length : int;
}

type problem = {
  duration : int;
  nb_intersections : int;
  streets : street list;
  bonus_points : int;
}

let (duration, nb_intersections, nb_streets, nb_cars, bonus_points) =
  Read.(line_as (quintuple int int int int int) stdin)
in
let streets =
  List.init nb_streets @@ fun _ ->
    let (begins, ends, name, length) = Read.(line_as (quadruple int int string int) stdin) in
    assert (0 <= begins && begins < nb_intersections);
    assert (0 <= ends && ends < nb_intersections);
    assert (1 <= length && length <= duration);
    { begins; ends; name; length }
in
let cars =
  List.init nb_cars @@ fun _ ->
    let (p, names) = Read.(line_as (couple int (list string)) stdin) in
    assert (2 <= p && p <= 1000);
    assert (List.length names = p);
    names
in
{ duration; nb_intersections; streets; cars; bonus_points }
```

[Hash Code programming competition]: https://en.wikipedia.org/wiki/Hash_Code_(programming_competition)
