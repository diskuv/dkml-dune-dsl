# dkml-dune-dsl

A mini-language (DSL) for Dune files embedded in OCaml that produces readable, valid Dune include files. The
mini-language closely matches the structure of a regular Dune file, and because it is embedded in OCaml
you get OCaml type-safety and the power of your favorite OCaml IDE as you write your Dune files.

The Dune DSL uses a tagless final design so your Dune DSL can be interpreted in a variety of ways that
are independent of Dune itself. The standard **dkml-dune-dsl-show** interpreter allows all of the Dune string
values (executable names, rule targets, etc.) to be parameterized from external JSON files. That lets you
produce many similar executables, libraries or assets using the same Dune logic without repeating yourself
[DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself).

The documentation is at https://diskuv.github.io/dkml-dune-dsl/dkml-dune-dsl/index.html

Once installed (see the documentation) you will be able to write DSL expressions like:

```ocaml
open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      rule
        [
          target "{{{ name }}}.txt";
          action
            (with_stdout_to "%{target}"
              (echo [ "{{{ age }}}" ]));
        ];
    ]
end
```

that are run over the parameters in a JSON file:

```json
{
  "param-sets": [
    {"name": "batman", "age": 39},
    {"name": "robin", "age": 24}
  ]
}
```

You can do also do aggregation or, if you are really adventurous, define your own interpreter.
Even if you don't use parameterization you get things you take for granted with OCaml: type-safety,
auto-complete and `let` constants.

## Examples and Testing

The examples are available in the [examples/](./examples/README.md) folder. Since Dune is the authority on whether Dune DSL is
producing correct output, each example includes a test that checks the output of Dune.

For now the process is when a new bug is found then an existing example is expanded or a new example created.
