# Examples

Below are the examples, ranked with the first examples being the easiest
and the last examples being the most advanced.

Each example will have a ["my_dune.ml"] which is where you want to start
reading. The ["dune.inc"] is automatically generated from the ["my_dune.ml"]
using the boilerplate in ["dune"].

| Example                 | Description                                                   |
| ----------------------- | ------------------------------------------------------------- |
| [basic-no-param]        | The simplest example                                          |
| [basic-deps]            | Adds dependencies to [basic-no-param]                         |
| [executable]            | Create an executable                                          |
| [basic-param-strings]   | Adds two parameters to [basic-no-param]                       |
| [ordered-set]           | An example using Ordered Sets                                 |
| [ordered-set-include]   | An example using Ordered Sets and `(:include file)            |
| [pragma-once-simple]    | Simple use of `(pragma once ...)`                             |
| [pragma-once-aggregate] | Adds an aggregate Mustache expression to [pragma-once-simple] |
| [toplevel-param-bad]    | Fails when `_` is not a prefix of your custom parameter name  |
| [test-support]          | *This is not an example. It is unit test code*                |

[basic-no-param]: ./basic-no-param
[basic-deps]: ./basic-deps
[basic-param-strings]: ./basic-param-strings
[executable]: ./executable
[ordered-set]: ./ordered-set
[pragma-once-aggregate]: ./pragma-once-aggregate
[pragma-once-simple]: ./pragma-once-simple
[test-support]: ./test-support
[toplevel-param-bad]: ./toplevel-param-bad
