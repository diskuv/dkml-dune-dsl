# Changes

## 0.1.5

* Add `(:include sexpfile)` to ordered sets
* Add `(link_flags)` to `(executable)`

## 0.1.4

* Add `package` and `private_modules` to `executable` and `library`
* Add `install_package` to `install`
* Add `copy_with_source_directive` which implements the `copy#` action.
* Add `flags` and `ocamlc_flags` to complement existing `ocamlopt_flags`.
  Type has changed but not a breaking change since `ocamlopt_flags` was previously unusable (lacked type constructor).
* Add `preprocess_action` to `preprocess`

## 0.1.3

* Allow DkmlDuneDslShow to be used programmatically

## 0.1.2

* Add Boolean Language
* Add `enabled_if` for `(rule)`

## 0.1.1

* Add expression for `(alias _name_)` in `(deps)`.
* Add `(file_deps [...])` in `(deps)` for multiple dependencies.
* Add `(with-stdin-from)` action.

## 0.1.0 (2022-10-14)

Initial release.
