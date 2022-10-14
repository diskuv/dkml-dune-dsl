(lang dune 2.9)

(name dkml-dune-dsl)

(version 0.1.0)

(generate_opam_files true)

(source
 (github diskuv/dkml-dune-dsl))

(license Apache-2.0)

(authors "Diskuv, Inc. <opensource+diskuv-ocaml@support.diskuv.com>")

(maintainers "opensource+diskuv-ocaml@support.diskuv.com")

(homepage "https://github.com/diskuv/dkml-dune-dsl#readme")

(bug_reports "https://github.com/diskuv/dkml-dune-dsl/issues")

(documentation "https://diskuv.github.io/dkml-dune-dsl/dkml-dune-dsl/index.html")

(package
 (name dkml-dune-dsl)
 (synopsis
  "Embedded DSL for Dune files to do syntax checking, auto-completion and generate dune.inc include files")
 (description
  "dkml-dune-dsl lets you define Dune files in OCaml code with your favorite auto-completing IDE, compile it to an executable, and use it as a generator of your dune.inc files.")
 (depends
  (ocaml
   (>= 4.12.1))))

(package
 (name dkml-dune-dsl-show)
 (synopsis
  "An interpreter for the embedded DSL of Dune that shows the DSL as a real Dune file")
 (documentation "https://diskuv.github.io/dkml-dune-dsl/dkml-dune-dsl-show/index.html")
 (depends
  (ocaml
   (>= 4.12.1))
  (dkml-dune-dsl :version)
  (base
   (>= v0.14))
  (ezjsonm
   (>= 1.3.0))
  (mustache
   (>= 3.1.0))
  (sexp_pretty
   (>= v0.14))
  (alcotest
   (and
    (>= 1.5.0)
    :with-test))))
