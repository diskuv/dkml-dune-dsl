; Parameter Set =
;   {}
(library
  (name    lib_a)
  (modules a))
(executable
  (name      run_print)
  (libraries lib_a)
  (flags (:standard \ :standard) (:include open_lib_a.sexp))
  (modules run_print))
(rule
  (target run_print.txt)
  (action (with-stdout-to %{target} (run ./run_print.exe))))
