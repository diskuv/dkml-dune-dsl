; Parameter Set =
;   {}
(executable
  (name do_all)
  (modules (:standard \ :standard) do_all a ((:standard \ :standard) b c)))
(rule (target all.txt) (action (with-stdout-to %{target} (run ./do_all.exe))))
