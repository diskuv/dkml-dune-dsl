; Parameter Set =
;   {}
(rule
  (target somefile.txt)
  (deps a.txt b.txt)
  (action (
    with-stdout-to %{target} (echo hello "to all the fish" "and let () = ()"))))
