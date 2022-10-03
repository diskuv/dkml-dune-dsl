(rule (target somefile.txt) (action (with-stdout-to %{target} (echo hello))))

