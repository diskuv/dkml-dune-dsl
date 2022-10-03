(rule (target batman.txt) (action (with-stdout-to %{target} (echo 39))))

(rule (target robin.txt) (action (with-stdout-to %{target} (echo 24))))

