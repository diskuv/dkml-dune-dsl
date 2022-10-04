; Parameter Set =
;   {"name":"batman","age":39}
(rule (target common.txt) (action (with-stdout-to %{target} (echo ageless))))
(rule (target batman.txt) (action (with-stdout-to %{target} (echo 39))))

; Parameter Set =
;   {"name":"robin","age":24}
(rule (target robin.txt) (action (with-stdout-to %{target} (echo 24))))
