; Parameters 1
;   {"name":"batman","age":39}
(rule (target batman.txt) (action (with-stdout-to %{target} (echo 39))))

; Parameters 2
;   {"name":"robin","age":24}
(rule (target robin.txt) (action (with-stdout-to %{target} (echo 24))))
