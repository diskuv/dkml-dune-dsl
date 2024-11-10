; Parameter Set =
;   {}
(rule
  (target     constant.txt)
  (enabled_if true)
  (action (with-stdout-to %{target} (echo "constant is true"))))
(rule
  (target     constant.txt)
  (enabled_if false)
  (action (with-stdout-to %{target} (echo "constant is false"))))
(rule
  (target somefile.txt)
  (enabled_if (= salmon tuna))
  (action (with-stdout-to %{target} (echo "same fish"))))
(rule
  (target somefile.txt)
  (enabled_if (<> salmon tuna))
  (action (with-stdout-to %{target} (echo "different fish"))))
