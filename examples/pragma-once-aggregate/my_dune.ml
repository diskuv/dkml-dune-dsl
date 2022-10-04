open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      pragma "once"
      @@ rule
           [
             target "aggregate.txt";
             action
               (with_stdout_to "%{target}"
                  (echo
                     [ "{{#param-sets}}{{ name }}={{ age }}. {{/param-sets}}" ]));
           ];
      rule
        [
          target "{{{ name }}}.txt";
          action (with_stdout_to "%{target}" (echo [ "{{{ age }}}" ]));
        ];
    ]
end
