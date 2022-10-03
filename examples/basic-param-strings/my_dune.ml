open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      rule
        [
          target "{{ name }}.txt";
          action (with_stdout_to "%{target}" (echo [ "{{ age }}" ]));
        ];
    ]
end
