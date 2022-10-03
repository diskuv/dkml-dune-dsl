open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      rule
        [
          target "somefile.txt";
          action
            (with_stdout_to "%{target}" (echo [ "hello"; "to all the fish" ]));
        ];
    ]
end
