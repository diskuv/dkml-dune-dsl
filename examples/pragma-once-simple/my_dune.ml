open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      pragma "once"
      @@ rule
           [
             target "common.txt";
             action (with_stdout_to "%{target}" (echo [ "ageless" ]));
           ];
      rule
        [
          target "{{{ name }}}.txt";
          action (with_stdout_to "%{target}" (echo [ "{{{ age }}}" ]));
        ];
    ]
end
