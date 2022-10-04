open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      executable
        [
          name "example";
          modules (set_of [ "example" ]);
          modes [ `Byte; `Mode (Best, Exe) ];
        ];
    ]
end
