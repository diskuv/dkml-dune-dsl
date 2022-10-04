open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      executable
        [
          name "example";
          modules [ "example" ];
          modes [ `Byte; `Mode (Best, Exe) ];
        ];
    ]
end
