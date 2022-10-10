(* Testing for type-safety is just testing that the code compiles. *)

open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      executable
        [
          name "example1";
          modules (set_of [ "example1" ]);
          modes [ `Byte; `Mode (Best, Exe) ];
        ];
      library [ name "example2"; virtual_modules (set_of [ "example2" ]) ];
    ]
end
