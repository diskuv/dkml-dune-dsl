open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      library [ name "lib_a"; modules (set_of [ "a" ]) ];
      executable
        [
          name "run_print";
          libraries [ `S "lib_a" ];
          flags (include_set "open_lib_a.sexp");
          modules (set_of [ "run_print" ]);
        ];
      rule
        [
          target "run_print.txt";
          action (with_stdout_to "%{target}" (run [ `S "./run_print.exe" ]));
        ];
    ]
end
