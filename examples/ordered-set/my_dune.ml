open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      executable
        [
          name "do_all";
          modules
            (union
               [
                 set_of [ "do_all" ];
                 union [ union [ union [ set_of [ "a" ] ] ] ];
                 set_of [ "b"; "c" ];
               ]);
        ];
      rule
        [
          target "all.txt";
          action (with_stdout_to "%{target}" (run [ "./do_all.exe" ]));
        ];
    ]
end
