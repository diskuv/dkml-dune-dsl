open DkmlDuneDsl

module Build (I : Dune.SYM) = struct
  open I

  let res =
    [
      rule
        [
          target "constant.txt";
          rule_enabled_if (template "true");
          action (with_stdout_to "%{target}" (echo [ "constant is true" ]));
        ];
      rule
        [
          target "constant.txt";
          rule_enabled_if (template "false");
          action (with_stdout_to "%{target}" (echo [ "constant is false" ]));
        ];
      rule
        [
          target "somefile.txt";
          rule_enabled_if (eq (template "salmon") (template "tuna"));
          action (with_stdout_to "%{target}" (echo [ "same fish" ]));
        ];
      rule
        [
          target "somefile.txt";
          rule_enabled_if (ne (template "salmon") (template "tuna"));
          action (with_stdout_to "%{target}" (echo [ "different fish" ]));
        ];
    ]
end
