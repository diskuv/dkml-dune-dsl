# Publishing

1. Update the version in `dune-project`.
2. Do a `dune build`, `git commit -m "Bump version" *.opam` and `git push`.
3. Make sure GitHub Actions succeeds
4. Run:
   
   ```shell
   dune-release distrib
   dune-release publish distrib
   dune-release opam pkg
   dune-release opam submit
   ```
