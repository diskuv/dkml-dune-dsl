# Publishing

1. Update the version in `dune-project`.
2. Run:

   ```shell
   dune build
   git commit -m "Bump version" *.opam
   VERSION=$(awk '$1=="(version" {print $2}' dune-project | tr -d ')')
   git tag -a -m "Version $VERSION" $VERSION
   git push
4. Make sure GitHub Actions succeeds
5. Run:
   
   ```shell
   VERSION=$(awk '$1=="(version" {print $2}' dune-project | tr -d ')')
   git push origin $VERSION
   opam install dune-release
   dune-release distrib
   dune-release publish distrib
   dune-release opam pkg
   dune-release opam submit
   ```
