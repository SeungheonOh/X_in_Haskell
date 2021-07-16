# Hello World!
This is how Haskell project directory looks when using Nix and Cabal.

`default.nix` will automatically create nix derivation and nix-shell file according to `.cabal` file. The developmental environment will be automatically setup by executing `nix-shell`.

Haskell source code will be located on `/app` directory according to standard Haskell project directory composition. 