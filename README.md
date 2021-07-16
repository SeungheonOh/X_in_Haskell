# X in Haskell
This repository contains(or will contain) various programs written in Haskell as a personal practice.

## Setup
### Environment
There is two major "build system/package manager" for Haskell. Cabal and Stack. 
Cabal is more barebone. Cabal scraps dependencies from [Hackage](http://hackage.haskell.org/) and simply builds it. But, Cabal doesn't give a great reproducible builds. 
Stack, too solve Cabal problems, uses snapshots to manage packages. Snapshot ensures correct GHC version as well as package versions. More reproducible builds.
Cabal and Stack is purly personal preference. In this repository, Cabal was used as it is cleaner with Nix. 
### Nix
`cabal2nix` is the best tool for gener nix-shell environment. From `.cabal` file, it creates nix-shell with required Haskell packages and tools.
### Editor
Being heavily dependent on the compiler (GHC) and language supports (HLS, Hlint), Haskell shines when you use it with an editor that have good LSP support. My primary editor, Vim, definitely, wasn't editor for great Haskell experience. Instead, you want to use editors like Emacs and VSCode (Or any other editors with good LSP supports). Personally, VSCode gave me the best experience with Neovim emulation. 

## Tips
### [Hoogle](https://hoogle.haskell.org/)
Hoogle is the best tool! It will search keywords from entire Stackage library and will link you directly to the documentation. 