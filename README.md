# accelerate dev environment

devenv + cabal setup for Accelerate's `new-pipeline` branch, with a scratch project in `test/`.
`accelerate/` and `accelerate-llvm/` are submodules pinned to that branch.

## Setup

Requires nix, devenv and direnv.

```sh
git clone --recurse-submodules <this repo> accelerate
cd accelerate
direnv allow
cabal run accelerate-test
```

`--recurse-submodules` matters: `accelerate-llvm` has a nested `llvm-pretty` submodule it builds a
sublibrary from, and cabal fails to configure without it. If you already cloned without it, run
`git submodule update --init --recursive`.
