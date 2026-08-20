{
  pkgs,
  ...
}:

{
  packages = with pkgs; [
    gcc
    cbc
    libffi
    pkg-config
    haskell.packages.ghc912.cabal-gild
    haskell.packages.ghc912.hlint
    haskell.packages.ghc912.ormolu
  ];

  env = {
    ACCELERATE_LLVM_CLANG_PATH = "${pkgs.llvmPackages.clang-unwrapped}/bin/clang";

    # NIXOS specific fix
    NIX_ENFORCE_NO_NATIVE = "0";
  };

  scripts = {
    build-ilp.exec = ''
      set -e
      cd "$DEVENV_ROOT"
      exec cabal build ilp "$@"
    '';

    test-ilp.exec = ''
      set -e
      cd "$DEVENV_ROOT"
      exec cabal test ilp --test-show-details=direct "$@"
    '';

    test-native.exec = ''
      set -e
      cd "$DEVENV_ROOT"
      cabal build nofib-llvm-native --constraint="accelerate +nofib" >&2
      bin="$(cabal list-bin nofib-llvm-native --constraint="accelerate +nofib")"
      exec "$bin" -p $'$0!~/scanl\x27Seg/' "$@"
    '';
  };

  languages = {
    haskell = {
      enable = true;
      package = pkgs.haskell.compiler.ghc912;
      stack.enable = false;
    };
  };
}
