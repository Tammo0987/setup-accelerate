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
  ];

  env = {
    ACCELERATE_LLVM_CLANG_PATH = "${pkgs.clang}/bin/clang";

    # NIXOS specific fix
    NIX_ENFORCE_NO_NATIVE = "0";
  };

  languages = {
    haskell = {
      enable = true;
      package = pkgs.haskell.compiler.ghc912;
      stack.enable = false;
    };
  };
}
