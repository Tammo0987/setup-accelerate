{
  pkgs,
  ...
}:

{
  packages = with pkgs; [
    (texlive.combine {
      inherit (texlive)
        scheme-small
        latexmk
        pgfplots
        algorithms
        algorithmicx
        ;
    })
    gcc
    cbc
    libffi
    pkg-config
    cudaPackages.cudatoolkit
    haskell.packages.ghc912.cabal-gild
    haskell.packages.ghc912.hlint
    haskell.packages.ghc912.ormolu
  ];

  env = {
    ACCELERATE_LLVM_CLANG_PATH = "${pkgs.llvmPackages.clang-unwrapped}/bin/clang";

    # Where the `cuda` package's configure step looks for nvcc and the headers.
    CUDA_PATH = "${pkgs.cudaPackages.cudatoolkit}";

    # libcuda ships with the NVIDIA driver, not the toolkit. The toolkit's stub
    # satisfies the link check so accelerate-llvm-ptx compiles without a GPU.
    LIBRARY_PATH = "${pkgs.cudaPackages.cudatoolkit}/lib/stubs";

    # NIXOS specific fix
    NIX_ENFORCE_NO_NATIVE = "0";
  };

  scripts = {
    build.exec = ''
      set -e
      cd "$DEVENV_ROOT/report"
      exec latexmk -pdf -interaction=nonstopmode -halt-on-error -outdir=build "$@" main.tex
    '';

    watch.exec = ''
      set -e
      cd "$DEVENV_ROOT/report"
      exec latexmk -pdf -pvc -view=none -interaction=nonstopmode -halt-on-error -outdir=build "$@" main.tex
    '';

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
