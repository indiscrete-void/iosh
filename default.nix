let
    sources = {
        haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
    };

    haskellNix = import sources.haskellNix {};
    pkgs = import
        haskellNix.sources.nixpkgs-2311
        haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "iosh";
        src = ./.;
    };

    compiler-nix-name = "ghc928";
}
