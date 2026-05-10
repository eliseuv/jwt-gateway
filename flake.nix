{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        systemLibs = with pkgs; [
          zlib
          zlib.dev
          ncurses
          gmp
          libsodium
        ];

        hpkgs = pkgs.haskell.packages.ghc96;
        ghcPackages = with hpkgs; [
          ghc
          haskell-language-server
          ormolu
          implicit-hie
        ];

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs =
            # Libraries needed for dynamic linking
            systemLibs
            # Main GHC packages
            ++ ghcPackages
            # Haskell tools
            ++ (with pkgs; [
              cabal-install
              ghcid
              ghciwatch
            ])
            # Other tools
            ++ (with pkgs; [
              pkg-config
            ]);

          # Fix for common linking errors in C libraries
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath systemLibs;

          shellHook = ''
            # Generate hie.yaml for LSP support
            ${pkgs.lib.getExe hpkgs.implicit-hie} > "$PWD/hie.yaml"
          '';
        };
      }
    );
}
