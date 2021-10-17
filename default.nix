let
  compiler = "ghc8104";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
              # hyphenation = self.callCabal2nix "hyphenation" ../ekmett-hyphenation { };
            };
          };
        };
      };
    };
    # allowBroken = true;
  };
  pkgs = import <nixpkgs> { inherit config; };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "buildStenoDict" ./. { };
in
  {
    env =
      # don't know why, but the haskell-language doesn't seem to
      # be a build tool, but a native build input
      #
      # with pkgs.haskell.lib;
      # addBuildTools drv (
      #   with pkgs.haskellPackages;
      #   [ haskell-language-server ]
      # );
      with pkgs.haskellPackages;
      drv.env.overrideAttrs ( oldAttrs: rec {
        nativeBuildInputs =
          oldAttrs.nativeBuildInputs ++ [
            haskell-language-server
            cabal-install
          ];
      });
    exec = drv;
  }
