let
  pkgs = import <nixpkgs> { inherit config; };
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "db85cac9d0405b4769b75cba0b004aed3beaf2de";
    sha256 = "10nff6mqflrd6dz1fp2l9vmfwbgk0r7zm81qh2xnjj19a47pd7v3";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.8.4" ]; };
  
  compiler = "ghc884";
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
    allowBroken = true;
  };
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
            easy-hls
            cabal-install
          ];
      });
    exec = drv;
  }
