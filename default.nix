let
  pkgs = import <nixpkgs> { inherit config; };
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "db85cac9d0405b4769b75cba0b004aed3beaf2de";
    sha256 = "10nff6mqflrd6dz1fp2l9vmfwbgk0r7zm81qh2xnjj19a47pd7v3";
  };
  streamly080 = pkgs.fetchFromGitHub {
    owner = "composewell";
    repo = "streamly";
    rev = "4f629b8cb36bd03b480edc08b77c9a0187ce2206";
    sha256 = "1ag6lqr74c1ml0vmai7a1b28dyf149pv3mhpg06kp27sawl71sy2";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.8.4" ]; };

  compiler = "ghc884";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
              my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
              parsec = self.callCabal2nix "parsec" ../parsec { };
              streamly = self.callCabal2nix "streamly" streamly080 { };
            };
          };
        };
      };
    };
    allowBroken = true;
  };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "palantype-tools" ./. { };
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
            brittany
          ];
      });
    exec = drv;
  }
