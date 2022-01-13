let
  pkgs = import <nixpkgs> { inherit config; };
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "7c123399ef8a67dc0e505d9cf7f2c7f64f1cd847";
    sha256 = "0402ih4jla62l59g80f21fmgklj7rv0hmn82347qzms18lffbjpx";
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
          ];
      });
    exec = drv;
  }
