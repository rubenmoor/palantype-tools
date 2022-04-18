let
  pkgs = import <nixpkgs> { inherit config; };
  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "7c123399ef8a67dc0e505d9cf7f2c7f64f1cd847";
    sha256 = "0402ih4jla62l59g80f21fmgklj7rv0hmn82347qzms18lffbjpx";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.7" ]; };

  my-palantype-src = pkgs.fetchFromGitHub {
    owner = "rubenmoor";
    repo = "my-palantype";
    rev = "cf3145f41b3d34739ef98140edefc942e52b6342";
    sha256 = "1h1hhwgcvcxqwc1by1j7yzz4a8risijqd0m20lczh1f3djxqjdhx";
  };

  compiler = "ghc8107";
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: {
              my-palantype = self.callCabal2nix "my-palantype" ../my-palantype { };
              #my-palantype = self.callCabal2nix "my-palantype" my-palantype-src { };
              bytestring-trie = pkgs.haskell.lib.doJailbreak (super.bytestring-trie);
            };
          };
        };
      };
    };
    allowBroken = true;
  };
  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "palantype-tools" ./. { };
  env =
    with pkgs.haskellPackages;
    drv.env.overrideAttrs ( oldAttrs: rec {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++ [
          easy-hls
          cabal-install
          threadscope
        ];
    });
in
  if pkgs.lib.inNixShell then env else drv
