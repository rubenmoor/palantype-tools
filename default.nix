let
  compiler = "ghc8107";

  my-palantype-src = pkgs.fetchFromGitHub {
    owner = "rubenmoor";
    repo = "my-palantype";
    rev = "cf3145f41b3d34739ef98140edefc942e52b6342";
    sha256 = "1h1hhwgcvcxqwc1by1j7yzz4a8risijqd0m20lczh1f3djxqjdhx";
  };

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

  pkgs = import <nixpkgs> { inherit config; };

  easy-hls-src = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "ecb85ab6ba0aab0531fff32786dfc51feea19370";
    sha256 = "14v0jx8ik40vpkcq1af1b3377rhkh95f4v2cl83bbzpna9aq6hn2";
  };
  easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.7" ]; };

  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "palantype-tools" ./. { };
  env =
    with pkgs.haskellPackages;
    drv.env.overrideAttrs ( oldAttrs: rec {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++ [
          easy-hls
          cabal-install
        ];
    });
in
  if pkgs.lib.inNixShell then env else drv
