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
              #bytestring-trie = pkgs.haskell.lib.doJailbreak (super.bytestring-trie);
            };
          };
        };
      };
    };
    allowBroken = true;
  };

  pkgs = import <nixpkgs> { inherit config; };

  drv = pkgs.haskell.packages."${compiler}".callCabal2nix "palantype-tools" ./. { };
  env =
    with pkgs.haskellPackages;
    drv.env.overrideAttrs ( oldAttrs: rec {
      nativeBuildInputs =
        oldAttrs.nativeBuildInputs ++ [
          cabal-install
        ];
    });
in
  if pkgs.lib.inNixShell then env else drv
