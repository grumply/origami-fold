{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = new: old: rec {

              trivial =
                new.callPackage ./deps/trivial/trivial.nix { };

              origami-fold =
                new.callPackage ./origami-fold.nix { };

            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { origami-fold = pkgs.haskell.packages.${compiler}.origami-fold;
    trivial = pkgs.haskell.packages.${compiler}.trivial;
  }

