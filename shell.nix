{ sources ? import ./nix/sources.nix
, overlays ? import ./nix/overlays.nix { inherit sources; }, pkgs ?
  import sources.nixpkgs {
    inherit overlays;
    config = { allowUnfree = true; };
  } }:

with pkgs;

let
  project = import ./. {
    inherit sources;
    inherit pkgs;
  };
in project.servant-jsaddle-streams.env.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs ++ [
    google-chrome
    haskell.packages.ghc8104.cabal-install
    haskell.packages.ghc8104.ghcid
    haskell.packages.ghc8104.ghcide
    haskell.packages.ghc8104.haskell-language-server
  ];
})
