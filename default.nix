{ sources ? import ./nix/sources.nix
, overlays ? import ./nix/overlays.nix { inherit sources; }, pkgs ?
  import sources.nixpkgs {
    inherit overlays;
    config = { allowUnfree = true; };
  } }:

with pkgs;

let
  servant-jsaddle-streams =
    (haskell.packages.ghc8104.callCabal2nix "servant-jsaddle-streams" ./.
      { }).overrideDerivation (args: {
        propagatedBuildInputs = args.propagatedBuildInputs ++ [ google-chrome ];
      });

in { inherit servant-jsaddle-streams; }
