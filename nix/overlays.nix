{ sources ? import ./sources.nix, hostPkgs ? import sources.nixpkgs { } }:

let
  cabalHashes = hostPkgs.fetchurl {
    url =
      "https://github.com/commercialhaskell/all-cabal-hashes/archive/2b715ac8df3179be92d4b60ceb26b2dc3f89f98f.tar.gz";
    sha256 = "1gf899f2njrazc83g6l82jnpasvnbjm60yic6p45h215hgph1x6y";
  };

  haskellOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc8104 = super.haskell.packages.ghc8104.override {
          all-cabal-hashes = cabalHashes;
          overrides = selfGhc: superGhc: {
            servant = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant" "0.18.2" { });
            servant-client-core = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant-client-core" "0.18.2" { });
            servant-server = self.haskell.lib.dontCheck
              (selfGhc.callHackage "servant-server" "0.18.2" { });
          };
        };
      };
    };
  };
in [ haskellOverlay ]
