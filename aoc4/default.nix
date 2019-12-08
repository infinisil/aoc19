let

  pkgs = import ../nixpkgs.nix;
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      aoc4 = self.callCabal2nix "aoc4" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^input$"
        "^.*\\.cabal$"
      ]) {};

      polysemy-plugin = hlib.dontCheck (hlib.unmarkBroken super.polysemy-plugin);
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.aoc4 ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.aoc4 // {
  inherit env hpkgs;
}
