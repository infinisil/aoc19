let

  pkgs = import ../nixpkgs.nix;
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      aoc6 = self.callCabal2nix "aoc6" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^input$"
        "^.*\\.cabal$"
      ]) {};

      polysemy-plugin = hlib.dontCheck (hlib.unmarkBroken super.polysemy-plugin);
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.aoc6 ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.aoc6 // {
  inherit env hpkgs;
}
