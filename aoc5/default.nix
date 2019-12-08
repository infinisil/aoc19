let

  pkgs = import ../nixpkgs.nix;
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      aoc5 = self.callCabal2nix "aoc5" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^input$"
        "^.*\\.cabal$"
      ]) {};

      polysemy-plugin = hlib.dontCheck (hlib.unmarkBroken super.polysemy-plugin);
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.aoc5 ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.aoc5 // {
  inherit env hpkgs;
}
