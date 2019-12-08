let

  pkgs = import ../nixpkgs.nix;
  inherit (pkgs) lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      aoc1 = self.callCabal2nix "aoc1" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^input$"
        "^.*\\.cabal$"
      ]) {};
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.aoc1 ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.aoc1 // {
  inherit env;
}
