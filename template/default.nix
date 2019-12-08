let

  pkgs = import ../nixpkgs.nix;
  inherit (pkgs) lib;

  hpkgs = pkgs.haskell.packages.ghc865.override (old: {
    overrides = lib.composeExtensions (old.overrides or (self: super: {})) (self: super: {
      aoc@day@ = self.callCabal2nix "aoc@day@" (lib.sourceByRegex ./. [
        "^.*\\.hs$"
        "^input$"
        "^.*\\.cabal$"
      ]) {};
    });
  });
  
  env = hpkgs.shellFor {
    packages = p: [ p.aoc@day@ ];
    nativeBuildInputs = [ hpkgs.cabal-install ];
  };
in hpkgs.aoc@day@ // {
  inherit env;
}
