let
  pkgs = import ./nixpkgs.nix;
  inherit (pkgs) lib;

  days = lib.filter (day: builtins.pathExists (./. + "/${day}")) (map (n: "aoc" + toString n) (lib.range 1 25));

  packages = lib.genAttrs days (name: import (./. + "/${name}"));

  combinedBinary = pkgs.writeScriptBin "aoc" (''
    #!${pkgs.stdenv.shell}
  '' + lib.concatMapStrings (day: ''
    ${packages.${day}}/bin/${day}
  '') days);

  combined = pkgs.symlinkJoin {
    name = "aoc";
    paths = [combinedBinary] ++ lib.attrValues packages;
  };

in combined // packages // {
  inherit pkgs lib;
}
