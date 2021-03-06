#
# Functions to work with Cabal packages and build Nix derivations from
# them.
#
{ pkgs }:

let
  inherit (pkgs.lib.filesystem) haskellPathsInDir;
  inherit (pkgs.lib.attrsets) mapAttrs;
  stockCallCabal2nix = pkgs.haskellPackages.callCabal2nix;
in rec {

    # Find all Cabal packages underneath a given base directory and convert
    # them to Nix derivations. Take a Cabal package to be any sub-directory
    # `s` with an `s.cabal` file in it---i.e. the Cabal file name (without
    # extension) is the same as the enclosing directory's name. Name the
    # corresponding Nix derivation `s`.
    #
    # Example.
    #
    # /my/project/components
    #   pkg1
    #     pkg1.cabal
    #     ...
    #   pkg2
    #     weirdo.cabal
    #     ...
    #   pkg3
    #     pkg3.cabal
    #     ...
    #
    # toNixDrvs { baseDir = /my/project/components; }
    # => { pkg1 = <pkg1 drv>; pkg3 = <pkg3 drv>; }
    #
    toNixDrvs = { baseDir, callCabal2nix ? stockCallCabal2nix }:
      let
        toDrv = dirName: absPath: callCabal2nix dirName absPath {};
      in
        mapAttrs toDrv (haskellPathsInDir baseDir);    # (*)
      # NOTE
      # Have a look at haskellPathsInDir to see how it finds Cabal package
      # dirs. It returns a set { d1 = /path/to/d1; d2 = /path/to/d2; ...}.

    # Turn a list of Haskell derivations into a set containing an attribute
    # for each input derivation and where each attribute name is the package
    # name of the attribute value. Example:
    #
    # drvListToSet [ <pkg1 drv> <pkg2 drv> ]
    # => { pkg1 = <pkg1 drv>; pkg2 = <pkg2 drv>; }
    #
    drvListToSet = ds:
      let
        toNameValue = d: { name = d.pname; value = d; };
      in
        builtins.listToAttrs (map toNameValue ds);

    # Tell if the input derivation contains a Haskell executable.
    isExe = drv: builtins.pathExists "${drv}/bin";

    # Pick any listed derivation containing a Haskell executable and put it
    # in the output set with an attribute name equal to the package name and
    # attribute value set to the derivation. Ignore any other derivation.
    drvListToExeSet = ds: drvListToSet (builtins.filter isExe ds);

}
