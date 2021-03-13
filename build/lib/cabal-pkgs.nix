#
# Functions to work with Cabal packages and build Nix derivations from
# them.
#
{ pkgs }:

let
  inherit (pkgs.lib.attrsets) mapAttrs;
  inherit (pkgs.lib.filesystem) haskellPathsInDir;
  inherit (pkgs.haskell.lib) doBenchmark dontBenchmark doCheck dontCheck
    doHaddock dontHaddock enableCabalFlag;
  stockCallCabal2nix = pkgs.haskellPackages.callCabal2nix;
in rec {

    # Call Cabal2Nix to build a derivation out of a Cabal file.
    # Use the Cabal file `pkgDirAbsPath/pkgName.cabal` and give the derivation
    # a name of `pkgName`. Accept optional arguments to tweak the derivation,
    # see below for the details.
    makeNixDrv = { # Use a specific `cabal2nix`.
                   callCabal2nix ? stockCallCabal2nix
                   # Run tests?
                 , withTests ? false
                   # Enable benchmarks?
                 , withBenches ? false
                   # Build Haddock documentation?
                 , withDocs ? false
                   # List of flags to pass to Cabal. We turn on each of
                   # them when building the Haskell source. Don't add the
                   # `-f`, e.g. use "release" instead of "-f release".
                   , withFlags ? []
                 , ...
                 }:
                 pkgName:
                 pkgDirAbsPath:
      let
        enableFlag = flag: drv: enableCabalFlag drv flag;

        fs = (if withTests then [ doCheck ] else [ dontCheck ])
          ++ (if withBenches then [ doBenchmark ] else [ dontBenchmark ])
          ++ (if withDocs then [ doHaddock ] else [ dontHaddock ])
          ++ (map enableFlag withFlags);

        bareDrv = callCabal2nix pkgName pkgDirAbsPath {};
      in
        builtins.foldl' (drv: f: f drv) bareDrv fs;    # (*)
    # NOTE
    # It doesn't look like we're able to override attrs at this stage.
    # E.g.
    #   bareDrv.overrideAttrs (oldAttrs: rec { doBenchmark = true; });
    # would've been easier but it has no effect, the derivation won't
    # include benchmark deps if there's some in the Cabal file. Also,
    # `bareDrv.override` isn't there so Nix pukes if you call it.

    # Find all Cabal packages underneath a given base directory and convert
    # them to Nix derivations. Take a Cabal package to be any sub-directory
    # `s` with an `s.cabal` file in it---i.e. the Cabal file name (without
    # extension) is the same as the enclosing directory's name. Name the
    # corresponding Nix derivation `s`.
    # Also accept any of the arguments of `makeNixDrv` to tweak derivations,
    # e.g. don't build docs. (Notice these options get applied to all drvs.)
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
    toNixDrvs = { baseDir, ... }@args:
      mapAttrs (makeNixDrv args) (haskellPathsInDir baseDir);  # (*)
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
