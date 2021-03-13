#
# Make a Nix overlay to build, develop and run the local Haskell packages
# in the project components directory. Have a look at the docs for each
# attribute below to see what's in it exactly.
# Got the basic idea from
# - https://github.com/cdepillabout/nix-cabal-example-project/blob/master/nixpkgs.nix
#
{
  # Nix packages from which to derive this overlay.
  pkgs

  # The GHC version to use. It determines the Haskell Nix package set in
  # `pkgs` from which all our Haskell deps come from.
  # NOTE. We need to know this since there's one Haskell package set in
  # Nixpkgs for each supported GHC version.
, ghcVersion

  # The name of the Haskell project to build. This overlay adds the various
  # project derivations we build to a top-level set having this name, e.g.
  # `pkgs.my-project`.
, projectName

  # The directory containing the Haskell local packages to build.
, componentsDir

  # An optional Haskell overlay function to override Haskell packages in the
  # Nix package set identified by `ghcVersion`.
, haskellPkgsOverrides ? hself: hsuper: {}

  # Optionally generate a Hoogle database for all the dependencies in the
  # dev shell as well as local Haskell packages. The Haddock docs of each
  # local package get built and become available for searching through
  # Hoogle---the Hoogle command gets added to your shell too.
  # See `devShell` attribute below.
, devWithHoogle ? false

  # Optional function to list extra derivations to add to the dev shell.
  # The `ps` argument points to this overlay, so besides adding any derivation
  # in the `pkgs` argument above, you can also add packages from the Haskell
  # set this overlay builds, e.g. `ps.my-project.haskell.cabal-plan` picks
  # the `cabal-plan` derivation from the Haskell package set that this overlay
  # uses (i.e. the one identified by `ghcVersion`) which isn't necessarily
  # the same as the one in `pkgs.haskellPackages`.
  # See `devShell` attribute below.
, devTools ? ps: []

  # Optional function to list extra derivations to add to the exe shell.
  # The `ps` argument points to this overlay, read explanation above for
  # how to use it.
  # See `exeShell` attribute below.
, runtimeExtras ? ps: []
}:

let
  inherit (import ./cabal-pkgs.nix { inherit pkgs; })
    drvListToSet drvListToExeSet overrideNixDrv toNixDrvs;

  # Add the project's Haskell local packages found in the components directory
  # to the Haskell overlay we're going to build for the project. Take a Haskell
  # package to be any sub-directory with a properly named Cabal file in it---see
  # `toNixDrvs` for the details. Add a convenience `_projPkgs_` attribute to
  # the returned set listing just the project derivations.
  haskellProjDrvs = hsuper:
    let
      projPkgs = toNixDrvs { baseDir = componentsDir;
                             callCabal2nix = hsuper.callCabal2nix;
                           };
    in
      projPkgs // { _projPkgs_ = builtins.attrValues projPkgs; };  # (*)
  # NOTE. projPkgs will contain an attribute for each derivation in the
  # original Nixpkgs Haskell package set plus an attribute for each derivation
  # we build out of the project's local Haskell packages. Since we need an
  # easy way to pick just the project's own packages out of `projPkgs`, we
  # add an extra attribute `_projPkgs_` to the set this function returns
  # that contains only the project's own packages.

  # Build the argument to the `haskell.shellFor` function we use to create
  # a dev shell---see `devShell` attribute below.
  devShellSpec = self: {
    nativeBuildInputs = [ self.cabal-install ] ++ (devTools self);

    packages = ps:                                                 # (1)
      let
        override = overrideNixDrv { withTests = true;
                                    withBenches = true;            # (2)
                                    withDocs = devWithHoogle;
                                  };
      in
        map override ps._projPkgs_;

    doBenchmark = true;                                            # (2)
    withHoogle = devWithHoogle;
  };
  # NOTE
  # 1. ps points to self."${projectName}".haskell
  # 2. `shellFor` doesn't bring in benchmark deps. Well, there's a knob to
  # ask for them (`doBenchmark`) but it doesn't look like it works unless
  # you also explicitly override the derivation attributes to include
  # benchmarks. It doesn't seem there's any side-effects to turning benchmarks
  # on if a package doesn't have any, so we can blindly turn them on for
  # every project package to make sure that if you do have a benchmark dep
  # (e.g. Criterion) in one of your packages, that dep will make it into the
  # dev shell's GHC DB.

in self: super:
{
  # TODO clean sources!!
  # TODO release builds!!

  # Nix overlay content. All the goodies we add sit inside a set named after
  # the input project name to avoid polluting the main Nix packages namespace
  # `pkgs` and keep all project derivations in one place, so you access them
  # with e.g. `pkgs.my-project.haskell.my-pkg-2` or `pkgs.my-project.devShell`.
  "${projectName}" = {

    # This set contains all the Haskell packages in the specified GHC Nixpkgs
    # set plus a derivation for each Haskell package in the specified project
    # components directory. We take a Haskell package to be any sub-directory
    # with a properly named Cabal file in it---see `toNixDrvs` for the details.
    haskell = self.haskell.packages."${ghcVersion}".override {
      overrides = hself: hsuper:
        haskellPkgsOverrides hself hsuper // haskellProjDrvs hsuper;
    };

    # This is a subset of `haskell` above, containing only the project's
    # Haskell local packages.
    components = drvListToSet self."${projectName}".haskell._projPkgs_;

    # This set contains a derivation for each exe built from the project's
    # Haskell local packages. It could obviously be empty if the there are
    # no executable components. Also, it won't include any test or benchmark
    # exes.
    executables = drvListToExeSet self."${projectName}".haskell._projPkgs_;

    # A derivation to start a Nix shell with GHC, Cabal, any additional dev
    # tool you'll need and all project dependencies found in the Cabal files
    # used to build the local packages in the `haskell` set above. Notice the
    # Nix environment won't have the local packages themselves in it. Check out
    # the `shellFor` function in Nixpkgs if this is Greek to you:
    # - https://github.com/NixOS/nixpkgs/blob/nixpkgs-unstable/pkgs/development/haskell-modules/make-package-set.nix
    devShell = self."${projectName}".haskell.shellFor (devShellSpec self);

    # A derivation to start a Nix shell with all the Haskell exes built from
    # the project's local packages, plus any other derivation listed in the
    # `runtimeExtras` input param.
    exeShell = self.mkShell {
      buildInputs = self."${projectName}".haskell._projPkgs_
                  ++ (runtimeExtras self);
    };

  };

}
