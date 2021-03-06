{
  pkgs ? import <nixpkgs> {}
}:

let
  inherit (pkgs) fetchFromGitHub;
  version = import ./config/version.nix;
  project = import ./config/project.nix;
  makeProjectOverlay = import ./lib/haskell-overlay.nix;
in rec {

  nixpin = fetchFromGitHub version.nixpkgsGitHub;
  pinnedPkgs = import nixpin {};

  projectOverlay = makeProjectOverlay {
    pkgs = pinnedPkgs;
    ghcVersion = version.ghc;
    projectName = project.name;
    componentsDir = project.componentsDir;
    haskellPkgsOverrides = hself: hsuper: {
      base-unicode-symbols =
        hself.callHackage "base-unicode-symbols" "0.2.4.1" {};
        # nixpin has base-unicode-symbols-0.2.4.2
    };
    devTools = [ pinnedPkgs.haskellPackages.cabal-plan ];
    # runtimeDeps = [ pinnedPkgs.hello ];
  };

# TODO fn to generate plain docker imgs for each Haskell exe

  p = import nixpin {
    overlays = [ projectOverlay ];
  };

}
