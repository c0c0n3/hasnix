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
#      hedgehog = hself.callHackage "hedgehog" "1.0.4" {};
      # nixpin has: hedgehog-1.0.3
#      base-unicode-symbols =
#        hself.callHackage "base-unicode-symbols" "0.2.4.1" {};
#        # nixpin has base-unicode-symbols-0.2.4.2
    };
    devWithHoogle = true;
    devTools = ps: [ ps."${project.name}".haskell.cabal-plan ];
       #[ pinnedPkgs.haskellPackages.cabal-plan ];
    runtimeExtras = ps: with ps; [ hello ];
  };

# TODO fn to generate plain docker imgs for each Haskell exe

  p = import nixpin {
    overlays = [ projectOverlay ];
  };

}
