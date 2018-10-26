let
  localLib = import ../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? localLib.commitIdFromGitRepo ../../.git
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
, cardano-sl-explorer
}:

with pkgs.lib;

let
  cleanSourceFilter = with pkgs.stdenv;
    name: type: let baseName = baseNameOf (toString name); in ! (
      # Filter out .git repo
      (type == "directory" && baseName == ".git") ||
      # Filter out editor backup / swap files.
      lib.hasSuffix "~" baseName ||
      builtins.match "^\\.sw[a-z]$" baseName != null ||
      builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

      # Filter out locally generated/downloaded things.
      baseName == "bower_components" ||
      (type == "directory" && (baseName == "node_modules" || baseName == "dist")) ||

      # Filter out the files which I'm editing often.
      lib.hasSuffix ".nix" baseName ||
      # Filter out nix-build result symlinks
      (type == "symlink" && lib.hasPrefix "result" baseName)
    );

  src = builtins.filterSource cleanSourceFilter ./.;

  bowerComponents = pkgs.buildBowerComponents {
    name = "cardano-sl-explorer-frontend-deps";
    generated = ./nix/bower-generated.nix;
    inherit src;
  };

  generatedSrc = pkgs.runCommand "cardano-sl-explorer-frontend-src" {
    inherit src bowerComponents;
    buildInputs = [ regen-script ];
  } ''
    cp -R --reflink=auto $src $out
    chmod -R u+w $out
    cd $out
    rm -rf .psci_modules .pulp-cache bower_components output result

    # Purescript code generation
    regen

    # Frontend dependencies
    ln -s $bowerComponents/bower_components .

    # Patch the build recipe for nix
    echo "patching webpack.config.babel.js"
    sed -e "s/COMMIT_HASH.*/COMMIT_HASH': '\"${gitrev}\"',/" \
        -e "s/import GitRevisionPlugin.*//" \
        -e "s/path:.*/path: process.env.out,/" \
        -e "/new ProgressPlugin/d" \
        -i webpack.config.babel.js
  '';

  # p-d-l does not build with our main version of nixpkgs.
  # Needs to use something off 17.03 branch.
  oldHaskellPackages = (import (pkgs.fetchzip {
    url = "https://github.com/NixOS/nixpkgs/archive/cb90e6a0361554d01b7a576af6c6fae4c28d7513.tar.gz";
    sha256 = "0gr25nph2yyk89j2g5zxqm2177lkh0cyy8gnzm6xcywz1qwf3zzf";
  }) {}).pkgs.haskell.packages.ghc802.override {
    overrides = self: super: {
      purescript-derive-lenses = oldHaskellPackages.callPackage ./nix/purescript-derive-lenses.nix {};
    };
  };

  yarn2nix = import (pkgs.fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) {
    # nixpkgs 18.03 branch contains yarn 1.5.1
    inherit pkgs;
    nodejs = pkgs.nodejs-6_x;
  };

  regen-script = pkgs.writeScriptBin "regen" ''
    export PATH=${makeBinPath [oldHaskellPackages.purescript-derive-lenses cardano-sl-explorer]}:$PATH
    cardano-explorer-hs2purs --bridge-path src/Generated/
    scripts/generate-explorer-lenses.sh
  '';

  frontend = { stdenv, python, purescript, mkYarnPackage }:
    mkYarnPackage {
      name = "cardano-explorer-frontend";
      src = generatedSrc;
      yarnLock = ./yarn.lock;
      packageJSON = ./package.json;
      extraBuildInputs = [
        purescript
        regen-script
      ];
      passthru = { inherit bowerComponents; };
      installPhase = ''
        # run the build:prod script
        export PATH=$(pwd)/node_modules/.bin:$PATH
        export NODE_ENV=production
        export HOME=$(pwd)/webpack-home
        webpack --config webpack.config.babel.js
      '';
    };

in

  pkgs.callPackage frontend {
    inherit (yarn2nix) mkYarnPackage;
  }
