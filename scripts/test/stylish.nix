{ runCommand, stylish-haskell, src, lib, diffutils, glibcLocales }:

let
  localLib = import ../../lib.nix;

  # just haskell sources and the stylish-haskell config file
  src' = lib.cleanSourceWith {
   inherit src;
   filter = with lib;
    name: type: let baseName = baseNameOf (toString name); in (
      (type == "regular" && hasSuffix ".hs" baseName) ||
      (type == "regular" && hasSuffix ".yaml" baseName) ||
      (type == "directory")
    );
  };
in
runCommand "cardano-stylish-check" {
  succeedOnFailure = true;
  buildInputs = [ stylish-haskell diffutils glibcLocales ];
} ''
  set +e
  ${localLib.utf8LocaleSetting}
  cp -a ${src'} orig
  cp -a ${src'} stylish
  chmod -R +w stylish
  cd stylish
  find . -type f -name "*hs" -not -name 'HLint.hs' -exec stylish-haskell -i {} \;
  cd ..
  diff --brief --recursive orig stylish > /dev/null
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    mkdir -p $out/nix-support
    diff -ur orig stylish > $out/stylish.diff
    echo "file none $out/stylish.diff" > $out/nix-support/hydra-build-products
    echo "*** Stylish-haskell found changes that need addressed first"
    echo "*** Please run \`nix-shell -A fixStylishHaskell\` and commit changes"
    echo "*** or apply the diff generated by hydra if you don't have nix."
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
