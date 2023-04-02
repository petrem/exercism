{ pkgs ? import <nixpkgs> {config.allowUnfree = true;} }:

with pkgs;
let
  inherit (lib) optional optionals;
  python = pkgs.python39;
  pythonPackages = pkgs.python39Packages;
in

mkShell {
  buildInputs = [
    (
      import ./nix/default.nix { inherit pkgs;
                                 inherit python;
                                 inherit pythonPackages;
                                 extra_python_packages = with pythonPackages; [
                                   pip      # to explore around...
                                   ipython  # repl is good
                                   # running tests
                                   pytest
                                   pytest-cache
                                   pytest-subtests
                                   pytest-pylint
                                   # linting
                                   # todo: how to specify prospector[with_vulture, with_mypy, with_bandit] ?
                                   prospector
                                   vulture
                                   mypy
                                   bandit
                                   # reformatting
                                   black
                                   isort
                                   # sometimes used while debugging
                                   rich
                                   icecream
                                 ];
                               }
    )
    direnv
    entr
    exercism
    git
    just
  ] ++ optional stdenv.isLinux inotify-tools
  ++ optional stdenv.isDarwin terminal-notifier
  ++ optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    CoreFoundation
    CoreServices
  ]);
}
