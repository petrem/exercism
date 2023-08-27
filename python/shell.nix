{ pkgs ?
  let sources = import ../nix/sources.nix;
  in import sources.nixpkgs {config.allowUnfree = true;} }:

with pkgs;
let
  inherit (lib) optional optionals;
  python = pkgs.python311;
  pythonPackages = pkgs.python311Packages;
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
                                   pytest-benchmark
                                   hypothesis
                                   # linting
                                   flake8
                                   mypy
                                   pylint
                                   ruff
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
