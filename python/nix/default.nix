{ pkgs ? import <nixpkgs> {}
, python ? pkgs.python311
, pythonPackages ? pkgs.python311Packages
, extra_python_packages ? []
}:

let
  app_python_packages = with pythonPackages; [];
  python_env = (
      python.buildEnv.override  {
        extraLibs = builtins.concatLists [ app_python_packages extra_python_packages];
      }
    );
in pkgs.buildEnv {
  name = "exercism-python-environment";
  paths = [
    pkgs.jq  # we don't really need jq, but maybe we'd need something else
    python_env
  ];
}
