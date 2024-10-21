pkgs:
with pkgs;
let
  python = pkgs.python311;
  python_env = python.withPackages(ps:
    with ps; [
      pip
      ipython
      # running the tests
      pytest
      pytest-cache
      pytest-subtests
      pytest-pylint
      pytest-benchmark
      hypothesis
      # linting
      mypy
      pylint
      python-lsp-server
      rope
      pylsp-mypy
      pylsp-rope
      python-lsp-ruff
      # reformatting
      black
      isort
      # profiling
      memray
      # pytest-memray
      # bells and whistles
      rich
      icecream
    ]);
in

{
  build_pkgs = [];
  shell_pkgs = [python_env ruff];
}
