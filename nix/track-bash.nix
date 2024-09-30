pkgs:
with pkgs;
{
  build_pkgs = [];
  shell_pkgs = [bash bats shellcheck];
}
