pkgs:
with pkgs;
{
  build_pkgs = [gawk];
  shell_pkgs = [bats];
}
