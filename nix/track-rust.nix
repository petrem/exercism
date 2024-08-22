pkgs:
with pkgs;
{
  build_pkgs = [];
  shell_pkgs = [pkgs.cargo pkgs.rustc pkgs.clippy pkgs.rustfmt];
}
