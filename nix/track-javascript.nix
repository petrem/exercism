pkgs:
with pkgs;
{
  build_pkgs = [];
  shell_pkgs = [nodejs nodePackages.npm];
}
