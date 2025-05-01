pkgs:
with pkgs;
{
  build_pkgs = [];
  shell_pkgs = [nodejs nodejs.pkgs.pnpm];
  # shell_pkgs = [nodejs nodejs.pkgs.pnpm nodePackages.npm];
}
