pkgs:
with pkgs;
{
  build_pkgs = [];
  shell_pkgs = [ jq
                 # test runner
                 bats
               ];
}
