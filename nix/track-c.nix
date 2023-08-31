pkgs:
with pkgs;
let
  inherit (lib) optional;
in
{
  build_pkgs = [];
  shell_pkgs = [gcc gnumake cppcheck] ++ optional stdenv.isLinux valgrind;
}
