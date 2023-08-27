pkgs:
with pkgs;
let
  inherit (lib) optional;
in
{
  build_pkgs = [gcc gnumake];
  shell_pkgs = [cppcheck] ++ optional stdenv.isLinux valgrind;
}
