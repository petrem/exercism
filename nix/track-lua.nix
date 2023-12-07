pkgs:
with pkgs;
let
  lua = pkgs.lua5_4;
  lua_env = lua.withPackages(ps:
    with ps; [
      busted
      luacheck
    ]);
in

{
  build_pkgs = [];
  shell_pkgs = [lua_env];
}
