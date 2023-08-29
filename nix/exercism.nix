pkgs:

with pkgs;
let
  inherit (lib) optional optionals;
  unjust = import ./unjust.nix {inherit pkgs;};
in

rec {
  platform_pkgs = optional stdenv.isLinux inotify-tools
                  ++ optionals stdenv.isDarwin (
                    with darwin.apple_sdk.frameworks;
                    [ CoreFoundation
                      CoreServices ]);
  build_pkgs = platform_pkgs ++ [];
  shell_pkgs = [ direnv
                 entr
                 exercism
                 git
                 unjust
                 niv ];
}
