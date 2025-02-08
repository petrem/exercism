pkgs:
with pkgs;
let haskell_env = haskellPackages.ghcWithPackages (
      pkgs: with pkgs; [
        # stack
        # haddock
        haskell-language-server
        hlint
        ghcid
        stack
        stack-clean-old
      ]);
in
{
  build_pkgs = [];
  shell_pkgs = [haskell_env ormolu];
}

  # Further read:
  # * https://nixos.wiki/wiki/Haskell
  # * https://docs.haskellstack.org/en/stable/nix_integration/
  # * https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
