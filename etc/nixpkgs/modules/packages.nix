{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./git.nix
  ];

  home.packages = with pkgs;
    [
      neovim
      fd
    ];

  home.file.".config/nvim/init.vim".source = ../../nvim.vim;
}
