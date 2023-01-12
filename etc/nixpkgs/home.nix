{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "asm";
  home.homeDirectory = "/Users/asm";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # extra nix setup
  # nix.extraOptions =
  #   ''
  #     auto-optimise-store = true
  #     experimental-features = nix-command flakes
  #     extra-platforms = x86_64-darwin aarch64-darwin
  #   '';

  imports = [
    ./modules/packages
    ./modules/files.nix
    ./modules/environment.nix
  ];
}
