{ config, pkgs, ... }:
let
in {
  home.username = "asm";
  home.homeDirectory = "/Users/asm";
  home.stateVersion = "22.11";
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
