{ config, pkgs, ... }: {
  home = {
    username = "asm";
    homeDirectory = "/Users/asm";
    stateVersion = "22.11";
  };

  programs.home-manager.enable = true;

  # extra nix setup
  # nix.extraOptions =
  #   ''
  #     auto-optimise-store = true
  #     experimental-features = nix-command flakes
  #     extra-platforms = x86_64-darwin aarch64-darwin
  #   '';

  # TODO(asm,2023-03-24): this is really slow, so disable for now
  manual.manpages.enable = false;
  programs.man.enable = false;

  imports = [
    ./modules/packages
    ./modules/files.nix
    ./modules/environment.nix
  ];
}
