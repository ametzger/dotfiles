{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: let
  in {
    defaultPackage.aarch64-darwin = home-manager.defaultPackage.aarch64-darwin;
    defaultPackage.x86_64-darwin = home-manager.defaultPackage.x86_64-darwin;

    homeConfigurations."asm@asm-mbp-16" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-darwin;
      modules = [ ./home.nix ];
    };

    homeConfigurations.asm = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      modules = [ ./home.nix ];
    };
  };
}
