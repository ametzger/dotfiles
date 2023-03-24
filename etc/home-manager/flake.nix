{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devenv.url = "github:cachix/devenv/latest";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay/702b1724ead7b6eec28bfc5e1404c26a57a3b248";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, devenv, emacs-overlay, ... } @ inputs: let
    inherit (nixpkgs.lib) optionalAttrs singleton optionals;

    supportedSystems = ["x86_64-darwin" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    overlays =
      singleton (
        # Sub in x86 version of packages that don't build on Apple Silicon yet
        final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          inherit
            (final.pkgs-x86)
            ;
        })
      )
      ++ [emacs-overlay.overlay];

    legacyPackages = forAllSystems (
      system:
        import nixpkgs {
          inherit system;
          overlays = overlays;
          config = {
            allowUnfree = true;
          };
        }
    );
  in {
    defaultPackage.aarch64-darwin = self.homeConfigurations.asm.activationPackage;
    defaultPackage.x86_64-darwin = self.homeConfigurations."asm@asm-mbp-16".activationPackage;

    homeConfigurations."asm@asm-mbp-16" = home-manager.lib.homeManagerConfiguration {
      pkgs = legacyPackages."x86_64-darwin";
      modules = [ ./home.nix ];
    };

    homeConfigurations.asm = home-manager.lib.homeManagerConfiguration {
      pkgs = legacyPackages."aarch64-darwin";

      modules = [
        ./home.nix
        {
          home = {
            packages = [devenv.packages.aarch64-darwin.devenv];
          };
        }
      ];
    };
  };
}
