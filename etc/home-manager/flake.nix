{
  description = "asm home-manager config";

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
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, nixpkgs, home-manager, devenv, emacs-overlay, nur, ... } @ inputs: let
    inherit (nixpkgs.lib) optionalAttrs singleton optionals;

    supportedSystems = ["x86_64-darwin" "aarch64-darwin"];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

    overlays =
      singleton (
        # Sub in x86 version of packages that don't build on Apple Silicon yet
        final: prev: (optionalAttrs (prev.stdenv.system == "aarch64-darwin") {
          inherit (final.pkgs-x86);
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

    nurModules = forAllSystems (
      system:
      import nur {
        nurpkgs = legacyPackages.x86_64-darwin;
        pkgs = legacyPackages.x86_64-darwin;
      }
    );

    homeManagerConfigs = forAllSystems (
      system: {
        pkgs = legacyPackages."${system}";
        modules = [
          {
            imports = [ nurModules."${system}".repos.rycee.hmModules.emacs-init ];
            home.packages = [devenv.packages."${system}".devenv];
          }
          ./home.nix
        ];
      });
 in {
    homeConfigurations."asm@asm-mbp-16" = home-manager.lib.homeManagerConfiguration homeManagerConfigs."x86_64-darwin";
    homeConfigurations.asm = home-manager.lib.homeManagerConfiguration homeManagerConfigs."aarch64-darwin";

    # TODO(asm,2023-03-24): This is a little hokey, but it means running `home-manager switch` or
    # `nix run . switch` works without specifying a flake path every time.
    defaultPackage.aarch64-darwin = self.homeConfigurations.asm.activationPackage;
    defaultPackage.x86_64-darwin = self.homeConfigurations."asm@asm-mbp-16".activationPackage;
  };
}
