#!/usr/bin/env bash

# TODO(asm,2023-01-11): can this move into justfile?
set -e

curl -L https://nixos.org/nix/install | sh

. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'

export NIX_PATH="$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels"

# nix-env --set-flag priority 0 nix-2.9.1

nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
nix-channel --add https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz home-manager

if [ "$(uname)" == "Darwin" ]; then
  nix-channel --add https://nixos.org/channels/nixpkgs-22.11-darwin nixpkgs
  # darwin-rebuild switch -I darwin-config="$XDG_CONFIG_HOME/nixpkgs/machines/$(uname | tr '[:upper:]' '[:lower:]')/default.nix"
fi

nix-channel --update

test -z "$(which home-manager)" && nix-shell '<home-manager>' -A install
