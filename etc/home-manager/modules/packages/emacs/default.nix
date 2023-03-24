{ config, lib, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = with pkgs; pkgs.emacsPgtk.overrideAttrs (old: {
      patches =
        (old.patches or [])
        ++ [
          # Fix OS window role (needed for window managers like yabai)
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-28/fix-window-role.patch";
            sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
          })
          # Use poll instead of select to get file descriptors
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-29/poll.patch";
            sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
          })
          # Make Emacs aware of OS-level light/dark mode
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-28/system-appearance.patch";
            sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
          })
        ];
    });
  };
}
