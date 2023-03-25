{ config, lib, pkgs, ... }: {
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

    init = {
      enable = true;
      recommendedGcSettings = true;

      prelude = ''
      ;; Disable startup message.
      (setq inhibit-startup-message t
            inhibit-startup-echo-area-message (user-login-name))
      '';

      usePackage = {
        abbrev = {
          enable = true;
          diminish = [ "abbrev-mode" ];
          command = [ "abbrev-mode" ];
        };
      };
    };

    extraPackages = (epkgs:
      (with epkgs; [
        ace-window
        all-the-icons
        all-the-icons-dired
        anzu
        avy
        blacken
        browse-kill-ring
        cider
        clojure-mode
        company
        company-go
        company-jedi
        company-terraform
        counsel
        crux
        deadgrep
        direnv
        discover-my-major
        dockerfile-mode
        doom-modeline
        doom-themes
        dumb-jump
        dwim-shell-command
        easy-kill
        ein
        elixir-mode
        exec-path-from-shell
        expand-region
        flycheck-mypy
        flycheck-rust
        git-link
        git-timemachine
        go-eldoc
        go-guru
        go-mode
        go-projectile
        gorepl-mode
        hl-todo
        iedit
        imenu-anywhere
        imenu-list
        ivy
        ivy-prescient
        jedi
        jinja2-mode
        js2-mode
        json-mode
        just-mode
        lsp-ivy
        lsp-mode
        lsp-pyright
        lsp-ui
        magit
        markdown-mode
        move-text
        multiple-cursors
        neotree
        nix-mode
        org
        org-bullets
        persp-projectile
        perspective
        pipenv
        prescient
        projectile
        pt
        py-isort
        pyenv-mode
        racer
        rainbow-delimiters
        reformatter
        restclient
        rust-mode
        s
        sdlang-mode
        shackle
        smartparens
        swiper
        terraform-mode
        toml-mode
        typescript-mode
        undo-tree
        volatile-highlights
        vterm
        web-mode
        wgrep
        which-key
        yaml-mode
        yasnippet
        yasnippet-snippets
        zop-to-char
      ]));
  };
}
