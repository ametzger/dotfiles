{
  pkgs,
  lib,
  config,
  ...
}: let
  rtx = pkgs.callPackage ./rtx.nix { };
in {
  imports = [
    ./fish.nix
    ./git.nix
    ./nvim.nix
    ./ssh.nix
    ./tmux.nix
    ./zsh.nix
  ];

  home.packages = with pkgs;
    [
      awscli
      black
      coreutils
      curl
      elixir
      fd
      gnupg
      gnused
      httpie
      hyperfine
      jq
      just
      mtr
      nmap
      nodejs
      openblas
      openssl
      pgbouncer
      postgresql
      python310Full
      python310Packages.pipx
      rabbitmq-server
      reattach-to-user-namespace
      redis
      ripgrep
      rtx
      ruby
      starship
      tokei
      tree
      wget
      wrk
      xmlsec
      zlib
      zsh
    ];

  # home-manager derived configurations
  programs.bash = {
    enable = true;
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "Nord";
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.fzf = {
    enable = true;
    defaultCommand = "rg --files --hidden --no-heading --height 40%";
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  programs.go = {
    enable = true;
    goPath = "proj/go";
    goBin = "proj/go/bin";
  };

  programs.kakoune.enable = true;

  programs.zellij.enable = true;

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    enableFishIntegration = true;
  };
}
