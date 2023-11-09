{ pkgs, ... }: {
  imports = [
    # ./emacs # wip
    ./docker.nix
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
      cascadia-code
      coreutils
      curl
      detect-secrets
      dogdns
      elixir
      elixir-ls
      eza
      fd
      figlet
      fantasque-sans-mono
      gnupg
      gnused
      httpie
      hyperfine
      ibm-plex
      infracost
      # NOTE(asm,2023-10-05): this requires some extra license/nonfree weirdness
      # input-fonts
      jetbrains-mono
      jq
      just
      magic-wormhole
      mtr
      nil
      nixpkgs-fmt
      nmap
      nodejs
      nodePackages.pyright
      openblas
      openssl
      pgbouncer
      postgresql
      python310Full
      # python310Packages.ruff-lsp
      python310Packages.mypy
      python310Packages.pipx
      rabbitmq-server
      reattach-to-user-namespace
      redis
      ripgrep
      rtx
      ruby
      # ruff
      shellcheck
      socat
      sops
      ssm-session-manager-plugin
      tflint
      terraform-ls
      terraform-lsp
      tokei
      tree
      trufflehog
      wget
      wrk
      xmlsec
      xsv
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

  # programs.exa = {
  #   enable = true;
  #   enableAliases = true;
  # };

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
