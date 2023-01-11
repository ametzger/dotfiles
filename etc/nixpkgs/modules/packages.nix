{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./git.nix
  ];

  home.packages = with pkgs;
    [
      awscli
      bat
      black
      coreutils
      curl
      direnv
      elixir
      exa
      fd
      fish
      fzf
      gnused
      gnupg
      go
      httpie
      hyperfine
      jq
      kakoune
      xmlsec
      mtr
      neovim
      nmap
      nodejs
      openblas
      openssl
      pgbouncer
      postgresql
      python310Full
      rabbitmq-server
      reattach-to-user-namespace
      redis
      ripgrep
      ruby
      starship
      tmuxinator
      tokei
      tree
      wget
      wrk
      zlib
      zoxide
      zsh
    ];

  home.file.".asdfrc".source = ../../asdfrc;
  home.file.".bashrc".source = ../../bashrc;
  home.file.".config/alacritty/alacritty.yml".source = ../../alacritty.yml;
  home.file.".config/black".source = ../../black;
  home.file.".config/fish/config.fish".source = ../../config.fish;
  # home.file.".config/fish/functions".source = ../../fish-functions;
  home.file.".config/flake8".source = ../../flake8;
  home.file.".config/kitty/kitty.conf".source = ../../kitty.conf;
  home.file.".config/nvim/init.vim".source = ../../nvim.vim;
  home.file.".config/zellij/config.kdl".source = ../../zellij.kdl;
  home.file.".direnvrc".source = ../../direnvrc;
  home.file.".editorconfig".source = ../../editorconfig;
  home.file.".gemrc".source = ../../gemrc;
  home.file.".hyper.js".source = ../../hyper.js;
  home.file.".irbrc".source = ../../irbrc;
  home.file.".psqlrc".source = ../../psqlrc;
  home.file.".pylintrc".source = ../../pylintrc;
  home.file.".ripgreprc".source = ../../ripgreprc;
  home.file.".spacemacs".source = ../../spacemacs;
  home.file.".ssh/config".source = ../../sshconfig;
  home.file.".tmux.conf".source = ../../tmux.conf;
  home.file.".tmuxinator.yml".source = ../../tmuxinator.yml;
  home.file.".tool-versions".source = ../../tool-versions;

}
