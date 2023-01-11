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

  home.file.".config/nvim/init.vim".source = ../../nvim.vim;
}
