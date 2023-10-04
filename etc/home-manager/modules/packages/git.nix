{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.git;

    userName = "Alex Metzger";
    userEmail = "asm@asm.io";
    signing = { key = "974B7213E1816927"; };

    aliases = {
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      d = "diff --ignore-space-at-eol -b -w";
      ds = "diff --staged --ignore-space-at-eol -b -w";
      unstage = "reset HEAD --";
      s = "status -sb";
      alias = "!git config --list | grep 'alias\\.' | sed 's/alias\\.\\([^=]*\\)=\\(.*\\)/\\1\\ => \\2/' | sort";
      aliases = "!git alias";
      assume = "update-index --assume-unchanged";
      unassume = "update-index --no-assume-unchanged";
      assumed = "!git ls-files -v | grep ^h | cut -c 3-";
      up = "pull --ff-only --all -p";
      mine = "!git lg --author=\"Alex Metzger\"";
      rmu = "!git status -s|grep '??'|sed -e 's/?? //'|xargs rm";
    };

    extraConfig = {
      github = { user = "ametzger"; };
      gitlab = { user = "ametzger"; };
      credential = { helper = "osxkeychain"; };
      core = {
        editor = "emacsclient -t -a=\\\"\\\"";
        whitespace = "trailing-space,space-before-tab,-indent-with-non-tab";
        filemode = "false";
        excludesfile = "~/.gitignore_global";
        attributesfile = "~/.gitattributes";
      };
      push = { default = "simple"; };
      color = { ui = "auto"; };
      log = { decorate = "true"; };
      pull = { ff = "only"; };
      branch = { autoSetupMerge = "always"; };
      diff = { algorithm = "patience"; };
      rebase = { autoStash = "true"; };
      init = { defaultBranch = "main"; };
      advice = { detachedHead = "false"; };
      mergetool = {
        keepBackup = "false";
        keepTemporaries = "false";
        prompt = "false";
      };
    };
  };

  home.file.".gitattributes".text =
    ''
      *.c     diff=cpp
      *.h     diff=cpp
      *.c++   diff=cpp
      *.h++   diff=cpp
      *.cpp   diff=cpp
      *.hpp   diff=cpp
      *.cc    diff=cpp
      *.hh    diff=cpp
      *.m     diff=objc
      *.mm    diff=objc
      *.cs    diff=csharp
      *.css   diff=css
      *.html  diff=html
      *.xhtml diff=html
      *.ex    diff=elixir
      *.exs   diff=elixir
      *.go    diff=golang
      *.php   diff=php
      *.pl    diff=perl
      *.py    diff=python
      *.md    diff=markdown
      *.rb    diff=ruby
      *.rake  diff=ruby
      *.rs    diff=rust
      *.lisp  diff=lisp
      *.el    diff=lisp
    '';

  home.file.".gitignore_global".text =
    ''
      # -*- mode: gitignore; -*-

      # Compiled source #
      ###################
      *.com
      *.class
      *.dll
      *.exe
      *.o
      *.so

      # Packages #
      ############
      # it's better to unpack these files and commit the raw source
      # git has its own built in compression methods
      *.7z
      *.dmg
      *.gz
      *.iso
      *.jar
      *.rar
      *.tar
      *.zip

      # Logs and databases #
      ######################
      *.log

      # OS generated files #
      ######################
      .DS_Store
      .DS_Store?
      ._*
      .Spotlight-V100
      .Trashes
      ehthumbs.db
      Thumbs.db

      *.swp
      *.swo
      *.swn

      # Emacs stuff #
      ###############
      *~
      \#*\#
      /.emacs.desktop
      /.emacs.desktop.lock
      *.elc
      auto-save-list
      tramp
      .\#*

      # Org-mode
      .org-id-locations
      *_archive

      # flymake-mode
      *_flymake.*

      # eshell files
      /eshell/history
      /eshell/lastdir

      # elpa packages
      /elpa/

      # reftex files
      *.rel

      # AUCTeX auto folder
      /auto/

      # cask packages
      .cask/
      dist/

      # Flycheck
      flycheck_*.el

      # server auth directory
      /server/

      # projectiles files
      .projectile

      # directory configuration
      .dir-locals.el

      # jetbrains crap
      .idea
      /.envrc

      # direnv
      .envrc
      .env
      .direnv
    '';
}
