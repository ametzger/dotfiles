{
  pkgs,
  lib,
  config,
  ...
}: {
  home.file.bin = {
    source = ../../../scripts;
    recursive = true;
  };

  home.file.".asdfrc".source = ../../asdfrc;
  home.file.".config/alacritty/alacritty.yml".source = pkgs.substituteAll {
    name = "alacritty.yml";
    src = ../../alacritty.yml;
    zsh = "${pkgs.zsh}";
  };
  home.file.".config/black".source = ../../black;
  home.file.".config/flake8".source = ../../flake8;
  home.file.".config/kitty/kitty.conf".source = ../../kitty.conf;
  home.file.".direnvrc".source = ../../direnvrc;
  home.file.".editorconfig".source = ../../editorconfig;
  home.file.".gemrc".source = ../../gemrc;
  home.file.".irbrc".source = ../../irbrc;
  home.file.".psqlrc".source = ../../psqlrc;
  home.file.".pylintrc".source = ../../pylintrc;
  home.file.".ripgreprc".source = ../../ripgreprc;
  home.file.".spacemacs".source = ../../spacemacs;
  home.file.".tool-versions".source = ../../tool-versions;
}
