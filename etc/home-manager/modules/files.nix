{ pkgs
, lib
, config
, ...
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
  home.file.".zfunc/_docker".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/docker/cli/a46f8504351c67fe00c610a279706d72d812f1a4/contrib/completion/zsh/_docker";
    sha256 = "0fj3niy4zw9yknjjfyv5j8xw5lx83pdq3xcm56g3mjqashjq26vj";
  };
}
