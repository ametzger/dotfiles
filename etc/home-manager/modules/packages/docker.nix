{ pkgs, ... }: {
  home.packages = with pkgs;
    [
      colima
      docker-client
    ];

  home.file.".zfunc/_docker".source = builtins.fetchurl {
    url = "https://raw.githubusercontent.com/docker/cli/a46f8504351c67fe00c610a279706d72d812f1a4/contrib/completion/zsh/_docker";
    sha256 = "0fj3niy4zw9yknjjfyv5j8xw5lx83pdq3xcm56g3mjqashjq26vj";
  };
}
