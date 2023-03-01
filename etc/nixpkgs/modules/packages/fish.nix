{
  programs.fish = {
    enable = true;

    functions = {
      bu =
        ''
          brew update
          brew upgrade
        '';
      sudo =
        ''
          if test "$argv" = !!
              eval command sudo $history[1]
          else
              command sudo $argv
          end
        '';
    };

    shellInit =
      ''
        # suppress fish prompt greeting
        set -g -x fish_greeting '''
      '';
  };
}
