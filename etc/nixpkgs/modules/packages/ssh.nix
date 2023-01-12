{
  programs.ssh = {
    enable = true;

    forwardAgent = false;
    extraConfig =
      ''
        AddKeysToAgent yes
        UseKeychain yes
      '';

    matchBlocks = {
      "i-*" = {
        proxyCommand =
          ''
            sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
          '';
      };
    };
  };
}
