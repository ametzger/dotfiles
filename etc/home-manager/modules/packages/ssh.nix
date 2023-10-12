{
  programs.ssh = {
    enable = true;

    forwardAgent = false;
    extraConfig =
      ''
        AddKeysToAgent yes
        UseKeychain yes
      '';

    includes = [ "~/.ssh/config.private" ];

    matchBlocks = {
      "i-*" = {
        proxyCommand =
          ''
            sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
          '';
      };

      "util" = {
        user = "ec2-user";
        identityFile = "~/.ssh/id_ed25519";
        checkHostIP = false;
        extraOptions = {
          StrictHostKeyChecking = "no";
          UserKnownHostsFile = "/dev/null";
        };
        proxyCommand = "~/bin/util-ssh";
      };
    };
  };
}
