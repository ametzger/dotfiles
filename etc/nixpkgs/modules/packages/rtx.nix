{ pkgs, lib, fetchFromGitHub, rustPlatform, coreutils, bash, direnv, perl }:
rustPlatform.buildRustPackage {
  # copied from https://github.com/jdxcode/rtx/blob/552c27d85cbea71f99a65e5ad30bf6356a8623cc/default.nix
  pname = "rtx";
  version = "1.20.3";

  src = fetchFromGitHub {
    owner = "jdxcode";
    repo = "rtx";
    rev = "552c27d85cbea71f99a65e5ad30bf6356a8623cc";
    sha256 = "sha256-3iElIPNc/2qpQJuh2vtqBcJVFEwx5RWOu0CRwWw0984=";
  };

  cargoHash = "sha256-YaNwkRUBoxV7EjSexYjl0DldqkbDj+ziCQJpccEa5d4=";

  # HACK(asm,2023-03-02): rtx has a nix overlay available in the repo, but it fails to build on
  # macOS - needed to add the Security framework thing here.
  buildInputs = with pkgs; [ darwin.apple_sdk.frameworks.Security coreutils bash direnv gnused git gawk ];

  prePatch = ''
    substituteInPlace ./test/data/plugins/**/bin/* \
      --replace '#!/usr/bin/env bash' '#!${bash}/bin/bash'
    substituteInPlace ./src/fake_asdf.rs ./src/cli/reshim.rs \
      --replace '#!/bin/sh' '#!${bash}/bin/sh'
    substituteInPlace ./src/env_diff.rs \
      --replace '"bash"' '"${bash}/bin/bash"'
    substituteInPlace ./src/cli/direnv/exec.rs \
      --replace '"env"' '"${coreutils}/bin/env"' \
      --replace 'cmd!("direnv"' 'cmd!("${direnv}/bin/direnv"'
  '';

  # Skip the test_plugin_list_urls as it uses the .git folder, which
  # is excluded by default from Nix.
  checkPhase = ''
    RUST_BACKTRACE=full cargo test --features clap_mangen -- \
      --skip cli::plugins::ls::tests::test_plugin_list_urls
  '';

  # Need this to ensure openssl-src's build uses an available version of `perl`
  # https://github.com/alexcrichton/openssl-src-rs/issues/45
  OPENSSL_SRC_PERL = "${perl}/bin/perl";

  meta = with lib; {
    description = "Polyglot runtime manager (asdf rust clone)";
    homepage = "https://github.com/jdxcode/rtx";
    license = licenses.mit;
  };
}
