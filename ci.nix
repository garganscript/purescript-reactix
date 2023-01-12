let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz";
  };
in

{ pkgs ? import nixpkgs {} }:

  let
    ezPscSrc = pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "master";
      sha256 = "tESal32bcqqdZO+aKnBzc1GoL2mtnaDtj2y7ociCRGA=";
    };
    ezPsc = import ezPscSrc { inherit pkgs; };
  in

    pkgs.mkShell {
      buildInputs = [
        ezPsc.purs-0_15_7
        ezPsc.psc-package
        ezPsc.spago
        pkgs.nodejs
      ];
    }
