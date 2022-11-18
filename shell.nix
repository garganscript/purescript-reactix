let
  pkgs = import <nixpkgs> {};
  default-shell = import (
    pkgs.fetchFromGitHub {
      owner = "garganscript";
      repo = "package-sets";
      rev = "master";
      sha256 = "VFRGYoJK4mRfeP/CLmi+F6M+aZTP6dNAc4QGgebx9a0=";
    } + "/default-shell.nix");
in
pkgs.mkShell {
  name = "purescript-reactix";

  buildInputs = [
    default-shell.purs
    default-shell.easy-ps.psc-package
    default-shell.easy-ps.spago
    default-shell.build
    default-shell.pkgs.dhall-json
  ];
}
