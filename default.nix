{ nixpkgs ? import <nixpkgs> { } }:


let
  pkgs = [
    (nixpkgs.xmonad-with-packages.override {
      packages = hPkgs: with hPkgs; [ xmonad-contrib xmonad-extras xmonad-wallpaper ];
    })
    nixpkgs.xmobar
    nixpkgs.feh
  ];

in
nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = pkgs;
}
