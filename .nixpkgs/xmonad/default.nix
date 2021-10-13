{ nixpkgs ? import <nixpkgs> { } }:

nixpkgs.xmonad-with-packages.override {
  packages = hPkgs: with hPkgs; [ xmonad-contrib xmonad-extras xmonad-wallpaper ];
}
