{
  packageOverrides = pkgs_: with pkgs_; {
    xmonad = import ./xmonad { nixpkgs = pkgs_; };
  };
}

