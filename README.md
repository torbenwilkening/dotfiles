# Dotfiles for Quick Setup

Install [nix](https://nixos.org/download/)  

Install [home-manager](https://nix-community.github.io/home-manager/index.xhtml) as a flake  

## Linux

Get configs from nix/linux to /etc/nixos and run `nixos-rebuild switch --use-remote-sudo`  

## Darwin

Get configs from nix/darwin to /etc/nix-darwin and run `darwin-rebuild switch`  
