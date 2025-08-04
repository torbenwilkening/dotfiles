{ config, pkgs, dotfiles, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "torben";
  home.homeDirectory = "/Users/torben";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  home.packages =  with pkgs; [
    # default cli tools
    ripgrep
    eza
    fzf
    jq
    htop
    cmake
    imagemagick
    ffmpeg
  
    # ui applications
    # emacs # @todo emacs plus from homebrew for now, it seems bugged with yabai
    alacritty
    aerospace

    # fonts and themes
    nerd-fonts.hack
    nerd-fonts.ubuntu

    # work
    asdf-vm
    docker-compose
    colima
    docker
    redis
    mysql80
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # home.file = {
  #   ".emacs".source = "${dotfiles}/.emacs";
  #   ".zshrc".source = "${dotfiles}/.zshrc";
  # };

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}

  
  
