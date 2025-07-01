{ config, pkgs, dotfiles, ... }:

{
  home.username = "torben";
  home.homeDirectory = "/home/torben";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    # things with ui
    emacs
    nerd-fonts.hack
    nerd-fonts.ubuntu
    firefox-wayland

    nil # nix lsp

    # nodejs global things and lsp
    nodejs_22
    yarn
    pnpm
    eslint
    vue-language-server
    typescript
    typescript-language-server
  ];

  # basic configuration of git, please change to your own
  programs.git = {
    enable = true;
    userName = "torbenwilkening";
    userEmail = "no-reply@github.com";
  };

  programs.alacritty = {
    enable = true;
    # custom settings
    settings = {
      env.TERM = "xterm-256color";
      window = {
        padding = {
          x = 12;
          y = 12;
        };
        # opacity done globally
        # opacity = 0.9;
      };
      colors = {
        primary.background = "#1e1e2e";
      };
      font = {
        size = 10;
      };
      selection.save_to_clipboard = true;      
    };
  };

  home.file = {

    ".emacs".source = "${dotfiles}/.emacs";

  };
  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
