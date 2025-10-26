
{ config, pkgs, dotfiles, ... }:

{
  home.username = "henrik";
  home.homeDirectory = "/home/henrik";

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

  gtk = {
    enable = true;
    theme = {
      name = "Dracula";
      package = pkgs.dracula-theme;
    };
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
    };
    gtk3 = {
      extraConfig.gtk-application-prefer-dark-theme = true;
    };    
  };

  # some webapps
  xdg.desktopEntries.youtube = {
    name = "YouTube";
    exec = "brave --app=https://youtube.com";
    icon = "youtube";
  };
  xdg.desktopEntries.netflix = {
    name = "Netflix";
    exec = "brave --app=https://netflix.com";
    icon = "netflix";
  };
  xdg.desktopEntries.spotify = {
    name = "Spotify";
    exec = "brave --app=https://open.spotify.com/";
    icon = "spotify";
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
