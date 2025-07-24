{ config, pkgs, ... }:

{
  home.username = "torben";
  home.homeDirectory = "/home/torben";

  home.packages = with pkgs; [

  ];
  
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

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "25.11";
  
  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
