# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # t2 linux wifi/bluetooth firmware. @todo move this to file
  hardware.firmware = [
    (pkgs.stdenvNoCC.mkDerivation (final: {
      name = "brcm-firmware";
      src = ./firmware/brcm;
      # dontUnpack = true;
      installPhase = ''
	mkdir -p $out/lib/firmware/brcm
	cp "${final.src}"/* "$out/lib/firmware/brcm"
      '';
    }))

#    (pkgs.runCommand "brcm-firmware" { } ''
#	mkdir -p $out/lib/firmware/
#	cp -r  ${./firmware/brcm} "$out"/lib/firmware/
#    '')

  ];

 # imports =
 #   [
 #     ./hardware-configuration.nix
 #   ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;


  

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  programs.regreet = {
    enable = true;
    # default config : https://github.com/rharish101/ReGreet/blob/main/regreet.sample.toml
    settings = {
      default_session = {
        command = "Hyperland";
      };
      background = {
        path = "/etc/background.png";
        fit = "Fill";
      };
      widget.clock = {
        format = "%H:%M";
        timeZone = "Europe/Berlin";
      };
      env = {
        GTK_USE_PORTAL = "0";
        GDK_DEBUG = "no-portals";
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.torben = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
    packages = with pkgs; [
    ];
  };

  fonts.packages = with pkgs; [
    nerd-fonts.hack
    nerd-fonts.ubuntu
    nerd-fonts.fira-code
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
  ];
  
  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    # default tools
    emacs # Do not forget to add an editor to edit configuration.nix!
    git
    wget
    curl
    ripgrep
    jq
    eza
    htop
    gnupg
    unzip

    # compiling
    cmake
    gcc
    gnumake
    libtool

    # gui tools
    alacritty
    brave
    firefox-devedition
    # kdePackages.dolphin # file manager @todo theme
    nautilus
    nwg-look
    dracula-theme
    bibata-cursors
    papirus-icon-theme
    papirus-folders
    
    # hyprland tools
    waybar # @todo theme
    waypaper
    hyprpaper
    mako # notifications @todo theme
    libnotify
    syshud # osd @todo theme
    syspower # power menu @todo theme
    networkmanagerapplet # network ui
    hyprpolkitagent # permissions
    anyrun # launcher @todo theme
    hypridle # idle
    hyprlock # @todo theme
    
  ];
  
  # gtk = {
  #   enable = true;
  #   theme = {
  #     name = "Dracula";
  #     package = pkgs.dracula-theme;
  #   };
  #   iconTheme = {
  #     name = "Papirus";
  #     package = pkgs.papirus-icon-theme;
  #   };
  #   gtk3 = {
  #     extraConfig.gtk-application-prefer-dark-theme = true;
  #   };
    
  # };
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    zsh-autoenv.enable = true;

    shellAliases = {
      #ll = "eza --icons -l";
      #la = "eza --icons -la";
      #l = "eza --icons";
      nix-update = "nixos-rebuild switch --flake /etc/nixos --sudo";
    }; 
  };

  # GUI
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.11"; # Did you read the comment?

}

