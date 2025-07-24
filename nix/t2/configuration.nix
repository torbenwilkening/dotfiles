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
    }))  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot";
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

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

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  programs.nh = {
    enable = true;
    clean.enable = true;
    clean.extraArgs = "--keep-since 4d --keep 3";
    flake = "/home/torben/Projects/dotfiles/nix/t2"; # sets NH_OS_FLAKE variable for you
  };
  
  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  programs.regreet = {
    enable = true;
    # default config:
    # https://github.com/rharish101/ReGreet/blob/main/regreet.sample.toml
    settings = lib.mkForce {
      default_session = {
        command = "Hyperland";
      };
      background = {
        path = "/etc/background.png"; # make sure it exists
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
      GTK = {
        theme_name = "Dracula";
        font_name = "Hack 16";
        icon_theme_name = "Papirus-Dark";
        cursor_theme_name = "Bibata-Modern-Ice";
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.torben = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
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
    emacs # do not forget to add an editor to edit configuration.nix!
    nil # nix LSP server
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

    # default gui tools
    alacritty
    firefox-devedition
    nautilus
    nwg-look
    dracula-theme
    bibata-cursors
    papirus-icon-theme
    
    # hyprland tools
    waybar # @todo theme
    waypaper
    hyprpaper
    mako # notifications @todo theme
    libnotify
    syspower # power menu, @todo use wleave
    networkmanagerapplet # network ui
    hyprpolkitagent # permissions
    anyrun # launcher
    hypridle # idle
    hyprlock # lockscreen
    udiskie # auto mount
    
  ];
  
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };
  
  programs.dconf.enable = true;

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    zsh-autoenv.enable = true;

    shellAliases = {
      nix-update = "nixos-rebuild switch --flake /etc/nixos --sudo";
    }; 
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "steam"
    "steam-original"
    "steam-unwrapped"
    "steam-run"
  ];
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

  
  # List services that you want to enable:

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

