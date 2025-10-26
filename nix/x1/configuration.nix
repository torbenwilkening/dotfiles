# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.loader.grub.useOSProber = true;

  
  networking.hostName = "x1"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "de";
#    useXkbConfig = true; # use xkb.options in tty.
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;


  # rtkit (optional, recommended) allows Pipewire to use the realtime scheduler for increased performance.
  security.rtkit.enable = true;

  # Enable sound.
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
  };
  
  hardware.bluetooth = {
    enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd Hyprland";
        user = "greeter";
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users = {
    torben = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    };
    henrik = {
      isNormalUser = true;
      shell = pkgs.zsh;
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    };
  };

  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    zsh-autoenv.enable = true;
    
    shellAliases = {
      ll = "eza --icons -l";
      la = "eza --icons -la";
      # update = "nixos-rebuild switch --use-remote-sudo";
      nix-update = "nixos-rebuild switch --flake /etc/dotfiles/nix/x1 --use-remote-sudo";
    };
  };

  fonts.packages = with pkgs; [
    nerd-fonts.hack
    nerd-fonts.ubuntu
    nerd-fonts.fira-code
    nerd-fonts.roboto-mono
    nerd-fonts.jetbrains-mono
  ];
  
  environment.systemPackages = with pkgs; [

    # default tools
    emacs
    nil
    git
    wget
    curl
    ripgrep
    jq
    eza
    htop
    dnsutils
    gnupg
    unzip
    fzf
    ffmpeg
    
    # compiling
    cmake
    gcc
    gnumake
    libtool


    # default gui tools
    alacritty
    firefox
    brave
    nautilus
    nwg-look
    dracula-theme
    bibata-cursors
    papirus-icon-theme
    pavucontrol
    blueberry
    
    # hyprland tools
    waybar # @todo theme
    waypaper
    hyprpaper
    mako # notifications
    libnotify
    networkmanagerapplet # network ui
    hyprpolkitagent # permissions
    # anyrun # launcher
    walker # launcher
    hypridle # idle
    hyprlock # lockscreen
    wleave # logout menu
    udiskie # auto mount
    swww # wallpaper backend for images
    nautilus # gtk file manager
    networkmanagerapplet # gtk network manager frontend
    hyprpolkitagent # for permissions
    nwg-look # for gtk theming    
    
  ];

  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  programs.dconf.enable = true;

  # hints electron apps to use wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1"; 

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
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
  system.stateVersion = "24.11"; # Did you read the comment?

}

