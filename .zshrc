# with darwin
export ZSH="$HOME/.oh-my-zsh"
# with nix
# export ZSH=$HOME/.nix-profile/share/oh-my-zsh

# great stuff: macovsky muse sonicradish arrow
ZSH_THEME="macovsky"

plugins=(
    docker
    git
    kubectl
    node
    python
    rust
    sudo
)

source $ZSH/oh-my-zsh.sh

ll="eza --icons -l"
la="eza --icons -la"

# with darwin
update="/run/current-system/sw/bin/darwin-rebuild switch"
