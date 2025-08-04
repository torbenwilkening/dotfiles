# with darwin
export ZSH="$HOME/.oh-my-zsh"
# with nix
# export ZSH=$HOME/.nix-profile/share/oh-my-zsh

# great stuff: macovsky muse sonicradish arrow
ZSH_THEME="dracula"

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

alias ls="eza --icons"
alias ll="eza --icons -l"
alias la="eza --icons -la"

# with darwin
if [ "$(uname)" = "Darwin" ]; then
    alias nix-update="/run/current-system/sw/bin/darwin-rebuild switch"
fi

# asdf setup
if command -v asdf >/dev/null 2>&1; then
    export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
fi

