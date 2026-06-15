# with darwin
export ZSH="$HOME/.oh-my-zsh"
# with nix
# export ZSH=$HOME/.nix-profile/share/oh-my-zsh

# great stuff: macovsky muse sonicradish arrow
ZSH_THEME="muse"

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

# direnv setup
if command -v direnv >/dev/null 2>&1; then
    eval "$(direnv hook zsh)"    
fi

# yarn global packages setup
if command -v yarn >/dev/null 2>&1; then
    export PATH="$HOME/.yarn/bin:$PATH"
fi

if command -v cargo >/dev/null 2>&1; then
    . "$HOME/.cargo/env"
fi

#### ------------------------------

#### exa - Color Scheme Definitions

#### ------------------------------

export EZA_COLORS="\
uu=36;\
gu=37;\
sn=32;\
sb=32;\
da=34;\
ur=34;\
uw=35;\
ux=36;\
ue=36;\
gr=34;\
gw=35;\
gx=36;\
tr=34;\
tw=35;\
tx=36;"

