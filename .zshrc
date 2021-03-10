export ZSH="$HOME/.oh-my-zsh"

bindkey '^[[Z' reverse-menu-complete

if [[ -n $SSH_CONNECTION ]]; then
    ZSH_THEME="nebirhos"
else
    ZSH_THEME="fwalch"
fi

plugins=(
    archlinux
    git
    golang
    yarn
    cabal
    colored-man-pages
    cargo
    alias-finder
)

ZSH_ALIAS_FINDER_AUTOMATIC=true

source $ZSH/oh-my-zsh.sh

cd $HOME
