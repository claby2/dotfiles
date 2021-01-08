export ZSH="$HOME/.oh-my-zsh"

if [[ -n $SSH_CONNECTION ]]; then
    ZSH_THEME="jbergantine"
else
    ZSH_THEME="fwalch"
fi

plugins=(
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
