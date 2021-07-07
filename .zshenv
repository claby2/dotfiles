ZDOTDIR=$HOME/.config/zsh

export PATH="$HOME/.config/scripts":$PATH
export PATH="$HOME/.cargo/bin":$PATH
export PATH="$HOME/.yarn/bin":$PATH
export PATH="$HOME/go/bin":$PATH

export GIT_SIGNINGKEY=$(git config --get user.emptykey)
export LESSHISTFILE=- # Disable less history file
export _JAVA_AWT_WM_NONREPARENTING=1
export RUST_LOG=error

export BROWSER="firefox"
export TERM="st-256color"
export EDITOR="nvim"
