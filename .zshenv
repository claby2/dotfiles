ZDOTDIR=$HOME/.config/zsh

export GOPATH="$HOME/.go"
export GIT_SIGNINGKEY=$(git config --get user.emptykey)
export LESSHISTFILE=- # Disable less history file
export _JAVA_AWT_WM_NONREPARENTING=1
export MANWIDTH=80

export BROWSER="qutebrowser"
export TERM="st-256color"
export EDITOR="nvim"
export MANPAGER="nvim +Man!"
export TERMINAL="st"

path=(
    $HOME/.lean/bin
    $HOME/.local/bin
    $HOME/.cargo/bin
    $HOME/.yarn/bin
    $HOME/.config/dotfiles/.local/bin
    $GOPATH/bin
    $path
)

# Load host specific configuration
[ -f ~/.config/zsh/host.zsh ] && source ~/.config/zsh/host.zsh
