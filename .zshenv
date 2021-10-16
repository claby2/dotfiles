ZDOTDIR=$HOME/.config/zsh

path=(
	$HOME/.cargo/bin
	$HOME/.yarn/bin
	$HOME/go/bin
	$HOME/bin
	$path
)

export GIT_SIGNINGKEY=$(git config --get user.emptykey)
export LESSHISTFILE=- # Disable less history file
export _JAVA_AWT_WM_NONREPARENTING=1
export GOPATH="$HOME/.go"
export MANWIDTH=80

export BROWSER="firefox"
export TERM="st-256color"
export EDITOR="nvim"
export MANPAGER="nvim +Man!"
export TERMINAL="st"
