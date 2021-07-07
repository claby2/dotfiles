# Prompt
autoload -U colors && colors
setopt prompt_subst
function git_prompt_info() {
	function __git_prompt_git() {
		GIT_OPTIONAL_LOCKS=0 command git "$@"
	}
	if ! __git_prompt_git rev-parse --git-dir &>/dev/null; then
		return 0
	fi
	local ref
	ref=$(__git_prompt_git symbolic-ref --short HEAD 2>/dev/null) ||
		ref=$(__git_prompt_git rev-parse --short HEAD 2>/dev/null) ||
		return 0
	local dirty
	[[ -n $(__git_prompt_git status --porcelain 2>/dev/null | tail -1) ]] && dirty=" ✗"
	echo " %{$fg[yellow]%}(%{$fg[red]%}${ref}%{$fg[yellow]%})$fg[red]%}${dirty}"
}
autoload -Uz git_prompt_info
function precmd() {
	git_prompt_info >/dev/null
}
PROMPT=" %B"
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	PROMPT="${PROMPT}%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[red]%}] "
fi
PROMPT="${PROMPT}%{$fg[cyan]%}%1~$(git_prompt_info)%{$reset_color%}%b "

# Options
setopt autocd   # Enter directory name to cd
stty stop undef # Disable ctrl+s freeze
setopt interactive_comments

# History
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=$HOME/.config/zsh/history

# Completions
fpath=($HOME/.config/scripts/completions $fpath)
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
bindkey '^[[Z' reverse-menu-complete

# Vim keys
export KEYTIMEOUT=1
bindkey -v
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -v '^?' backward-delete-char

# Aliases
[ -x "$(command -v nvim)" ] && alias vim="nvim"
# Color aliases
alias \
	ls="ls -hN --color=always --group-directories-first" \
	grep="grep --color=always" \
	diff="diff --color=always"
# Abbreviation aliases
alias \
	cp="cp -iv" \
	mv="mv -iv" \
	rm="rm -vI" \
	clippy="cargo clippy --all-targets --all-features" \
	clip="xclip -selection clipboard" \
	grip="grip --pass=$GIT_SIGNINGKEY" \
	dots="cd $HOME/.config/ambit/repo"
