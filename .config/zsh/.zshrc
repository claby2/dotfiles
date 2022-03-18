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
	echo " %{$fg[yellow]%}git:(%{$fg[red]%}${ref}%{$fg[yellow]%})%{$fg[red]%}${dirty}"
}
autoload -Uz git_prompt_info
function precmd() {
	git_prompt_info >/dev/null
}
PROMPT=" %B%(?:%{$fg_bold[green]%}➜ :%{$fg_bold[red]%}➜ ) "
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
	# Add username indicator if ssh
	PROMPT+="%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[red]%}] "
fi
PROMPT+='%{$fg[cyan]%}%1~$(git_prompt_info)%{$reset_color%}%b '

# Options
setopt autocd   # Enter directory name to cd
stty stop undef # Disable ctrl+s freeze
setopt interactive_comments

# History
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE=$HOME/.config/zsh/history

# Completions
fpath=($HOME/bin/completions $fpath)
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)
bindkey '^[[Z' reverse-menu-complete

# vim keys
export KEYTIMEOUT=1
bindkey -v
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -v '^?' backward-delete-char

# History search
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^[[A" history-beginning-search-backward-end
bindkey "^[[B" history-beginning-search-forward-end

# Aliases
[ -x "$(command -v nvim)" ] && alias vim="nvim"
# Color aliases
alias \
	grep="grep --color=always" \
	diff="diff --color=always"
# Abbreviation aliases
alias \
	cp="cp -iv" \
	mv="mv -iv" \
	ip="ip --color=always" \
	cip="cargo install --path" \
	clippy="cargo clippy --all-targets --all-features" \
	clip="xclip -selection clipboard" \
	grip="grip --pass=$GIT_SIGNINGKEY" \
	dots="cd $HOME/.config/dotfiles" \
	music="ncmpcpp -q"
# OS-specific aliases
os=$(uname -s)
case $os in
Darwin*)
	alias \
		rm="rm -v" \
		ls="ls -hG"
	;;
Linux*)
	alias \
		rm="rm -vI" \
		ls="ls -hN --color=always --group-directories-first"
	;;
esac

# Colored man pages
export LESS_TERMCAP_mb="${fg_bold[red]}"               # begin blink
export LESS_TERMCAP_md="${fg_bold[cyan]}"              # begin bold
export LESS_TERMCAP_me="${reset_color}"                # reset bold/blink
export LESS_TERMCAP_so="${fg_bold[yellow]}${bg[blue]}" # begin reverse video
export LESS_TERMCAP_se="${reset_color}"                # reset reverse video
export LESS_TERMCAP_us="${fg_bold[green]}"             # begin underline
export LESS_TERMCAP_ue="${reset_color}"                # reset underline

# Edit current line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Syntax highlighting
typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]='fg=blue'
ZSH_HIGHLIGHT_STYLES[path_prefix]=none
# zsh-syntax-highlighting location is dependent on os
case $os in
Darwin*)
	source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
	;;
Linux*)
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null ||
		source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
	;;
esac
