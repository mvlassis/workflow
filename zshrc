# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

zstyle :compinstall filename '/home/manos/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit

eval "$(zoxide init zsh)" # Add zoxide

# Aliases
# Easier Navigation
alias d="cd ~/Documents"
alias dl="cd ~/Downloads"
alias gt="cd ~/Documents/Github"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# Shortcuts
# Finds unused packages (orhans) and removes them
alias clean='pacman -Qtdq | sudo pacman -Rns -'
alias duck='ddgr -j'
#alias emacs='emacs -nw'
alias emacs='emacsclient -t'
alias open='xdg-open'
alias update='sudo pacman -Syu'
alias wiki='ddgr -j \!w'
alias yeet='sudo pacman -Rns'

# ls shortcuts
alias ll='la -l'
alias ls='ls --color=auto'
alias la='ls -aA'

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
