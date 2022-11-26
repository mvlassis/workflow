#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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
alias emacs='emacs -nw'
alias open='xdg-open'
alias update='sudo pacman -Syu'
alias wiki='ddgr -j \!w'
alias yeet='sudo pacman -Rns'

# ls shortcuts
alias ll='la -l'
alias ls='ls --color=auto'
alias la='ls -aA'
alias lsd="ls -lF | grep '^d'"
PS1="\[\e[1;33m\][\w] \[\e[m\]"

# Exports
export PATH="$HOME/.bin:$HOME/.local/bin:$PATH"
export CHROME_EXECUTABLE=chromium
export LANG=en_US.UTF-8
source $HOME/.bin/melee.sh-master/melee.sh

# Source RACECAR tool # RACECAR_ALIASES
if [ -f /home/manos/Documents/Github/racecar-mvlassis/scripts/.config ]; then # RACECAR_ALIASES
. /home/manos/Documents/Github/racecar-mvlassis/scripts/.config # RACECAR_ALIASES
fi # RACECAR_ALIASES
if [ -f /home/manos/Documents/Github/racecar-mvlassis/scripts/racecar_tool.sh ]; then # RACECAR_ALIASES
. /home/manos/Documents/Github/racecar-mvlassis/scripts/racecar_tool.sh # RACECAR_ALIASES
fi # RACECAR_ALIASES
