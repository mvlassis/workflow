#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Bash configuration file
read -p "Do you want to symlink .bashrc? This may delete your existing .bashrc file"
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ ! -L "${HOME}/.bashrc" ]; then
		rm "${HOME}/.bashrc"
		ln -sv "${BASEDIR}/dotfiles/bashrc" ~/.bashrc
	fi
fi

# zsh configuration file
read -p "Do you want to symlink .zshrc? This may delete your existing .zshrc file"
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ ! -L "${HOME}/.zshrc" ]; then
		rm "${HOME}/.zshrc"
		ln -sv "${BASEDIR}/dotfiles/zshrc" ~/.zshrc
	fi
fi

# Emacs configuration file
read -p "Do you want to symlink init.el? This may delete your existing init.el file"
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.emacs.d" ]; then
		if [ ! -L "${HOME}/.emacs.d/init.el" ]; then
			ln -sv "${BASEDIR}/dotfiles/init.el" "${HOME}/.emacs.d/init.el"
		fi    
	fi
fi

# i3 configuration file
read -p "Do you want to symlink i3's config? This will delete your existing config file"
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.config/i3" ]; then
		if [ ! -L "${HOME}/.config/i3/config" ]; then
			ln -sv "${BASEDIR}/i3/config" "${HOME}/.config/i3/config"
			cp "${BASEDIR}/i3/i3lock-solarized.sh" "${HOME}/.config/i3/i3lock-solarized.sh"
		fi    
	fi
fi

# Polybar configuration file
read -p "Do you want to symlink Polybar's config.ini? This may delete your existing config.ini file"
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.config/polybar" ]; then
		if [ ! -L "${HOME}/.config/polybar/config.ini" ]; then
			ln -sv "${BASEDIR}/polybar/config.ini" "${HOME}/.config/polybar/config.ini"
			cp "${BASEDIR}/polybar/"*.sh "${HOME}/.config/polybar/"
			cp "${BASEDIR}/polybar/rofi-power-menu" "${HOME}/.config/polybar/"		
		fi    
	fi
fi

# Scripts
# Create directory ~/.bin if it doesn't exit, then place all scripts there
if [ ! -d "${HOME}/.bin" ]; then
	echo "~/.bin directory not found, creating..."
    mkdir ~/.bin
fi

SCRIPTS="${BASEDIR}/bin/*"
for file in ${SCRIPTS}
do
    if [ ! -L "${HOME}/.bin/$(basename ${file})" ]; then
      	ln -sv "${file}" "${HOME}/.bin/$(basename ${file})"
    fi
done
