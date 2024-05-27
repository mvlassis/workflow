#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

YELLOW=$'\e[1;33m'  # Bold yellow color
RESET=$'\e[0m'      # Reset to default terminal color
PROMPT="${YELLOW}[y/n]: ${RESET}"

# Clone and install ble.sh if it isn't already installed
blesh_folder1="/usr/share/blesh" 
blesh_folder2="$HOME/.local/share/blesh"
if [[ ! -d "${blesh_folder1}" && ! -d "${blesh_folder2}" ]]; then
    read -p  "ble.sh not detected! Do you want to install ble.sh in $HOME/.local/share/blesh? ${PROMPT}" -n 1 -r; echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	echo "Downloading ble.sh..."
	git clone --recursive https://github.com/akinomyoga/ble.sh.git
	cd ble.sh
	make install
	echo "ble.sh successfully installed, cleaning..."
	cd ..
	rm -rf ble.sh
    fi
else
    echo "ble.sh is already installed! Moving on..."
fi   


# Bash configuration file
read -p "Do you want to symlink .bashrc? This may delete your existing .bashrc file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm -f "${HOME}/.bashrc"
	ln -sv "${BASEDIR}/dotfiles/bashrc" ~/.bashrc
fi

# blesh configuration file
read -p "Do you want to symlink .blerc? This may delete your existing .blerc file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm -f "${HOME}/.blerc"
	ln -sv "${BASEDIR}/dotfiles/blerc" ~/.blerc
	ln -sv "${BASEDIR}/dotfiles/blerc2" ~/.blerc2
fi

# Dunst configuration file
read -p "Do you want to symlink dunstrc? This may delete your existing dunstrc file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	[[ -e "${HOME}/.config/dunstrc" ]] && rm "${HOME}/.config/dunstrc"
	ln -sv "${BASEDIR}/dotfiles/dunstrc" "${HOME}/.config/dunstrc"
fi

# Kitty configuration file
read -p "Do you want to symlink kitty.conf? This may delete your existing kitty.conf file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm -f "${HOME}/.config/kitty/kitty.conf"
	ln -sv "${BASEDIR}/dotfiles/kitty.conf" "${HOME}/.config/kitty/kitty.conf"
fi

# zsh configuration file
read -p "Do you want to symlink .zshrc? This may delete your existing .zshrc file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm -f "${HOME}/.zshrc"
	ln -sv "${BASEDIR}/dotfiles/zshrc" ~/.zshrc
fi

# Emacs configuration file
read -p "Do you want to symlink init.el? This may delete your existing init.el file ${PROMPT}" -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	[[ -e "${HOME}/.emacs.d/custom.el" ]] || touch "${HOME}/.emacs.d/custom.el"
	if [ -d "${HOME}/.emacs.d" ]; then
		ln -sv "${BASEDIR}/dotfiles/init.el" "${HOME}/.emacs.d/init.el"
	fi
fi

# i3 configuration file
read -p "Do you want to symlink i3's config? This will delete your existing config file ${PROMPT}" -n 1 -r
echo 
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [[ ! -d "${HOME}/.config/i3" ]]; then
		mkdir "${HOME}/.config/i3"
	fi
	[[ -e "${HOME}/.config/i3/config" ]] && rm "${HOME}/.config/i3/config"
	ln -sv "${BASEDIR}/i3/config" "${HOME}/.config/i3/config"
	ln -sv "${BASEDIR}/i3/i3format.py" "${HOME}/.config/i3/i3format.py"
	chmod +x "${BASEDIR}/i3/i3format.py"
	cp "${BASEDIR}/i3/i3lock-solarized.sh" "${HOME}/.config/i3/i3lock-solarized.sh"
fi

# Polybar configuration file
read -p "Do you want to symlink Polybar's config.ini? This may delete your existing config.ini file ${PROMPT}" -n 1 -r; echo;
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [[ ! -d "${HOME}/.config/polybar" ]]; then
		mkdir "${HOME}/.config/polybar"
	fi	
	ln -sv "${BASEDIR}/polybar/config.ini" "${HOME}/.config/polybar/config.ini"
	cp "${BASEDIR}/polybar/"*.sh "${HOME}/.config/polybar/"
	cp "${BASEDIR}/polybar/rofi-power-menu" "${HOME}/.config/polybar/"		
fi

# xprofile
read -p "Do you want to symlink .xprofile? This may delete your existing .xprofile file ${PROMPT}" -n 1 -r; echo;
if [[ $REPLY =~ ^[Yy]$ ]]; then
	[[ -e "${HOME}/.xprofile" ]] && rm "${HOME}/.xprofile"
	ln -sv "${BASEDIR}/dotfiles/xprofile" "${HOME}/.xprofile"
fi

# Micromamba
if [[ ! -d "${HOME}/.micromamba" ]]; then
	echo "~/.micromamba directory not found, creating..."
	mkdir "${HOME}/.micromamba"
fi
	
# Scripts

# First clone and symlink pokecat
if [[ ! -d "${BASEDIR}/pokecat" ]]; then
	git clone "https://github.com/gvlassis/pokecat.git"
fi

if [ ! -L "${HOME}/.bin/pokecat.sh" ]; then
	ln -sv "${BASEDIR}/pokecat/src/pokecat.sh" "${HOME}/.bin"
fi	

# Create directory ~/.bin if it doesn't exit, then place all scripts there
if [ ! -d "${HOME}/.bin" ]; then
	echo "~/.bin directory not found, creating..."
    mkdir ~/.bin
fi

# Move all scripts to ~/.bin
SCRIPTS="${BASEDIR}/bin/*"
for file in ${SCRIPTS}
do
    if [ ! -L "${HOME}/.bin/$(basename ${file})" ]; then
      	ln -sv "${file}" "${HOME}/.bin/$(basename ${file})"
    fi
done

read -p "Do you want to symlink the systemd units? ${PROMPT}" -n 1 -r; echo;
if [[ $REPLY =~ ^[Yy]$ ]]; then
	[[ -d "${HOME}/.config" ]] || mkdir "${HOME}/.config"
	[[ -d "${HOME}/.config/systemd" ]] || mkdir "${HOME}/.config/systemd"
	[[ -d "${HOME}/.config/systemd/user" ]] || mkdir "${HOME}/.config/systemd/user"
	SYSTEMD_UNITS="${BASEDIR}/systemd/*"
	for file in ${SYSTEMD_UNITS}
	do
		if [ ! -L "${HOME}/.config/systemd/user/$(basename ${file})" ]; then
      		ln -sv "${file}" "${HOME}/.config/systemd/user/$(basename ${file})"
		fi
	done
fi

