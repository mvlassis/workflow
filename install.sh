#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Clone and install ble.sh if it isn't already installed
blesh_folder1="/usr/share/blesh" 
blesh_folder2="$HOME/.local/share/blesh"
if [[ ! -d "${blesh_folder1}" && ! -d "${blesh_folder2}" ]]; then
    read -p  "ble.sh not detected! Do you want to install ble.sh in $HOME/.local/share/blesh? [y/n]: " -n 1 -r; echo
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
read -p "Do you want to symlink .bashrc? This may delete your existing .bashrc file [y/n]: " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm "${HOME}/.bashrc"
	ln -sv "${BASEDIR}/dotfiles/bashrc" ~/.bashrc
fi

# blesh configuration file
read -p "Do you want to symlink .blerc? This may delete your existing .blerc file [y/n]: " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm "${HOME}/.blerc"
	ln -sv "${BASEDIR}/dotfiles/blerc" ~/.blerc
fi

# zsh configuration file
read -p "Do you want to symlink .zshrc? This may delete your existing .zshrc file [y/n]: " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	rm "${HOME}/.zshrc"
	ln -sv "${BASEDIR}/dotfiles/zshrc" ~/.zshrc
fi

# Emacs configuration file
read -p "Do you want to symlink init.el? This may delete your existing init.el file [y/n]: " -n 1 -r; echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.emacs.d" ]; then
		ln -sv "${BASEDIR}/dotfiles/init.el" "${HOME}/.emacs.d/init.el"
	fi
fi

# i3 configuration file
read -p "Do you want to symlink i3's config? This will delete your existing config file [y/n]: " -n 1 -r
echo 
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.config/i3" ]; then
		ln -sv "${BASEDIR}/i3/config" "${HOME}/.config/i3/config"
		ln -sv "${BASEDIR}/i3/i3format.py" "${HOME}/.config/i3/i3format.py"
		chmod +x "${BASEDIR}/i3/i3format.py"
		cp "${BASEDIR}/i3/i3lock-solarized.sh" "${HOME}/.config/i3/i3lock-solarized.sh"
	fi
fi

# Polybar configuration file
read -p "Do you want to symlink Polybar's config.ini? This may delete your existing config.ini file [y/n]: " -n 1 -r; echo;
if [[ $REPLY =~ ^[Yy]$ ]]; then
	if [ -d "${HOME}/.config/polybar" ]; then
		ln -sv "${BASEDIR}/polybar/config.ini" "${HOME}/.config/polybar/config.ini"
		cp "${BASEDIR}/polybar/"*.sh "${HOME}/.config/polybar/"
		cp "${BASEDIR}/polybar/rofi-power-menu" "${HOME}/.config/polybar/"		
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

SYSTEMD_UNITS="${BASEDIR}/systemd/*"
for file in ${SYSTEMD_UNITS}
do
    if [ ! -L "${HOME}/.config/systemd/user/$(basename ${file})" ]; then
      	ln -sv "${file}" "${HOME}/.config/systemd/user/$(basename ${file})"
    fi
done
