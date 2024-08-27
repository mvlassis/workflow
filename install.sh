#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

BOLD=$'\e[1m'       # Bold text
YELLOW=$'\e[1;33m'  # Bold yellow color
RESET=$'\e[0m'      # Reset to default terminal color
PROMPT="${YELLOW}[y/n]: ${RESET}"

# If ${AUTOMATE} is true, then return yes, otherwise prompt the user
send_prompt() {
	local prompt_message="$1"
	if [[ "${AUTOMATE}" = "true" ]]; then
		return 0
	else
		read -p "$prompt_message" -n 1 -r; echo
        if [[ "$REPLY" =~ ^[Yy]$ ]]; then
            return 0
        else
            return 1
        fi
	fi
}

# Install all prerequisite packages (requires sudo)
install_prerequisites() {
	if ! [[ $(command -v apt) ]]; then
        echo "apt command not found. Exiting."
        return 1
    fi
	
	sudo apt update
	while IFS= read -r package
	do
		if dpkg -s "$package" >/dev/null 2>&1; then
            echo "$package is already installed."
        else
		    if [[ "$package" = "emacs" ]]; then
                sudo snap install "$package" --classic || true
            else
                sudo apt install -y "$package" 2>/dev/null || true
            fi
		fi
	done < "${BASEDIR}/requirements.txt"
}

# Clone and install ble.sh if it isn't already installed
install_blesh() {
	blesh_folder1="/usr/share/blesh" 
	blesh_folder2="$HOME/.local/share/blesh"
	if [[ ! -d "${blesh_folder1}" && ! -d "${blesh_folder2}" ]]; then
		if send_prompt "ble.sh not detected! Do you want to install ble.sh in $HOME/.local/share/blesh? ${PROMPT}"; then
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
}

# Symlink the bash configuration file
symlink_bash() {
	if send_prompt "Do you want to symlink ${BOLD}.bashrc${RESET}? This may delete your existing .bashrc file ${PROMPT}"; then
		rm -f "${HOME}/.bashrc"
		ln -sv "${BASEDIR}/dotfiles/bashrc" ~/.bashrc
	fi
}

# Symlink the blesh configuration file
symlink_blesh() {
	if send_prompt "Do you want to symlink ${BOLD}.blerc${RESET}? This may delete your existing .blerc file ${PROMPT}"; then
		rm -f "${HOME}/.blerc"
		rm -f "${HOME}/.blerc2"
		ln -sv "${BASEDIR}/dotfiles/blerc" "${HOME}/.blerc"
		ln -sv "${BASEDIR}/dotfiles/blerc2" "${HOME}/.blerc2"
	fi	
}

# Symlink the zsh configuration file
symlink_zsh() {
	if send_prompt "Do you want to symlink ${BOLD}.zshrc${RESET}? This may delete your existing .zshrc file ${PROMPT}"; then
		rm -f "${HOME}/.zshrc"
		ln -sv "${BASEDIR}/dotfiles/zshrc" ~/.zshrc
	fi
}

# Symlink the Emacs configuration file
symlink_emacs() {
	if send_prompt "Do you want to symlink ${BOLD}init.el${RESET}? This may delete your existing init.el file ${PROMPT}"; then
		if [[ ! -d "${HOME}/.emacs.d" ]]; then
			mkdir "${HOME}/.emacs.d"
		fi
		rm -f "${HOME}/.emacs.d/init.el"
		ln -sv "${BASEDIR}/dotfiles/init.el" "${HOME}/.emacs.d/init.el"
	fi
}

# Symlink the kitty configuration file
symlink_kitty() {
	if send_prompt "Do you want to symlink ${BOLD}kitty.conf${RESET}? This may delete your existing kitty.conf file ${PROMPT}"; then
		rm -f "${HOME}/.config/kitty/kitty.conf"
		ln -sv "${BASEDIR}/dotfiles/kitty.conf" "${HOME}/.config/kitty/kitty.conf"
	fi	
}

# Symlink the i3 configuration file
symlink_i3() {
	if send_prompt "Do you want to symlink ${BOLD}i3 config${RESET}? This will delete your existing config file ${PROMPT}"; then
		if [[ ! -d "${HOME}/.config/i3" ]]; then
			mkdir "${HOME}/.config/i3"
		fi
		[[ -e "${HOME}/.config/i3/config" ]] && rm "${HOME}/.config/i3/config"
		ln -sv "${BASEDIR}/i3/config" "${HOME}/.config/i3/config"
		ln -sv "${BASEDIR}/i3/i3format.py" "${HOME}/.config/i3/i3format.py"
		chmod +x "${BASEDIR}/i3/i3format.py"
		cp "${BASEDIR}/i3/i3lock-solarized.sh" "${HOME}/.config/i3/i3lock-solarized.sh"
	fi	
}

# Symlink the polybar configuration file
symlink_polybar() {
	if send_prompt "Do you want to symlink ${BOLD}Polybar's config.ini${RESET}? This may delete your existing config.ini file ${PROMPT}"; then
		if [[ ! -d "${HOME}/.config/polybar" ]]; then
			mkdir "${HOME}/.config/polybar"
		fi	
		ln -sv "${BASEDIR}/polybar/config.ini" "${HOME}/.config/polybar/config.ini"
		cp "${BASEDIR}/polybar/"*.sh "${HOME}/.config/polybar/"
		cp "${BASEDIR}/polybar/rofi-power-menu" "${HOME}/.config/polybar/"		
	fi
}

# Symlink the dunst configuration file
symlink_dunst() {
	if send_prompt "Do you want to symlink ${BOLD}dunstrc${RESET}? This may delete your existing dunstrc file ${PROMPT}"; then
		[[ -d "${HOME}/.config/dunst" ]] || mkdir "${HOME}/.config/dunst"
		rm -f "${HOME}/.config/dunstrc"
		ln -sv "${BASEDIR}/dotfiles/dunstrc" "${HOME}/.config/dunst/dunstrc"
	fi
}

# Symlink the .xprofile file
symlink_xprofile() {
	if send_prompt "Do you want to symlink ${BOLD}.xprofile${RESET}? This may delete your existing .xprofile file ${PROMPT}"; then
		[[ -e "${HOME}/.xprofile" ]] && rm "${HOME}/.xprofile"
		ln -sv "${BASEDIR}/dotfiles/xprofile" "${HOME}/.xprofile"
	fi
}

# Symlink the .Xresources file
symlink_xresources() {
	if send_prompt "Do you want to symlink ${BOLD}.Xresources${RESET}? This may delete your existing .Xresources file ${PROMPT}"; then
		[[ -e "${HOME}/.Xresources" ]] && rm "${HOME}/.Xresources"
		ln -sv "${BASEDIR}/dotfiles/Xresources" "${HOME}/.Xresources"
	fi
}

# Symlink the scripts
symlink_scripts() {
	# Scripts
	# Create directory ~/.bin if it doesn't exit, then place all scripts there
	if [[ ! -d "${HOME}/.bin" ]]; then
		echo "~/.bin directory not found, creating..."
		mkdir "${HOME}/.bin"
	fi
	# Clone and symlink pokecat
	if [[ ! -d "${BASEDIR}/pokecat" ]]; then
		git clone "https://github.com/gvlassis/pokecat.git"
	fi
	if [ ! -L "${HOME}/.bin/pokecat.sh" ]; then
		ln -sv "${BASEDIR}/pokecat/src/pokecat.sh" "${HOME}/.bin/pokecat.sh"
	fi	
	# Move all scripts to ~/.bin
	SCRIPTS="${BASEDIR}/bin/*"
	for file in ${SCRIPTS}
	do
		if [ ! -L "${HOME}/.bin/$(basename ${file})" ]; then
      		ln -sv "${file}" "${HOME}/.bin/$(basename ${file})"
		fi
	done	
}

# Symlink the systemd units
symlink_systemd() {
	if send_prompt "Do you want to symlink the ${BOLD}systemd units${RESET}? ${PROMPT}"; then
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
		systemctl --user daemon-reload
		# systemctl --user enable --now emacs.service &
	fi
} 

# Install micromamba
install_micromamba() {
	if [[ ! -d "${HOME}/.micromamba" ]]; then
		echo "~/.micromamba directory not found, creating..."
		mkdir "${HOME}/.micromamba"
	fi
}

install_vm_profile() {
	install_prerequisites
	install_blesh
	symlink_bash
	symlink_blesh
	symlink_emacs
	symlink_scripts
	symlink_systemd
}


install_full_profile() {
	install_prerequisites
	install_blesh
	symlink_bash
	symlink_blesh
	symlink_zsh
	symlink_emacs
	symlink_kitty
	symlink_i3
	symlink_polybar
	symlink_dunst
	symlink_xprofile
	symlink_xresources
	symlink_scripts
	symlink_systemd
	install_micromamba
}

PROFILE="$1"
AUTOMATE=false
case "$PROFILE" in
    vm)
        AUTOMATE=true
        install_vm_profile
        ;;
    full)
        AUTOMATE=true
        install_full_profile
        ;;
    *)
        install_full_profile
        ;;
esac
