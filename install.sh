#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# bash configuration file
if [ ! -L "${HOME}/.bashrc" ]; then
    rm "${HOME}/.bashrc"
    ln -sv "${BASEDIR}/bashrc" ~/.bashrc
fi

# zsh configuration file
if [ ! -L "${HOME}/.zshrc" ]; then
    rm "${HOME}/.zshrc"
    ln -sv "${BASEDIR}/zshrc" ~/.zshrc
fi    

# emacs configuration file
if [ -d "${HOME}/.emacs.d" ]; then
    if [ ! -L "${HOME}/.emacs.d/init.el" ]; then
	ln -sv "${BASEDIR}/init.el" "${HOME}/.emacs.d/init.el"
    fi    
fi

# scripts
# Create directory ~/.bin if it doesn't exit, then place all scripts there
if [ ! -d "${HOME}/.bin" ]; then
    mkdir ~/.bin
fi

SCRIPTS="${BASEDIR}/bin/*"
for file in ${SCRIPTS}
do
    if [ ! -L "${HOME}/.bin/$(basename ${file})" ]; then
      	ln -sv "${file}" "${HOME}/.bin/$(basename ${file})"
    fi
done
