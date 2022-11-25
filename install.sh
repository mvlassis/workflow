#! /usr/bin/env bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# bash
ln -sv ${BASEDIR}/.bashrc ~/.bashrc

# scripts
# Create directory ~/.bin if it doesn't exit, then place all scripts there
if [ ! -d "~/.bin" ]; then
    mkdir ~/.bin
fi

SCRIPTS="${BASEDIR}/bin/*"
for file in ${SCRIPTS}
do
    ln -sv "${file}" "${HOME}/.bin/$(basename ${file})"
done
