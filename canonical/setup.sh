#! /usr/bin/env bash

sudo apt install libffi-dev -y
sudo apt install tox -y
sudo apt install gnome-keyring -y
sudo snap install charmcraft --classic
lxd init --auto
sudo snap install docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
