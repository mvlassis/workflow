#! /usr/bin/env bash

sudo apt install libffi-dev -y
sudo apt install tox -y
sudo apt install gnome-keyring -y
sudo snap install microk8s --classic
sudo snap install charmcraft --classic
sudo snap install rockcraft --classic
sudo snap install jhack

lxd init --auto
adduser ubuntu lxd

sudo snap install docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker

# Setup Juju
sudo snap install juju
juju bootstrap localhost lxd
juju bootstrap microk8s microk8s
