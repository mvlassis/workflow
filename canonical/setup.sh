#! /usr/bin/env bash

sudo apt install python3-pip -y
sudo snap install charmcraft --classic
sudo snap install rockcraft --classic

pip install tox

lxd init --auto
adduser ubuntu lxd

sudo snap install docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
