#! /usr/bin/env bash

setup_canonical_dev() {
	sudo apt install python3-pip -y
	sudo apt install python3-venv -y
	sudo snap install charmcraft --classic
	sudo snap install rockcraft --channel=latest/candidate --classic
	sudo snap install docker
	sudo snap install yq
	sudo snap install kustomize
	
	sudo groupadd docker
	sudo usermod -aG docker $USER
	newgrp docker
}


