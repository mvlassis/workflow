#!/usr/bin/env bash

pipx install charmcraftcache

sudo snap install --edge jhack
sudo snap connect jhack:dot-local-share-juju snapd
sudo snap connect jhack:ssh-read snapd

sudo snap install terraform --classic
sudo snap install concierge --classic

sudo apt-get remove -y docker-ce docker-ce-cli containerd.io
sudo rm -rf /run/containerd

sudo concierge prepare -c "$(dirname "${BASH_SOURCE[0]}")/concierge.yaml"
juju add-model kubeflow
