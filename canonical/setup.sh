#!/usr/bin/env bash

sudo snap install concierge --classic

sudo apt-get remove -y docker-ce docker-ce-cli containerd.io
sudo rm -rf /run/containerd

sudo concierge prepare -c "$(dirname "${BASH_SOURCE[0]}")/concierge.yaml"

