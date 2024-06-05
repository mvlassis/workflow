#! /usr/bin/env bash

sudo snap install snapcraft --classic
sudo snap install docker
sudo groupadd docker
sudo usermod -aG docker $USER
newgrp docker
