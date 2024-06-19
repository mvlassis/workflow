#!/usr/bin/env bash

# Ensure the script is running as root
if [ "$(id -u)" -ne 0 ]; then
    echo "This script must be run as root"
    exit 1
fi

# Install and setup Microk8s
sudo apt update
sudo snap install microk8s --classic --channel=1.26/stable
sudo usermod -a -G microk8s $USER
newgrp microk8s
sudo chown -f -R $USER ~/.kube

# Enable some Microk8s addons
microk8s enable dns hostpath-storage ingress metallb:10.64.140.43-10.64.140.49 rbac

# Install and setup Juju
sudo snap install juju --classic --channel=3.1/stable
mkdir -p ~/.local/share
microk8s config | juju add-k8s my-k8s --client
juju bootstrap my-k8s uk8sx
juju add-model kubeflow

# Deploy Charmed Kubeflow
sudo sysctl fs.inotify.max_user_instances=1280
sudo sysctl fs.inotify.max_user_watches=655360
juju deploy kubeflow --trust  --channel=1.8/stable
