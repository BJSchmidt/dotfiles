---
author: Ben Schmidt
email: benschmidt@benschmidt.tech
title: New Machine Setup
---

# Create SSH directory if it doesn\'t exist already

``` shell
mkdir ~/.ssh
```

# Copy SSH Keys to new machine

``` shell
scp username@OtherMachineAddress:/mnt/user/username/Documents/.ssh/* ~/.ssh/
```

# Fix SSH permissions

``` shell
chmod 700 ~/.ssh
chmod 644 ~/.ssh/authorized_keys
chmod 644 ~/.ssh/known_hosts
chmod 644 ~/.ssh/config
chmod 600 ~/.ssh/id_rsa
chmod 644 ~/.ssh/*.pub
```

# Clone dotfiles repo to \~/repos/dotfiles

``` shell
git clone git@github.com:BenSchmidtTech/dotfiles.git ~/repos/dotfiles/
```

# Create dotfiles symlinks

Needs to be updated to check if running under WSL, and if so grab the correct profile name.

``` shell
. ~/repos/dotfiles/create_symlinks.sh
```

# Remove old emacs version

If you have an older Emacs version you will need to purge it before proceeding:

## Purge Emacs

``` shell
sudo apt --purge remove emacs
sudo apt autoremove
```

# Install Emacs

Check that emacs version 27+ is available in your distro\'s package repos:

``` shell
sudo apt update
apt-cache policy emacs
```

If that isn\'t Emacs27 or newer, then install it from Kevin Kelly\'s PPA:

## Installing Emacs27 from Kevin Kelley PPA

``` shell
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt install emacs27
```

### If you get an error about emacs27~common~ during the install process:

``` shell
Errors were encountered while processing:
 /tmp/apt-dpkg-install-RVK8CA/064-emacs27-common_27.1~1.git86d8d76aa3-kk2+20.04_all.deb
```

run

``` shell
sudo apt --purge remove emacs-common
sudo apt --fix-broken install
```

# Install other required packages for my config

``` shell
# Org Roam requires sqlite3.
sudo apt install sqlite3
# VTerm requirements:
sudo apt install cmake
sudo apt install libtool-bin
```

# Install Doom Emacs

``` shell
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

# Doom Sync - Configure Doom

``` shell
doom sync
```

# Run emacs

``` shell
emacs &
```

# Build Org Roam Cache

\<spc\> n r f
