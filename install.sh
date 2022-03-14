#!/bin/sh

welcome="Welcome!
This script will install the necessary programs and dotfiles to boostrap a working Arch-based system.
Please proceed with caution...
"

printf "%s\n" "$welcome"

printf "Proceed? [Y/n] "
read -r yn
if [ "$yn" != "Y" ]; then
	exit 1
fi

installpkg() {
	sudo pacman --noconfirm --needed -S "$@"
}

installaurpkg() {
	paru --noconfirm --needed -S "$@"
}

# Start in the home directory
cd "$HOME" || exit 1
sudo pacman --noconfirm -Syu

# Install paru
installpkg base-devel git rustup
rustup update stable
git clone https://aur.archlinux.org/paru.git
cd paru || exit 1
makepkg --noconfirm -si
cd "$HOME" || exit 1
rm -rf paru

# Sync dotfiles
installpkg stow just
mkdir "$HOME/.config" && cd "$HOME/.config" || exit 1
git clone --recurse-submodules https://github.com/claby2/dotfiles.git
cd dotfiles || exit 1
just stow

# Install terminal
installpkg libxft
just install .config/st

# Install programs
installpkg bspwm sxhkd dmenu xwallpaper zathura neovim dunst libnotify picom mpv mpd ncmpcpp ripgrep bat zsh zsh-syntax-highlighting qutebrowser python-adblock
installaurpkg polybar

# Install other required packages
installpkg alsa-utils pipewire pipewire-alsa pipewire-media-session pipewire-pulse bluez bluez-utils
installaurpkg nvim-packer-git

# Add kernel module for bluetooth
sudo modprobe btusb

# Install font
installaurpkg nerd-fonts-jetbrains-mono

# Change default shell to zsh
chsh -s /usr/bin/zsh
export SHELL=zsh

# Install X-related packages
installpkg xorg-xsetroot xorg-xinput xorg-xrandr xorg-server xorg-xinit
touch "$HOME"/.config/Xresources-theme

exec zsh
