# 🏡 dotfiles

My text-based configuration files for Linux systems.

## 🔄 Sync dotfiles

Use the `bin/dotman` shell script to install and sync dotfiles to your system.
This script relies on [`stow`](https://www.gnu.org/software/stow/) to sync files.
`stow` creates symlinks that link the files in this repository to your system's home directory.

```shell
$ git clone --recurse-submodules https://github.com/claby2/dotfiles
$ cd dotfiles
$ ./bin/dotman stow
```

Simply run `dotman unstow` to remove the installed symlinks.

## 🔨 Install suckless programs

To install any suckless program, `just install DIRECTORY`.
For example:

```shell
$ just install .config/st
```

## 🔧 System installation

The `./install.sh` script can be used to bootstrap a working Arch-based system from **scratch**.

The script installs and configures necessary programs using the dotfiles.

```shell
$ sudo pacman -S curl # Install curl if it isn't already installed
$ curl -O "https://raw.githubusercontent.com/claby2/dotfiles/master/install.sh"
$ chmod +x install.sh
$ ./install.sh
```

> **Note**: the install script probably outdated :(

## 🔤 Font

- JetBrainsMono Nerd Font
