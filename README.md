# dotfiles

text-based configuration files

## Installation

### Sync dotfiles

Use [`just`](https://github.com/casey/just) to install the dotfiles with ease.
Installation requires [`stow`](https://www.gnu.org/software/stow/).

```shell
git clone --recurse-submodules https://github.com/claby2/dotfiles
cd dotfiles
just stow
```

`just unstow` to remove all symlinks.

### Build suckless Programs

To install any suckless program, `just install DIRECTORY`.
For example:

```shell
just install .config/st
```

## System Installation

The `./install.sh` script can be used to bootstrap a working Arch-based system from **scratch**.

The script installs and configures necessary programs using the dotfiles.

```shell
sudo pacman -S curl # Install curl if it isn't already installed
curl -O "https://raw.githubusercontent.com/claby2/dotfiles/master/install.sh"
chmod +x install.sh
./install.sh
```

## Font

- JetBrains Mono (nerd-font patch)
