# 🏡 dotfiles

My text-based configuration files for Unix-like systems.

## 🔄 Sync dotfiles

Use the `.local/bin/dotman` shell script to install and sync dotfiles to your system.
This script relies on [`stow`](https://www.gnu.org/software/stow/) to sync files.
`stow` creates symlinks that link the files in this repository to your system's home directory.

```shell
$ git clone --recurse-submodules https://github.com/claby2/dotfiles
$ cd dotfiles
$ ./.local/bin/dotman stow
```

Simply run `dotman unstow` to remove the installed symlinks.

