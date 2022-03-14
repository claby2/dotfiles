# dotfiles

text-based configuration files

| Program             | Name                                                   |
| ------------------- | ------------------------------------------------------ |
| Window Manager      | [bspwm](https://github.com/baskerville/bspwm)          |
| Status Bar          | [polybar](https://github.com/polybar/polybar)          |
| Hotkey Daemon       | [sxhkd](https://github.com/baskerville/sxhkd)          |
| Program Launcher    | [dmenu](https://tools.suckless.org/dmenu/)             |
| Wallpaper Setter    | [xwallpaper](https://github.com/stoeckmann/xwallpaper) |
| Terminal Emulator   | [st](https://github.com/claby2/st)                     |
| Document Viewer     | [zathura](https://pwmt.org/projects/zathura/)          |
| Code Editor         | [neovim](https://github.com/neovim/neovim)             |
| Notification Daemon | [dunst](https://github.com/dunst-project/dunst)        |
| Compositor          | [picom](https://github.com/yshui/picom)                |
| Media Player        | [mpv](https://github.com/mpv-player/mpv)               |
| Music Player Daemon | [mpd](https://musicpd.org/)                            |
| Music Player Client | [ncmpcpp](https://github.com/ncmpcpp/ncmpcpp)          |
| Search Tool         | [ripgrep](https://github.com/BurntSushi/ripgrep)       |
| File Displayer      | [bat](https://github.com/sharkdp/bat)                  |
| Shell               | [zsh](https://wiki.archlinux.org/index.php/Zsh)        |
| Browser             | [qutebrowser](https://qutebrowser.org/)                |

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
