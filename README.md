# dotfiles

text-based configuration files

| Program             | Name                                                    |
| ------------------- | ------------------------------------------------------- |
| Window Manager      | [bspwm](https://github.com/baskerville/bspwm)           |
| Status Bar          | [polybar](https://github.com/polybar/polybar)           |
| Hotkey Daemon       | [sxhkd](https://github.com/baskerville/sxhkd)           |
| Program Launcher    | [dmenu](https://tools.suckless.org/dmenu/)              |
| Wallpaper Setter    | [xwallpaper](https://github.com/stoeckmann/xwallpaper)  |
| Terminal Emulator   | [st](https://github.com/claby2/st)                      |
| Document Viewer     | [zathura](https://pwmt.org/projects/zathura/)           |
| Code Editor         | [neovim](https://github.com/neovim/neovim)              |
| Notification Daemon | [dunst](https://github.com/dunst-project/dunst)         |
| Compositor          | [picom](https://github.com/yshui/picom)                 |
| Music Player        | [spotify-tui](https://github.com/Rigellute/spotify-tui) |
| Search Tool         | [ripgrep](https://github.com/BurntSushi/ripgrep)        |
| File Displayer      | [bat](https://github.com/sharkdp/bat)                   |
| Shell               | [zsh](https://wiki.archlinux.org/index.php/Zsh)         |
| Browser             | [qutebrowser](https://qutebrowser.org/)                 |

## Dotfile Installation

### Install dotfiles

Use [`just`](https://github.com/casey/just) to install the dotfiles with ease.
Installation requires [`stow`](https://www.gnu.org/software/stow/).

    $ git clone --recurse-submodules https://github.com/claby2/dotfiles
    $ cd dotfiles
    $ just stow

`just unstow` to remove all symlinks.

### Install suckless Programs

To install any suckless program, `just install DIRECTORY`.
For example:

    $ just install .config/st

## Font

- JetBrains Mono (nerd-font patch)
