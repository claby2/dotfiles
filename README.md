# dotfiles

text-based configuration files

| Program             | Name                                                    |
| ------------------- | ------------------------------------------------------- |
| Window Manager      | [bspwm](https://github.com/baskerville/bspwm)           |
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

Use [`stow`](https://www.gnu.org/software/stow/) to install:

    $ git clone --recurse-submodules https://github.com/claby2/dotfiles
    $ cd dotfiles
    $ stow -v . -t ~

Run `stow -Dv . -t ~` to remove all symlinks.

## Font

- JetBrains Mono (nerd-font patch)
