# dotfiles

text-based configuration files

| Program             | Name                                                                            |
|---------------------|---------------------------------------------------------------------------------|
| Window Manager      | [bspwm](https://github.com/baskerville/bspwm)                                   |
| Bar                 | [polybar](https://github.com/polybar/polybar)                                   |
| Program Launcher    | [rofi](https://github.com/davatorium/rofi)                                      |
| Wallpaper Setter    | [feh](https://github.com/derf/feh)                                              |
| Terminal Emulators  | [st](https://st.suckless.org/) and [kitty](https://github.com/kovidgoyal/kitty) |
| Document Viewer     | [zathura](https://pwmt.org/projects/zathura/)                                   |
| Code Editor         | [neovim](https://github.com/neovim/neovim)                                      |
| Notification Daemon | [dunst](https://github.com/dunst-project/dunst)                                 |
| Compositor          | [picom](https://github.com/jonaburg/picom)                                      |
| Hotkey Daemon       | [sxhkd](https://github.com/baskerville/sxhkd)                                   |
| Music Player        | [spotify-tui](https://github.com/Rigellute/spotify-tui)                         |
| Search Tool         | [ripgrep](https://github.com/BurntSushi/ripgrep)                                |
| File Displayer      | [bat](https://github.com/sharkdp/bat)                                           |
| Shell               | [zsh](https://wiki.archlinux.org/index.php/Zsh)                                 |
| Media Control       | [playerctl](https://github.com/altdesktop/playerctl)                            |

## Ubuntu Installation

### Programs

Majority of programs can be installed with apt:

    $ sudo apt install bspwm rofi feh zathura neovim dunst sxhkd ripgrep bat zsh playerctl

Simple Terminal (my st fork):

    $ git clone https://github.com/claby2/st
    $ cd st
    $ sudo make clean install

Other programs:

*   [polybar](https://github.com/polybar/polybar)
*   [kitty](https://github.com/kovidgoyal/kitty)
*   [picom](https://github.com/jonaburg/picom)
*   [spotify-tui](https://github.com/Rigellute/spotify-tui)

### Fonts

*   Cascadia Code
*   Inter

<!---->

    $ sudo apt install fonts-cascadia-code fonts-inter
