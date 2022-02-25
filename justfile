verbose := "1"
scripts := "bin/"

_default:
    @just --unsorted --list

# install dotfiles
stow:
    stow --verbose {{verbose}} . -t ~

# remove installed dotfiles
unstow:
    stow --verbose {{verbose}} -D . -t ~

# install given suckless program
install DIRECTORY:
    cd {{DIRECTORY}} && {{justfile_directory()}}/{{scripts}}/slinstall {{DIRECTORY}}

# uninstall given suckless program
uninstall DIRECTORY:
    sudo make -C {{DIRECTORY}} clean uninstall

# run shellcheck on scripts in bin/
shellcheck:
    shellcheck `find {{scripts}} -maxdepth 1 -type f`

# update and compile neovim packer plugins
packer:
    nvim --headless -c "autocmd User PackerComplete quitall" -c "PackerSync"
