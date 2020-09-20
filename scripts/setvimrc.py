#!/usr/bin/env python3
"""This script replaces the current vimrc to the vimrc in the same directory of this script"""
import dotset

VIMRC = dotset.get_root() + "/vimrc"


def main():
    """Copy vimrc"""
    home = dotset.get_home()
    system = dotset.get_system()
    if system == "Windows":
        system_vimrc = home + "/_vimrc"
    elif system in ("Darwin", "Linux"):
        system_vimrc = home + "/.vimrc"
    dotset.print_diff(VIMRC, system_vimrc)
    dotset.copy_file(VIMRC, system_vimrc)


if __name__ == "__main__":
    main()
