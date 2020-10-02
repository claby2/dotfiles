#!/usr/bin/env python3
"""This script replaces the ripgrep config"""
import dotset

CONFIG = dotset.get_root() + "/config/ripgrep/config"


def main():
    """Copy ripgrep"""
    home = dotset.get_home()
    system_config = home + "/.config/ripgrep/config"
    dotset.print_diff(CONFIG, system_config)
    dotset.copy_file(CONFIG, system_config)


if __name__ == "__main__":
    main()
