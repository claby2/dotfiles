#!/usr/bin/env python3
"""This script replaces the spotify-tui config.yml"""
import dotset

CONFIG = dotset.get_root() + "/config/spotify-tui/config.yml"


def main():
    """Copy spotify-tui"""
    home = dotset.get_home()
    system_config = home + "/.config/spotify-tui/config.yml"
    dotset.print_diff(CONFIG, system_config)
    dotset.copy_file(CONFIG, system_config)


if __name__ == "__main__":
    main()
