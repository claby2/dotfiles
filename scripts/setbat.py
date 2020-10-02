#!/usr/bin/env python3
"""This script replaces the bat config"""
import dotset

CONFIG = dotset.get_root() + "/config/bat/bat.conf"


def main():
    """Copy bat"""
    home = dotset.get_home()
    system_config = home + "/.config/bat/bat.conf"
    dotset.print_diff(CONFIG, system_config)
    dotset.copy_file(CONFIG, system_config)


if __name__ == "__main__":
    main()
