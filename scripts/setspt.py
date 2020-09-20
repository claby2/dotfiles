#!/usr/bin/env python3
"""This script replaces the spotify-tui config.yml"""
from os.path import expanduser
import shutil

def main():
    """Copy spotify-tui"""
    home = expanduser("~")
    shutil.copyfile("config/spotify-tui/config.yml", home + "/.config/spotify-tui/config.yml")

if __name__ == "__main__":
    main()
