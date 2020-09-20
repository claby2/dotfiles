#!/usr/bin/env python3
"""This script replaces the current vimrc to the vimrc in the same directory of this script"""
from os.path import expanduser
import platform
import shutil

def main():
    """Copy vimrc"""
    home = expanduser("~")
    if platform.system() == "Windows":
        shutil.copyfile("vimrc", home + "/_vimrc")
    elif platform.system() == "Darwin" or platform.system() == "Linux":
        shutil.copyfile("vimrc", home + "/.vimrc")

if __name__ == "__main__":
    main()
