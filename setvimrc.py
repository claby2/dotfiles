#!/usr/bin/env python3
"""This script replaces the current vimrc to the vimrc in the same directory of this script"""
import platform
import shutil

def main():
    """Copy vimrc"""
    if platform.system() == "Windows":
        shutil.copyfile("vimrc", "C:/Users/wibow9770/_vimrc")
    elif platform.system() == "Darwin":
        shutil.copyfile("vimrc", "~/.vimrc")

if __name__ == "__main__":
    main()
