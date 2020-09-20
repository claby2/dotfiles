#!/usr/bin/env python3
"""Utility functions for dotfile setting"""
import os
import difflib
import platform
import shutil


def get_home():
    """Get home directory"""
    return os.path.expanduser("~")


def get_root():
    """Get dotfiles root"""
    return os.path.dirname(os.path.realpath(__file__)) + "/.."


def get_system():
    """Return system platform"""
    return platform.system()


def copy_file(file1, file2):
    """Copy contents of file1 to file2"""
    shutil.copyfile(file1, file2)


def print_diff(text1, text2):
    """Print diff between two text files"""
    file_1 = open(text1).readlines()
    file_2 = open(text2).readlines()
    for line in difflib.unified_diff(file_2, file_1):
        print(line)
