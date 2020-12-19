"""This script helps install dotfiles"""
import configparser
import sys
import os
import pathlib
import errno


def lint(config):
    """Parses the config if any options do not have required options"""
    good = True
    for section in config:
        if section != 'DEFAULT':
            missing_options = []
            if not config.has_option(section, 'source'):
                missing_options.append('source')
            if not config.has_option(section, 'destination'):
                missing_options.append('destination')
            if len(missing_options) > 0:
                good = False
                print(section + ' is missing options: ' +
                      ', '.join(map(str, missing_options)))
    return good


def main():
    """Symlink files"""
    platform = sys.platform
    home = str(pathlib.Path.home())
    dotfiles = os.path.dirname(os.path.realpath(__file__))
    config = configparser.ConfigParser()
    config.read_file(open('./ambit.conf'))
    if lint(config):
        for section in config:
            if section != 'DEFAULT' and (
                    not config.has_option(section, 'os') or platform in (''.join(
                        config[section]['os'].split())).split(',')):
                src = dotfiles + '/' + config[section]['source']
                destination = home + '/' + config[section]['destination']
                if not os.path.isfile(src):
                    print('ERROR: source of ' + section + ' does not exist')
                elif os.path.isfile(destination):
                    print('destination of ' + section + ' already exists')
                else:
                    if not os.path.exists(os.path.dirname(destination)):
                        try:
                            os.makedirs(os.path.dirname(destination))
                        except OSError as exc:
                            if exc.errno != errno.EEXIST:
                                raise
                    print('symlink ' + src + ' -> ' + destination)
                    os.symlink(src, destination)
    else:
        print("Too many lint errors...")


if __name__ == '__main__':
    main()
