from qutebrowser.api import interceptor

config = config  # type: ignore
c = c  # type: ignore

config.load_autoconfig()

# https://github.com/qutebrowser/qutebrowser/issues/7489
c.qt.args = ["disable-accelerated-2d-canvas"]

c.aliases = {
    'gr': 'greasemonkey-reload',
    'cs': 'config-source',
    'w': 'session-save',
    'q': 'quit',
    'wq': 'quit --save',
}

config.bind('~', 'mode-leave', mode='passthrough')
config.bind('M', 'hint links spawn --detach mpv {hint-url}')


def bind_chained(key, *commands):
    config.bind(key, ' ;; '.join(commands))


bind_chained('cs', 'config-source', 'message-info "Loaded config.py"')
bind_chained('gr', 'greasemonkey-reload --quiet',
             'message-info "Loaded Greasemonkey scripts"')

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    'gmaps': 'https://maps.google.com/?q={}',
    'shellcheck': 'https://www.shellcheck.net/wiki/SC{}'
}

# https://github.com/qutebrowser/qutebrowser/issues/7118#issuecomment-1111951853
c.qt.workarounds.remove_service_workers = True

startpage = 'qute://start'
c.url.default_page = startpage
c.url.start_pages = [startpage]

c.content.blocking.method = 'both'
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    'https://easylist-downloads.adblockplus.org/antiadblockfilters.txt'
]

c.content.javascript.clipboard = "access"

c.content.user_stylesheets = ["~/.config/qutebrowser/stylesheet.css"]

c.statusbar.padding = {
    'bottom': 5,
    'left': 5,
    'top': 5,
    'right': 5,
}

c.tabs.padding = {
    'bottom': 10,
    'left': 5,
    'top': 10,
    'right': 5,
}

c.tabs.last_close = "close"

c.fonts.default_size = '12pt'

c.colors.webpage.preferred_color_scheme = 'dark'

c.downloads.position = 'bottom'

config.source('color.py')
