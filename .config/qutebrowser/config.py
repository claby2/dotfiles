config.load_autoconfig()

c.aliases = {
    'gr': 'greasemonkey-reload',
    'cs': 'config-source',
    'w': 'session-save',
    'q': 'quit',
    'wq': 'quit --save',
}


def bind_chained(key, *commands):
    config.bind(key, ' ;; '.join(commands))


bind_chained('cs', 'config-source', 'message-info "Loaded config.py"')
bind_chained('gr', 'greasemonkey-reload --quiet',
             'message-info "Loaded Greasemonkey scripts"')

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    'reddit': 'https://old.reddit.com/r/{}',
    'arch': 'https://wiki.archlinux.org/?search={}'
}

startpage = 'https://edwardwibowo.com'
c.url.default_page = startpage
c.url.start_pages = [startpage]

c.content.blocking.method = 'both'
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    'https://easylist-downloads.adblockplus.org/antiadblockfilters.txt'
]

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

config.source('color.py')
