from qutebrowser.api import interceptor

config = config  # type: ignore
c = c  # type: ignore

config.load_autoconfig()

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

startpage = 'https://edwardwibowo.com'
c.url.default_page = startpage
c.url.start_pages = [startpage]

c.content.blocking.method = 'both'
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    'https://easylist-downloads.adblockplus.org/antiadblockfilters.txt'
]

c.content.javascript.can_access_clipboard = True

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


def redirect(request: interceptor.Request):
    rewrite_map = {
        "www.reddit.com": "old.reddit.com",
    }

    for original_link, new_link in rewrite_map.items():
        if request.request_url.host() == original_link:
            request.request_url.setHost(new_link)
            try:
                request.redirect(request.request_url)
            except:
                pass


interceptor.register(redirect)

config.source('color.py')
