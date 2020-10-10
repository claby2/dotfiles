" ///// CROSS-PLATFORM /////
let s:windows = has('win32') || has('win16')





" ///// PLUGINS /////
if s:windows
    let $PLUGGED = "~/vimfiles/plugged"
else
    let $PLUGGED = "~/.vim/plugged"
endif

call plug#begin($PLUGGED)
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'airblade/vim-gitgutter'
Plug 'aserebryakov/vim-todo-lists'
Plug 'itchyny/lightline.vim'
Plug 'cespare/vim-toml'
Plug 'claby2/genfmt.vim'
Plug 'eemed/sitruuna.vim'
Plug 'jaredgorski/spacecamp'
call plug#end()





" ///// GENERAL SETTINGS /////
set nocompatible
set number tabstop=4 shiftwidth=4
set expandtab
set termguicolors
filetype plugin indent on

" Remove annoying sound on errors
set belloff=all
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" A buffer becomes hidden when it is abandoned
set hidden

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" When searching try to be smart about cases
set smartcase

" Allow backspace in insert mode
set backspace=indent,eol,start

" Don't add empty newlines at the end of files
set noeol

" Splits open at the bottom and right
set splitbelow splitright

" Set spell language
set spelllang=en_us

" Disable the default vim startup message
set shortmess+=I

" Always display status bar
set laststatus=2

" No context lines before scroll
set scrolloff=0

" Clear using current background color
if s:windows
    set t_ut=
endif

" Make vim always open windows maximized
autocmd GUIEnter * simalt ~x

" Disable automatic comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o





" ///// MAPPING COMMANDS /////
" Clear hlsearch highlights
nnoremap <silent> <esc><esc> : nohls<cr>

" Toggle spellchecking
nnoremap <silent> <leader>s :set spell!<CR>

" Nerd tree bind
nnoremap <silent> <leader>nt :NERDTreeToggle<CR>

" genfmt.vim bind
nnoremap <silent> <leader>gf :GenfmtFormat<CR>





" ///// PLUGIN SPECIFIC SETTINGS /////
" Lightline configuration
let g:lightline = {
            \ 'colorscheme': 'sitruuna',
            \ 'active': {
            \   'left': [ [ 'mode' ],
            \             [ 'readonly', 'filename', 'modified', 'spell'] ],
            \   'right': [ [ 'lineinfo' ],
            \              [ 'fileformat', 'filetype' ] ]
            \ },
            \ 'component_function': {
            \   'spell': 'HasSpell',
            \ },
            \ }

" Check if spell is on
function! HasSpell()
    if &spell
        return 'spell'
    endif
    return ''
endfunction

" genfmt.vim configuration
function! ClangFormat()
    if len(findfile(".clang-format", expand("%:p:h").";")) || len(findfile("_clang-format", expand("%:p:h").";"))
        " clang-format file has been found
        return "clang-format"
    endif
    " No clang-format file has been found
    return "clang-format --style=\"{BasedOnStyle: Google, IndentWidth: 4}\""
endfunction
let g:genfmt_formatters = {
            \ 'python': "yapf",
            \ 'cpp': ClangFormat(),
            \ }
let g:genfmt_enable_fallback = 1





" ///// COLOR SETTINGS /////
syntax on
colorscheme spacecamp

" Highlight line numbers
highlight LineNr guifg=#FFFFFF
