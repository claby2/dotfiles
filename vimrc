set nocompatible

" Plugins 
if has("win32") || has("win16")
    call plug#begin('C:\Users\wibow9770\vimfiles\plugged')
else
    call plug#begin('~/Users/wibow9770/.vim/plugged')
endif

Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'rhysd/vim-clang-format'
Plug 'airblade/vim-gitgutter'
Plug 'aserebryakov/vim-todo-lists'
Plug 'eemed/sitruuna.vim'
Plug 'itchyny/lightline.vim'

call plug#end()

set number tabstop=4 shiftwidth=4
set expandtab 

autocmd GUIEnter * simalt ~x
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

" Clear hlsearch highlights
nnoremap <silent> <esc><esc> : nohls<cr>

" Allow backspace in insert mode
set backspace=indent,eol,start

" Don't add empty newlines at the end of files
set noeol

" Disable automatic comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Clear using current background color
if has("win32") || has("win16")
    set t_ut=
endif

" Enable spell check 
set spell spelllang=en_us

" Disable the default vim startup message
set shortmess+=I

" Nerd tree bind
nnoremap <leader>nt :NERDTree<CR>

" Clang format bind
nnoremap <leader>cf :ClangFormat<CR>

" Always display status bar
set laststatus=2

" Set lightline theme to sitruuna
let g:lightline = {
            \ 'colorscheme': 'sitruuna',
            \ }

" Syntax related stuff
syntax on
colorscheme sitruuna 

" Highlight line numbers
highlight LineNr guifg=#FFFFFF
