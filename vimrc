" ///// PLUGINS /////
if has("win32") || has("win16")
    let $PLUGGED = "~/vimfiles/plugged"
else
    let $PLUGGED = "~/.vim/plugged"
endif

call plug#begin($PLUGGED)
Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'rhysd/vim-clang-format'
Plug 'airblade/vim-gitgutter'
Plug 'aserebryakov/vim-todo-lists'
Plug 'eemed/sitruuna.vim'
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf'
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

" Clear using current background color
if has("win32") || has("win16")
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
nnoremap <leader>s :set spell!<CR>

" Nerd tree bind
nnoremap <leader>nt :NERDTreeToggle<CR>

" Clang format bind
nnoremap <leader>cf :ClangFormat<CR>

" fzf bind
nnoremap <leader>fz :FZF<CR>





" ///// PLUGIN SPECIFIC SETTINGS /////
" Set lightline theme to sitruuna 
let g:lightline = {
            \ 'colorscheme': 'sitruuna',
            \ }

" Customize fzf colors to match color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }





" ///// COLOR SETTINGS /////
syntax on
colorscheme sitruuna 

" Highlight line numbers
highlight LineNr guifg=#FFFFFF
