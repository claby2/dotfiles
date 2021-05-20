" ///// PLUGINS /////
call plug#begin("~/.vim/plugged")
Plug 'airblade/vim-gitgutter'
Plug 'cespare/vim-toml'
Plug 'claby2/genfmt.vim'
Plug 'itchyny/lightline.vim'
Plug 'preservim/nerdcommenter'
Plug 'preservim/nerdtree'
Plug 'sheerun/vim-polyglot'
Plug 'terryma/vim-smooth-scroll'
Plug 'fatih/vim-go'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'delphinus/vim-firestore'
Plug 'lervag/vimtex'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'rafi/awesome-vim-colorschemes'
call plug#end()





" ///// GENERAL SETTINGS /////
set nocompatible
set number relativenumber tabstop=4 shiftwidth=4
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

" Mouse support
set mouse=a

" Clear using current background color
set t_ut=

" augroup
augroup stuff
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c setlocal filetype=c
    " Recognize compile flags, mostly for coc.nvim
    autocmd FileType cpp,c let b:coc_root_patterns=['compile_flags.txt']
augroup END

" Make vim always open windows maximized
autocmd GUIEnter * simalt ~x

" Disable automatic comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

if has('nvim')
    set inccommand=nosplit
endif





" ///// MAPPING COMMANDS /////
" Clear hlsearch highlights
nnoremap <silent> <esc><esc> :nohls<CR>

" Toggle spellchecking
nnoremap <silent> <leader>s :set spell!<CR>

" Redraw screen
nnoremap <silent> <leader>r :redraw!<CR>:CocRestart<CR>

" Nerd tree bind
nnoremap <silent> <leader>t :NERDTreeToggle<CR>

" genfmt.vim bind
nnoremap <silent> <leader>f :GenfmtFormat<CR>

" fzf bind
nnoremap <silent> <leader><tab> :Files<CR>

" Switch between window splits easily
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <C-H> <C-W>h





" ///// PLUGIN SPECIFIC SETTINGS /////
" Lightline configuration
function! HasSpell()
    " Check if spell is on
    if &spell
        return 'spell'
    endif
    return ''
endfunction
let g:lightline = {
            \ 'colorscheme': 'jellybeans',
            \ 'active': {
            \   'left': [ [ 'mode' ],
            \             [ 'readonly', 'filename', 'modified', 'cocstatus', 'spell'] ],
            \   'right': [ [ 'lineinfo' ],
            \              [ 'fileformat', 'filetype' ] ]
            \ },
            \ 'component_function': {
            \   'spell': 'HasSpell',
            \   'cocstatus': 'coc#status',
            \ },
            \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
            \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
            \ }

" genfmt.vim configuration
function! ClangFormat()
    if len(findfile(".clang-format", expand("%:p:h").";")) || len(findfile("_clang-format", expand("%:p:h").";"))
        " clang-format file has been found
        return "clang-format --assume-filename=".expand('%:t')." -style=file"
    endif
    " No clang-format file has been found
    return "clang-format --assume-filename=".expand('%:t')." --style=\"{BasedOnStyle: Google, IndentWidth: 4}\""
endfunction
function! CmakeFormat()
    return "cmake-format ".expand('%:t')
endfunction
function! Prettier()
    return "prettier --stdin-filepath ".expand('%:p')
endfunction
let g:genfmt_formatters = {
            \ 'python': "yapf",
            \ 'cpp': "ClangFormat()",
            \ 'java': "ClangFormat()",
            \ 'javascript': "Prettier()",
            \ 'typescriptreact': "Prettier()",
            \ 'typescript': "Prettier()",
            \ 'rust': "rustfmt --edition 2018",
            \ 'haskell': "stylish-haskell",
            \ 'cmake': "CmakeFormat()",
            \ 'markdown': "remark --no-color --silent",
            \ 'gdscript3': "gdformat -",
            \ 'go': "gofmt",
            \ 'lua': "lua-format",
            \ 'sh': "shfmt",
            \ 'zsh': "shfmt",
            \ }
let g:genfmt_enable_fallback = 1

" Smooth scroll
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 0, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 0, 2)<CR>
noremap <silent> <c-b> :call smooth_scroll#up(&scroll*2, 0, 4)<CR>
noremap <silent> <c-f> :call smooth_scroll#down(&scroll*2, 0, 4)<CR>

" Nerd Commenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" vimtex Configuration
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=1
let g:tex_conceal='abdmg'

" polyglot Configuration
let g:vim_json_syntax_conceal=0
let g:vim_markdown_conceal=0
let g:vim_markdown_conceal_code_blocks=0

" vim-go Configuration
" Disable warning on startup
let g:go_version_warning = 0





" ///// COC.NVIM CONFIGURATION /////
" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
    " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
            \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
    else
        execute '!' . &keywordprg . " " . expand('<cword>')
    endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

let g:c_syntax_for_h = 1
let g:coc_disable_startup_warning = 1




" ///// COLOR SETTINGS /////
syntax on
colorscheme jellybeans
hi CocErrorSign ctermfg=Red guifg=#882222
hi Quote ctermbg=109 guifg=#111111
