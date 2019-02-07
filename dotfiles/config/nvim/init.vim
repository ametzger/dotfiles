set nocompatible
filetype off

let mapleader=","

set tabstop=2
set shiftwidth=2

set mouse=a
" set cursorline                  " Highlight the current line
set lazyredraw                  " Faster scrolling
set number relativenumber       " Show line number
set showcmd                     " Show current command
set showmode                    " Show current mode
set wildmode=longest:list,full  " Autocomplete
set wildignore=*.o,*.obj,*~     " Ignore file
set showmatch                   " highlight matching braces
set ignorecase                  " ignore case while searching
set smartcase                   " unless uppercase explicitly mentioned
set smartindent                 " indent smartly
set nowrap                      " Don't wrap text
set laststatus=2                " Always show statusbar
set scrolloff=5                 " Minimum space on bottom/top of window
set sidescrolloff=7             " Minimum space on side
set sidescroll=1
set expandtab                   " Spaces > tabs
set nofoldenable                " Disable folding
set clipboard+=unnamed          " Use system clipboard
set nobackup
set ruler

set undodir=~/.vim-undo
set undofile
set undolevels=1000  "max number of changes that can be undone
set undoreload=10000 "max number lines to save for undo on buffer reload

set shortmess+=A

set splitbelow
set splitright

set incsearch
set hlsearch
set inccommand=nosplit
"noremap - ddp
"noremap _ ddkP

call plug#begin('~/.vim/plugged')

Plug 'haya14busa/incsearch.vim'
"Plug 'vim-scripts/indentpython.vim'
"Plug 'kien/ctrlp.vim'
"Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'
Plug 'chr4/nginx.vim'
" Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'arcticicestudio/nord-vim'
Plug 'PeterRincker/vim-argumentative'
Plug '/usr/local/opt/fzf'

call plug#end()

" incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
set hlsearch
let g:incsearch#auto_nohlsearch = 1
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)

nnoremap <C-p> :FZF<Cr>

set background=dark
if has('gui_running')
    try
        colorscheme nord
    catch
    endtry

    try
        set guifont=Operator\ Mono\ Book:h17
    catch
    endtry
else
    colorscheme nord
endif

filetype plugin indent on

set encoding=utf-8

try
    let NERDTreeIgnore=['\.pyc$', '\~$']
    map <C-n> :NERDTreeToggle<CR>
    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
catch
endtry

" autoreload vimrc
if has ('autocmd') " Remain compatible with earlier versions
 augroup vimrc     " Source vim configuration upon save
    autocmd! BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd! BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
    autocmd! BufWritePost vimrc source $MYVIMRC | echom "Reloaded " . $MYVIMRC | redraw
  augroup END
endif " has autocmd

" The Silver Searcher
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2
autocmd FileType python setlocal shiftwidth=4 tabstop=4

" insert mode
imap <C-b> <Left>
imap <C-f> <Right>
imap <C-a> <Home>
imap <C-e> <End>
imap <C-d> <Del>
imap <C-h> <BS>
imap <C-w> <Esc>ddi
imap <C-_> <Esc>ui
imap <M-d> <Esc>dwi
imap <M-b> <Esc>bi
imap <M-f> <Esc>wi
inoremap <C-k> l<Esc>d$a

" command line mode
cmap <C-p> <Up>
cmap <C-n> <Down>
cmap <C-b> <Left>
cmap <C-f> <Right>
cmap <C-a> <Home>
cmap <C-e> <End>
cnoremap <C-d> <Del>
cnoremap <C-h> <BS>
cnoremap <C-k> <C-f>D<C-c><C-c>:<Up>

" command-T window
let g:CommandTCursorLeftMap  = ['<Left>',  '<C-b>']
let g:CommandTCursorRightMap = ['<Right>', '<C-f>']
let g:CommandTBackspaceMap   = ['<BS>',    '<C-h>']
let g:CommandTDeleteMap      = ['<Del>',   '<C-d>']

