set nocompatible
filetype off

let mapleader=" "

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
Plug 'scrooloose/nerdcommenter'
Plug 'arcticicestudio/nord-vim'
Plug 'PeterRincker/vim-argumentative'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-surround'
" Plug 'lambdalisue/vim-gita', {'on': ['Gita']}
Plug 'jreybert/vimagit', {'on': ['Magit']}
Plug 'ruanyl/vim-gh-line'

" these require neovim nightly
if has('nvim-0.5')
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'neovim/nvim-lspconfig'
endif

call plug#end()

" rg
" if executable('rg')
"     set grepprg=rg\ --vimgrep\ --hidden\ —glob ‘!.git’
" endif

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

" telescope
if has('nvim-0.5')
  nnoremap <C-p> <cmd>Telescope find_files<cr>
  nnoremap <leader>ff <cmd>Telescope find_files<cr>
  nnoremap <leader>fg <cmd>Telescope live_grep<cr>
  nnoremap <leader>fb <cmd>Telescope buffers<cr>
  nnoremap <leader>fh <cmd>Telescope help_tags<cr>
endif

" vim-gh-line
let g:gh_use_canonical = 1

" colorscheme zellner
" let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
colorscheme nord
if has('gui_running')
    try
        set guifont=Operator\ Mono\ Book:h17
    catch
    endtry
endif

filetype plugin indent on

set encoding=utf-8

vnoremap // y/\V<C-R>=escape(@",'/\')<CR><CR>

nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

autocmd FileType ruby setlocal shiftwidth=2 tabstop=2
autocmd FileType python setlocal shiftwidth=4 tabstop=4
autocmd FileType html setlocal shiftwidth=4 tabstop=4

" emacs insert mode: old habits die hard
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

" NERDCommenter
let g:NERDDefaultAlign = 'left'
let g:NERDSpaceDelims = 1
