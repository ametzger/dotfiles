" == General Config
set nocompatible                " Use VIM not vi
set number                      " Line numbers are good
set backspace=indent,eol,start  " Allow backspace in insert mode
set history=1000                " Store lots of :cmdline history
set showcmd                     " Show incomplete cmds down the bottom
set showmode                    " Show current mode down the bottom
set gcr=a:blinkon0              " Disable cursor blink
set mouse=a                     " Always enable mouse use
if has('gui_running')
  set guioptions-=T             " Disable toolbar in graphical vim
endif
set autoread                    " Reload files changed outside vim
set hidden                      " Hide buffers even if they are changed
set laststatus=2                " Always show status line

" Turn on syntax highlighting
syntax on

" Stupid shift key
command! Q q
command! W w
command! E e

" == Satusline
set statusline=%t[%{&ff}]%h%m%r%y%=%c,%l/%L\ %P

" == Pathogen Init
runtime bundle/tpope-vim-pathogen/autoload/pathogen.vim
call pathogen#infect()
call pathogen#helptags()

" == Search Settings
set incsearch        " Find the next match as we type the search
set viminfo='100,f1  " Save up to 100 marks, enable capital marks
set ignorecase       " Ignore search case
set smartcase        " Override ignorecase if search string contains uppercase

" == Turn Off Swap Files
set noswapfile
set nobackup
set nowb

" == Persistent Undo
" Keep undo history across sessions, by storing in file.
" Only works in MacVim (gui) mode.
if has('gui_macvim')
  set undodir=~/.vim/backups
  set undofile
endif

" == Indentation
set autoindent
set smartindent
" Smartindent hack to keep hash comments indented
inoremap # X<BS>#
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab

filetype plugin on
filetype indent on

" Display tabs and trailing spaces visually
set list listchars=tab:\ \ ,trail:·
set wrap                " Wrap lines

" == Folds
set foldmethod=indent   " Fold based on indent
set foldnestmax=3       " Deepest fold is 3 levels
set nofoldenable        " Dont fold by default

" == Completion
set wildmode=list:longest
set wildmenu                " Enable ctrl-n and ctrl-p to scroll thru matches
set wildignore=*.o,*.obj,*~ " Stuff to ignore when tab completing
set wildignore+=*vim/backups*

" == Scrolling
set scrolloff=8         "Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" == Colors
" tell the term has 256 colors
set t_Co=256
if has('gui_running')
  " If we're in a GUI, use dark Solarized theme
  colorscheme solarized
  set background=dark

  " Show tab number
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M
  autocmd VimEnter * set guitablabel=%N:\ %t\ %M

  " 90x60 by default
  set lines=60
  set columns=90

  " Use Dejavu Sans Mono on Linux, Monaco on Mac
  if has('gui_gtk2')
    set guifont=DejaVu\ Sans\ Mono\ 10
  elseif has('gui_macvim')
    set guifont=Monaco:h12
  endif
else
  " If we're just in a terminal, use gummybears
  colorscheme gummybears
endif

" == Weird File Syntax
" Clojure and Racket files are Lisp
au BufRead,BufNewFile *.clj set filetype=lisp
au BufRead,BufNewFile *.rkt set filetype=lisp
" Indent Python correctly
autocmd FileType python set softtabstop=4 shiftwidth=4

" == NERDTree Stuff
" Bind <leader>nt to NerdTree
map <leader>nt :NERDTree<CR>

" Close the whole window if only NERDTree is open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

