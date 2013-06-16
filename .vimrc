set nocompatible               " be iMproved
filetype off                   " required!

set mouse=a

set shell=/bin/bash
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'
Bundle 'jnwhiteh/vim-golang'
Bundle 'jnurmine/Zenburn'

filetype plugin indent on     " required!

colorscheme zenburn

"syntax highlighting
syntax on

set tabstop=2

set ignorecase
set smartcase
set incsearch

let mapleader = ","

inoremap jj <esc>
