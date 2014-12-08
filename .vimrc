set nocompatible               " be iMproved
filetype off                   " required!

set mouse=a

set shell=/bin/bash
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/Vundle.vim'
Bundle 'jnwhiteh/vim-golang'
Bundle 'jnurmine/Zenburn'

" All of your Plugins must be added before the following line
call vundle#end()             " required
filetype plugin indent on     " required!

" colorscheme zenburn

"syntax highlighting
syntax on

set tabstop=2

set ignorecase
set smartcase
set incsearch

let mapleader = ","

inoremap jj <esc>
