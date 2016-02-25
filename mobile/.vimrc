set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'

Plugin 'tpope/vim-surround'
Plugin 'vim-scripts/paredit.vim'
Plugin 'kien/rainbow_parentheses.vim'

Plugin 'guns/vim-clojure-static'
Plugin 'JuliaLang/julia-vim'

call vundle#end()            " required
filetype plugin indent on    " required

" common
syntax on
set cindent
set incsearch
set hlsearch
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

" key bindings
imap jj <Esc>
map <space> <leader>
map <leader>e :Explore<CR>

" paredit
au FileType hs call PareditInitBuffer()
au FileType jl call PareditInitBuffer()

" rainbow_parentheses
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"nerdtree
map <C-n> :NERDTreeToggle<CR>

"syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
