set nocompatible

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
" required!
Plugin 'gmarik/vundle'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/syntastic'
Plugin 'plasticboy/vim-markdown'
Plugin 'vim-ruby/vim-ruby'
Plugin 'avakhov/vim-yaml'
Plugin 'tpope/vim-rails.git' 
Plugin 'tpope/vim-bundler.git' 
Plugin 'Lokaltog/powerline'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'cakebaker/scss-syntax.vim'

call vundle#end()

syntax on
set number
filetype plugin indent on
set t_Co=256
set textwidth=80  " lines longer than 80 columns will be broken
set shiftwidth=4  " operation >> indents 4 columns; << unindents 4 columns
set tabstop=4     " a hard TAB displays as 4 columns
set expandtab     " insert spaces when hitting TABs
set softtabstop=4 " insert/delete 4 spaces when hitting a TAB/BACKSPACE
set shiftround    " round indent to multiple of 'shiftwidth'
set autoindent    " align the new line indent with the previous line

" search config
set incsearch
set ignorecase
set smartcase
set hlsearch
nmap \q :nohlsearch<CR>

autocmd FileType ruby setlocal sw=2 sts=2 et
autocmd FileType yaml setlocal sw=2 sts=2 et
autocmd FileType scss setlocal sw=2 sts=2 et
autocmd FileType html setlocal sw=2 sts=2 et
autocmd FileType erb setlocal sw=2 sts=2 et

" trim white space
autocmd FileType rb,scss,py,erb,html,css autocmd BufWritePre <buffer> :%s/\s\+$//e

let g:ycm_autoclose_preview_window_after_completion = 1

let g:molokai_original = 1
colorscheme molokai

" rubocop
let g:syntastic_ruby_checkers = ['rubocop']

" paste toggle
set pastetoggle=<F2>

" powerline
set laststatus=2
set encoding=utf-8
let g:Powerline_symbols = 'fancy'

" color column 80
set cc=80
