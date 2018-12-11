j"
" Key Bindings
"

:imap jj <Esc>

"
" Tabs
"
set expandtab

"
" Visual
"
set scrolloff=10
set linebreak
set laststatus=2
set noshowmode

"
" Filetypes and syntax highlighting
"
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType markdown setlocal conceallevel=0
autocmd FileType markdown let g:indentLine_enabled=0

let g:markdown_fenced_languages = ['html', 'python', 'bash=sh', 'yaml']

"
" Built in Editor settings
"
" set number
highlight Comment cterm=italic

"
" Plugin settings
"
let g:indentLine_enabled = 1
let g:indentLine_char = '▏'
let g:ansible_unindent_after_newline = 1

"
" Plugins
"
call plug#begin()
Plug 'scrooloose/nerdtree'
Plug 'w0rp/ale'
Plug 'yggdroot/indentline'
Plug 'pearofducks/ansible-vim'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/goyo.vim'
Plug 'chmp/mdnav'
Plug 'itchyny/lightline.vim'
call plug#end()

