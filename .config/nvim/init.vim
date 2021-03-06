"
" Control
"

set mouse=a
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
set termguicolors

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
highlight Comment cterm=italic

"
" Plugin settings
"
let g:indentLine_enabled = 1
let g:indentLine_char = '▏'
let g:ansible_unindent_after_newline = 1
let g:airline_powerline_fonts = 1
let g:airline_theme='solarized'
let g:airline_solarized_bg='dark'

let g:python3_host_prog = '~/.pyenv/versions/neovim/bin/python'

" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

"
" Plugins
"
call plug#begin()

Plug 'junegunn/goyo.vim'
Plug 'yggdroot/indentline'

Plug 'w0rp/ale'

Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'edkolev/promptline.vim'

Plug 'cespare/vim-toml'
Plug 'pearofducks/ansible-vim'

Plug 'lifepillar/vim-solarized8'

call plug#end()

