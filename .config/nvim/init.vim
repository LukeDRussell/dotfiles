"
" Key Bindings
"

:imap jj <Esc>



"
" FileType indent custom files
"
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab


"
" Plugins
"
call plug#begin()
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'w0rp/ale'
Plug 'nathanaelkane/vim-indent-guides'
call plug#end()


