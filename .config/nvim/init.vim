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
"Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
call plug#end()


