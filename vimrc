set nocompatible
filetype plugin on

" install pathogen
" mkdir -p ~/.vim/autoload ~/.vim/bundle && \
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
execute pathogen#infect()

set runtimepath+=~/.vim/bundle/neobundle.vim/

" install syntastic
" cd ~/.vim/bundle && \
" git clone https://github.com/scrooloose/syntastic.git

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimfiler.vim'

call neobundle#end()


filetype plugin indent on
NeoBundleCheck

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

set expandtab
set tabstop=4
set shiftwidth=4
set smartindent
let g:vimfiler_as_default_explorer=1
let g:syntastic_always_populate_loc_list = 1
autocmd BufWritePre * :%s/\s\+$//ge
autocmd BufWritePre * :%s/\t/ /ge
syntax on
