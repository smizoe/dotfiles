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
