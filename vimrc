set nocompatible
filetype plugin on
set omnifunc=syntaxcomplete#Complete

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
NeoBundle 'vim-scripts/Align'
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tyru/eskk.vim'
" vimproc needs manual intervention;
" cd path/to/vimproc.vim && make
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'tpope/vim-surround'
NeoBundle 'vim-scripts/Vim-R-plugin'

call neobundle#end()


filetype plugin indent on
NeoBundleCheck

runtime macros/matchit.vim

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
xmap <C-z>     <Plug>(neosnippet_register_oneshot_snippet)
cnoremap <C-A> <Home>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>

set expandtab
set tabstop=2
set shiftwidth=2
set smartindent
set backspace=indent,eol,start
let g:vimfiler_as_default_explorer=1
let g:syntastic_always_populate_loc_list = 1
autocmd BufWritePre * :%s/\s\+$//ge
autocmd BufWritePre * :%s/\t/ /ge
syntax on

" expands to 'dirname ${current_file}' with %%
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" enable omnicompletion
set omnifunc=syntaxcomplete#Complete

" set search path for ctags
set tags=tags,./tags

" eskk dictionary setting
" assumed that some skk server is listening to localhost:1178
let g:eskk#server = {
\   'host': 'localhost',
\   'port': 1178,
\}
