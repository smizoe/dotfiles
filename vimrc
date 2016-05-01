set nocompatible

" install pathogen
" mkdir -p ~/.vim/autoload ~/.vim/bundle && \
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
execute pathogen#infect()

set runtimepath+=~/.vim/bundle/neobundle.vim/

" install syntastic
" cd ~/.vim/bundle && \
" git clone https://github.com/scrooloose/syntastic.git

""" NeoBundle related settings
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
" vim-orgmode depends vim-speeddating
NeoBundle 'jceb/vim-orgmode'
NeoBundle 'tpope/vim-speeddating'

" make friends with git
NeoBundle 'tpope/vim-fugitive'

" gauche
NeoBundle 'aharisu/vim_goshrepl'

" rust
NeoBundle 'rust-lang/rust.vim'

" Nvim-R
NeoBundle 'jalvesaq/Nvim-R'

call neobundle#end()
NeoBundleCheck
""

runtime macros/matchit.vim
syntax enable
filetype plugin on
filetype indent on

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
xmap <C-z>     <Plug>(neosnippet_register_oneshot_snippet)

" move freely in command mode
cnoremap <C-A> <Home>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>

" set , to <leader>
let mapleader = ","
" edit .vimrc easily
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>
" dot command with region
vnoremap <leader>r. :normal!.<cr>
nnoremap <leader>w :match Error /\v[^\s]\s+$/<cr>
nnoremap <leader>W :match<cr>
nnoremap <leader>nh :nohlsearch<cr>
nnoremap <leader>cn :cnext<cr>
nnoremap <leader>cp :cprevious<cr>
nnoremap <leader>P  :setlocal paste!<cr>
nnoremap <leader>N  :setlocal number!<cr>

set expandtab
set tabstop=2
set shiftwidth=2
set smartindent
set backspace=indent,eol,start
set number
set hlsearch incsearch

""" set statusline (from learn vimscript the hard way ch. 17)
set statusline=%F         " Path to the file
set statusline+=\ %m\ %r  " modified and readonly flag
set statusline+=%=        " Switch to the right side
set statusline+=%y        " file type
set statusline+=\ %l        " Current line
set statusline+=/         " Separator
set statusline+=%L        " Total lines
"" finish statusline setting


let g:vimfiler_as_default_explorer=1
let g:syntastic_always_populate_loc_list = 1
augroup remove_tabs_and_trailing_spaces
  autocmd!
  function! s:DeleteTabsAndTrailingSpaces()
    %s/\s\+$//ge " remove trailing spaces
    if expand('%:t') !~# "\\.\\(snip\\|tsv\\)$"
      %s/\t/ /ge " remove tabs only when the file is not tsv
    endif
  endfunction
  autocmd BufWritePre * :call <SID>DeleteTabsAndTrailingSpaces()
augroup END
augroup r_specific_conf
  autocmd!
  autocmd Filetype r,rout,rmd,rrst inoreabbrev pp %>%
augroup END
augroup org_specific_conf
  autocmd!
  autocmd Filetype org set tabstop=2 shiftwidth=2
augroup END

augroup chdir_on_bufenter
  autocmd!
  function! s:ChdirToParentOfCurrentFile()
    let l:parent = fnamemodify(resolve(expand("%:p")), ":h")
    if l:parent !~ '^/tmp'
      execute "silent! lchdir " . l:parent
    endif
  endfunction
  autocmd BufEnter * :call <SID>ChdirToParentOfCurrentFile()
augroup END

function! s:MkJunkFile(...)
  let l:junk_dir = join([expand('~/junk'), strftime("%Y-%m")], "/")
  if ! isdirectory(l:junk_dir)
    call mkdir(l:junk_dir, "p")
  endif
  let l:junk_file = strftime("%Y-%m-%d_%H-%M-%S")
  if a:0 > 0
    let l:junk_file = join([l:junk_file, a:1], "_")
  endif
  let l:junk_file = l:junk_file . ".org"
  execute 'edit ' . join([l:junk_dir, l:junk_file], "/")
endfunction
command! -nargs=? Memo call <SID>MkJunkFile(<args>)

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
" add neosnippet directory
let g:neosnippet#snippets_directory = '~/dotfiles/neosnippet-snippets'

" vim_goshrepl
vmap <CR> <Plug>(gosh_repl_send_block)

" rust autofmt
let g:rustfmt_autosave = 1

" make triple "s or more into a fold
" see http://vi.stackexchange.com/questions/3814/is-there-a-best-practice-to-fold-a-vimrc-file
"" vim:fdm=expr:fdl=0
"" vim:fde=getline(v\:lnum)=~'^""'?'>'.(matchend(getline(v\:lnum),'""*')-2)\:'='
