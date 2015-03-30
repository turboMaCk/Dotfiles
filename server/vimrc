" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.
set nocompatible

" Disable bleep!
set visualbell
set noerrorbells

" Leader
let mapleader = ","

" ======================
" Interface
" ======================

" setup commandline
if has('cmdline_info')
  set ruler
  set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%)
  set showcmd
endif

" setup statusline
if has('statusline')
  set laststatus=2
  set statusline=%<%f\ " Filename
  set statusline+=%w%h%m%r " Options
  set statusline+=\ [%{&ff}/%Y] " Filetype
  set statusline+=\ [%{getcwd()}] " Current dir
  set statusline+=%=%-14.(%l,%c%V%)\ %p%% " Right aligned file nav info
endif

" ======================
" Editor setup
" ======================

" Syntax highlighting
syntax on

" Set encoding
scriptencoding utf-8
set encoding=utf-8
setglobal fileencoding=utf-8
set nobomb
set termencoding=utf-8
set fileencodings=utf-8,iso-8859-15

" Setup mouse support
set mouse=a " Automatically enable mouse usage
set mousehide " Hide the mouse cursor while typing

" Easy backspace
set backspace=indent,eol,start

" Use system cliboard
set clipboard=unnamed

" Wildmenu
set wildmenu " Show list instead of just completing
set wildmode=list:longest,full " Command <Tab> completion, list matches, then longest common part, then all.
set whichwrap=b,s,h,l,<,>,[,] " Backspace and cursor keys wrap too

" Scrolling
set scrolljump=5 " Lines to scroll when cursor leaves screen
set scrolloff=3 " Minimum lines to keep above and below cursor
set foldenable " Auto fold code

" Abbrev. of messages (avoids 'hit enter')
set shortmess+=filmnrxoOtT

" Better Unix / Windows compatibility
set viewoptions=folds,options,cursor,unix,slash

" Allow for cursor beyond last character
set virtualedit=onemore

" Allow buffer switching without saving
set hidden

" ======================
" Formating setup
" ======================

" No extra spaces between rows
set linespace=3

" Line numbers
set number " Line numbers on
set relativenumber " Like relative numbers mod

" Show matching brackets/parenthesis
set showmatch

" Search setting
set incsearch " Find as you type search
set hlsearch " Highlight search terms
set ignorecase " Case insensitive search
set smartcase " Case sensitive when uc present

" Windows can be 0 line high
set winminheight=0

" Lines settings
set cursorline " Highlight current line
set nowrap " Do not wrap long lines
set tabpagemax=15 " Only show 15 tabs
set showmode " Display the current mode

" indentation
set autoindent " Indent at the same level of the previous line
filetype plugin indent on " when working with plaintext
set shiftwidth=2  " Use indents of 2 spaces
set expandtab " Tabs are spaces, not tabs
set tabstop=2 " An indentation every four columns
set softtabstop=2 " Let backspace delete indent

" Show whitespaces
set list
set listchars=tab:»\ ,trail:›,extends:#,nbsp:. " Highlight problematic whitespace

" Prevents inserting two spaces after punctuation on a join (J)
set nojoinspaces

" window spliting
set splitright " Puts new vsplit windows to the right of the current
set splitbelow " Puts new split windows to the bottom of the current
"set matchpairs+=<:> " Match, to be used with %
set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" ======================
" AUTO CMDS
" ======================

" Strip whitespace
function! StripTrailingWhitespace()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " do the business:
    %s/\s\+$//e
    " clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

" Restore cursor to file position in previous editing session
function! ResCur()
    if line("'\"") <= line("$")
        normal! g`"
        return 1
    endif
endfunction

augroup resCur
    autocmd!
    autocmd BufWinEnter * call ResCur()
augroup END

"===============================
" KEY MAPING
" ==============================

" Easy window navigation
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-L> <C-W>l<C-W>_
map <C-H> <C-W>h<C-W>_

" buffer swithing
map <silent> <leader>l :bn<CR>
map <silent> <leader>h :bp<CR>

" Easy split with file open
map <Leader>e :e <C-R>=expand("%:p:H") . '/'<CR>
map <Leader>s :split <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>v :vnew <C-R>=expand("%:p:h") . '/'<CR>

" Stupid shift key fixes
command! -bang -nargs=* -complete=file E e<bang> <args>
command! -bang -nargs=* -complete=file W w<bang> <args>
command! -bang -nargs=* -complete=file Wq wq<bang> <args>
command! -bang -nargs=* -complete=file WQ wq<bang> <args>
command! -bang Wa wa<bang>
command! -bang WA wa<bang>
command! -bang Q q<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
cmap Tabe tabe

" Toggle search highlighting rather than clear the current search results.
nmap <silent> <leader>/ :nohlsearch<CR>

" Visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" When forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

" Map <Leader>ff to display all lines with keyword under cursor and ask which one to jump to
nmap <Leader>ff [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

" Find merge conflict markers
map <leader>fc /\v^[<\|=>]{7}( .*\|$)<CR>

" Adjust viewports to the same size
map <Leader>= <C-w>=

" Allow using the repeat operator with a visual selection (!)
vnoremap . :normal .<CR>

" Wrapped lines goes down/up to next row, rather than next line in file.
noremap j gj
noremap k gk

" End/Start of line motion keys act relative to row/wrap width in the
" presence of `:set wrap`, and relative to line for `:set nowrap`.
function! WrapRelativeMotion(key, ...)
    let vis_sel=""
    if a:0
        let vis_sel="gv"
    endif
    if &wrap
        execute "normal!" vis_sel . "g" . a:key
    else
        execute "normal!" vis_sel . a:key
    endif
endfunction
