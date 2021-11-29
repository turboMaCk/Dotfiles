" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.
set nocompatible

"==============
"  Bundles
"==============
" required
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Use Vundle for plugin management
Plugin 'gmarik/Vundle.vim'

" Cache files
Plugin 'vim-addon-mw-utils'

" Another have to bundle
Plugin 'tomtom/tlib_vim'

" Editor config is another have to
Plugin 'editorconfig/editorconfig-vim'

" ~Ack~ AG is better than grep
Plugin 'rking/ag.vim'

Plugin 'matthewtodd/vim-twilight'

" basic plugins
Plugin 'scrooloose/nerdtree'
Plugin 'spf13/vim-autoclose'
Plugin 'ChrisKempson/Vim-Tomorrow-Theme'
Plugin 'nanotech/jellybeans.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'vim-scripts/sessionman.vim'
Plugin 'matchit.zip'
Plugin 'bling/vim-airline'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'mbbill/undotree'
Plugin 'vim-scripts/restore_view.vim'
Plugin 'mhinz/vim-signify'
Plugin 'tpope/vim-abolish.git'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-surround'

" Pick one of the checksyntax, jshint, or syntastic
Plugin 'scrooloose/syntastic'
Plugin 'tpope/vim-fugitive'
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'godlygeek/tabular'
Plugin 'majutsushi/tagbar'

" Snippets
Plugin 'honza/vim-snippets'
" Source support_function.vim to support vim-snippets.
if filereadable(expand("~/.vim/bundle/vim-snippets/snippets/support_functions.vim"))
    source ~/.vim/bundle/vim-snippets/snippets/support_functions.vim
endif

" Dash documentation tool
Plugin 'rizzatti/dash.vim'

"=================================
" LANGUAGES / FRAMEWORKS SPECIFIC
"=================================

" PHP
Plugin 'spf13/PIV'
Plugin 'arnaud-lb/vim-php-namespace'

" PYTHON
" Pick either python-mode or pyflakes & pydoc
"Plugin 'klen/python-mode'
"Plugin 'python.vim'
"Plugin 'python_match.vim'
"Plugin 'pythoncomplete'

" JAVASCRIPT
Plugin 'elzr/vim-json'
Plugin 'pangloss/vim-javascript'
Plugin 'briancollins/vim-jst'
Plugin 'kchmck/vim-coffee-script'
Plugin 'nono/vim-handlebars'
Plugin 'dsawardekar/ember.vim'
Plugin 'mxw/vim-jsx'

" ELM
Plugin 'lambdatoast/elm.vim'

" JAVA
"Plugin 'derekwyatt/vim-scala'
"Plugin 'derekwyatt/vim-sbt'

" HASKEL
Plugin 'travitch/hasksyn'
Plugin 'dag/vim2hs'
Plugin 'lukerandall/haskellmode-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'Shougo/vimproc'
Plugin 'adinapoli/cumino'
Plugin 'bitc/vim-hdevtools'

" HTML
Plugin 'ypid/HTML-AutoCloseTag'
Plugin 'othree/html5.vim'
Plugin 'gorodinskiy/vim-coloresque'
Plugin 'tpope/vim-haml'
Plugin 'digitaltoad/vim-jade'
Plugin 'mattn/emmet-vim'

" CSS/preprocessors
Plugin 'hail2u/vim-css3-syntax'
Plugin 'groenewege/vim-less'
Plugin 'wavded/vim-stylus'
" haml bundle include sass support

" RUBY
Plugin 'tpope/vim-rails'
Plugin 'thoughtbot/vim-rspec'
Plugin 'tpope/vim-endwise'
Plugin 'yaymukund/vim-rabl'

" GO
Plugin 'Blackrush/vim-gocode'

" TEXT
Plugin 'tpope/vim-markdown'
Plugin 'spf13/vim-preview'
Plugin 'tpope/vim-cucumber'
Plugin 'quentindecock/vim-cucumber-align-pipes'
Plugin 'Puppet-Syntax-Highlighting'

" ELIXIR
Plugin 'elixir-lang/vim-elixir'

" NETTE FW
Plugin 'Janiczek/vim-latte'

" RUST
Plugin 'rust-lang/rust.vim'

" Processing
"Plugin 'sophacles/vim-processing'

" Android
"Plugin 'hsanson/vim-android'

" Vimux for tmux integration
Plugin 'benmills/vimux'

" Seamless navigation between tmux panes and vim splits
Plugin 'christoomey/vim-tmux-navigator'

"Plugin 'tclem/vim-arduino'

call vundle#end()
filetype plugin indent on

" HTML 5
" Vim syntax file
" Language:     HTML (version 5)
" Maintainer:   Rodrigo Machado <rcmachado@gmail.com>
" URL:          http://gist.github.com/256840
" Last Change:  2010 Aug 26
" License:      Public domain
"               (but let me know if you liked it :) )
"
" Note: This file just adds the new tags from HTML 5
"       and don't replace default html.vim syntax file

" HTML 5 tags
syn keyword htmlTagName contained article aside audio bb canvas command datagrid
syn keyword htmlTagName contained datalist details dialog embed figure footer
syn keyword htmlTagName contained header hgroup keygen mark meter nav output
syn keyword htmlTagName contained progress time ruby rt rp section time video
syn keyword htmlTagName contained source figcaption

" HTML 5 arguments
syn keyword htmlArg contained autofocus autocomplete placeholder min max step
syn keyword htmlArg contained contenteditable contextmenu draggable hidden item
syn keyword htmlArg contained itemprop list sandbox subject spellcheck
syn keyword htmlArg contained novalidate seamless pattern formtarget manifest
syn keyword htmlArg contained formaction formenctype formmethod formnovalidate
syn keyword htmlArg contained sizes scoped async reversed sandbox srcdoc
syn keyword htmlArg contained hidden role
syn match   htmlArg "\<\(aria-[\-a-zA-Z0-9_]\+\)=" contained
syn match   htmlArg contained "\s*data-[-a-zA-Z0-9_]\+"


" Disable bleep!
set visualbell
set noerrorbells

" Leader
let mapleader = ","

" File Type
filetype plugin on

" backup, undo and swap files
set backup
set undofile
set writebackup
set swapfile
set dir=~/.vimswap//,/var/tmp//,/tmp//,.

" Reload file when it changes
set autoread

" Not writes are annoying
set confirm

" UTF-8 FTW
set encoding=utf-8  " The encoding displayed.
set fileencoding=utf-8  " The encoding written to file.

" Disable Ex mode
nnoremap Q <Nop>

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
  set statusline+=%{fugitive#statusline()} " Git Hotness
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

" Setup history backup and undo
set swapfile
set backup
set history=1000
set undolevels=1000 " use many muchos levels of undo
set dir=~/.vim/tmp
set undodir=~/.vim/undo
set backupdir=~/.vim/backup

" Allow buffer switching without saving
set hidden

"change cursor in terminal
if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" ======================
" Formatting setup
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
filetype plugin indent on " when working with plain-text
set shiftwidth=2  " Use indents of 2 spaces
set expandtab " Tabs are spaces, not tabs
set tabstop=2 " An indentation every four columns
set softtabstop=2 " Let backspace delete indent

" Show white-spaces
set list
set listchars=tab:»\ ,trail:›,extends:#,nbsp:. " Highlight problematic white-space

" Prevents inserting two spaces after punctuation on a join (J)
set nojoinspaces

" window splitting
set splitright " Puts new vertical split windows to the right of the current
set splitbelow " Puts new split windows to the bottom of the current
"set matchpairs+=<:> " Match, to be used with %
set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks

" ======================
" AUTO CMDs
" ======================

" coffee script syntax
autocmd BufNewFile,BufRead *.coffee set filetype=coffee

" Remove trailing whites-paces and ^M chars
autocmd FileType c,cpp,java,go,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer>  call StripTrailingWhitespace()
autocmd FileType go autocmd BufWritePre <buffer> Fmt
autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig
autocmd FileType haskell setlocal expandtab shiftwidth=2 softtabstop=2

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
autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \ exe "normal g`\"" |
    \ endif

"===============================
" GUI SETTINGS (macvim)
" ==============================
set guioptions-=T
set guioptions-=r
set guioptions-=L
set guifont=Menlo:h12

"===============================
" KEY MAPING
" ==============================

" Easy window navigation
nmap <silent> <C-J> <C-W>j<C-W><CR>
nmap <silent> <C-K> <C-W>k<C-W><CR>
nmap <silent> <C-L> <C-W>l<C-W><CR>
nmap <silent> <C-H> <C-W>h<C-W><CR>

" buffer swithing
map <silent> <leader>l :bn<CR>
map <silent> <leader>h :bp<CR>

" Easy split with file open
map <Leader>e :e <C-R>=expand("%:p:H") . '/'<CR>
map <Leader>s :split <C-R>=expand("%:p:h") . '/'<CR>
map <Leader>v :vnew <C-R>=expand("%:p:h") . '/'<CR>

" Easy split resize
if bufwinnr(1)
  map + <C-W>+
  map - <C-W>-
endif

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

" Setup colors
set t_Co=256 " enable 256 colors
"colorscheme jellybeans " set colorscheme
colorscheme twilight
set background=dark

" Spell checking on
set spell

" setup spell error styles
hi clear SpellBad
hi SpellBad cterm=underline

" ===================================
" ACK.vim / AG
" ===================================

let g:ackprg = 'ag --nogroup --nocolor --column'

" ===================================
" CTRLP
" ===================================

" Open buffer
map <Leader>m :CtrlPBuffer<CR>

" working path node
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
    \ 'dir': '\.git$\|\.hg$\|\.svn$\|\.sass-cache$\|\bower_components$\|\node_modules$\|\dist$',
    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

if executable('ag')
    let s:ctrlp_fallback = 'ag %s --nocolor --nogroup --column -f'
else
    let s:ctrlp_fallback = 'find %s -type f'
endif
let g:ctrlp_user_command = {
    \ 'types': {
        \ 1: ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others'],
        \ 2: ['.hg', 'hg --cwd %s locate -I .'],
    \ },
    \ 'fallback': s:ctrlp_fallback
\ }

" ===================================
" CTAGS
" ===================================

" map tags key
nmap <leader>t :TagbarToggle<CR>

" setup tags
set tags=./tags;/,~/.vimtags

" Make tags placed in .git/tags file available in all levels of a repository
let gitroot = substitute(system('git rev-parse --show-toplevel'), '[\n\r]', '', 'g')
if gitroot != ''
    let &tags = &tags . ',' . gitroot . '/.git/tags'
endif

" ===================================
" NERD TREE
" ===================================

" map nertree key
map <C-e> <plug>NERDTreeTabsToggle<CR>

let NERDTreeShowBookmarks=1
let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git', '\.hg', '\.svn', '\.bzr']
let NERDTreeIgnore += ['^\.bundle$', '^\.bzr$', '^\.git$', '^\.hg$', '^\.sass-cache$', '^\.svn$', '^\.$', '^\.\.$', '^Thumbs\.db$']
let NERDTreeChDirMode=0
let NERDTreeQuitOnOpen=0
let NERDTreeMouseMode=2
let NERDTreeShowHidden=1
let NERDTreeKeepTreeInNewTab=1
let g:nerdtree_tabs_open_on_gui_startup=0

" ===================================
" JSON
" ===================================
nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>

" ===================================
" FUGITIVE GIT
" ===================================
set diffopt+=vertical
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gr :Gread<CR>
nnoremap <silent> <leader>gw :Gwrite<CR>
nnoremap <silent> <leader>ge :Gedit<CR>

" ===================================
" OMNI COMPLETION
" ===================================
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" ===================================
" NEOCOMPLETE CACHE
" ===================================

" basic settings
let g:acp_enableAtStartup = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_camel_case_completion = 0
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_enable_auto_delimiter = 1
let g:neocomplcache_max_list = 15
let g:neocomplcache_force_overwrite_completefunc = 1
"let g:neocomplcache_disable_auto_complete = 1
let g:neocomplcache_min_syntax_length = 3

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
\ 'default' : '',
\ 'vimshell' : $HOME.'/.vimshell_hist',
\ 'scheme' : $HOME.'/.gosh_completions'
\ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns._ = '\h\w*'

" These two lines conflict with the default digraph mapping of <C-K>
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
\ 'default' : '',
\ 'vimshell' : $HOME.'/.vimshell_hist',
\ 'scheme' : $HOME.'/.gosh_completions'
\ }

"iunmap <CR>
" <ESC> takes you out of insert mode
inoremap <expr><Esc> pumvisible() ? "\<C-y>\<Esc>" : "\<Esc>"

" <Down> and <Up> cycle like <Tab> and <S-Tab>
inoremap <expr> <Down>  pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr> <Up>    pumvisible() ? "\<C-p>" : "\<Up>"

" Jump up and down the list
inoremap <expr> <C-d>   pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
inoremap <expr> <C-u>   pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

smap <TAB> <Right><Plug>(neosnippet_jump_or_expand)

"<C-h>, <BS>: close popup and delete backword char.
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()

" <TAB>: completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"

" Enable heavy omni completion.
let g:neocomplcache_omni_patterns = {}

let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'

" Snippets

" Use honza's snippets.
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

" Enable neosnippet snipmate compatibility mode
let g:neosnippet#enable_snipmate_compatibility = 1

" For snippet_complete marker.
if has('conceal')
    set conceallevel=2 concealcursor=i
endif

" Disable the neosnippet preview candidate window
" When enabled, there can be too much visual noise
" especially when splits are used.
set completeopt-=preview

" ======================
" UNDO TREE
" ======================
nnoremap <Leader>u :UndotreeToggle<CR>
" If undotree is opened, it is likely one wants to interact with it.
let g:undotree_SetFocusWhenToggle=1


" ======================
" Indent guides
" ======================
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

" ======================
" Airline
" ======================
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 0
"let g:airline_theme = 'tomorrow'

let g:airline_left_sep=''
let g:airline_right_sep=''

" ======================
" Dash
" ======================
map <leader>d :Dash
map <leader>D :Dash<CR>

" ======================
" Elm
" ======================
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:elm_syntastic_show_warnings = 1

let g:elm_format_autosave = 1
