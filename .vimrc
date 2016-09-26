" ~/.vimrc
" Author: matheuristic

" Section: Vi Compatibility {{{1
" ------------------------------

set nocompatible " vi non-compatible mode

"}}}1
" Section: Plugins {{{1
" ---------------------

" Suggested plugin manager:
"   pathogen.vim - package manager | https://github.com/tpope/vim-pathogen [or vim-plug]
" Suggested plugins:
"   CtrlP - fuzzy file finder | https://github.com/ctrlpvim/ctrlp.vim [or fzf or unite.vim]
"   deoplete - auto-complete, requires editor be built with Python3 support (Neovim-only) | https://github.com/Shougo/deoplete.nvim [see for installation instructions]
"   elm-vim - Elm plugin for vim | https://github.com/ElmCast/elm-vim
"   ghcmod-vim - Haskell type checking, linting, and code expansion using ghc-mod, requires vimproc | https://github.com/eagletmt/ghcmod-vim
"   neco-ghc - more Haskell auto-completion, requires neocomplete or deoplete | https://github.com/eagletmt/neco-ghc
"   neocomplete - auto-complete, requires editor be built with lua support (Vim-only) | https://github.com/Shougo/neocomplete.vim
"   Neomake - asynchronous make (can be configured for linting) | https://github.com/neomake/neomake
"   tagbar - tag browser | https://github.com/majutsushi/tagbar
"   vim-commentary - block comment and uncomment | https://github.com/tpope/vim-commentary
"   vim-fireplace - Clojure REPL support | https://github.com/tpope/vim-fireplace
"   vim-fugitive - Git wrapper | https://github.com/tpope/vim-fugitive
"   vim-orgmode - emulate Emacs' Org-mode, requires vim-speeddating | https://github.com/jceb/vim-orgmode
"   vim-python-pep8-indent - modify indentation to fit PEP8 | https://github.com/hynek/vim-python-pep8-indent
"   vim-racket - Racket file auto-detect, indentation and syntax highlighting | https://github.com/wlangstroth/vim-racket
"   vim-rails - Rails tools | https://github.com/tpope/vim-rails
"   vim-repeat - interface to extend '.' command to plugin maps | https://github.com/tpope/vim-repeat
"   vim-sexp - emulates Emacs' paredit mode, supports vim-repeat | https://github.com/guns/vim-sexp
"   vim-slime - emulates Emacs' SLIME plugin, requires that screen or tmux be installed | https://github.com/jpalardy/vim-slime [see for configuration instructions]
"   vim-speeddating - increment and decrement dates in Vim | https://github.com/tpope/vim-speeddating
"   vim-surround - paranthesis, bracket and quote manipulations | https://github.com/tpope/vim-surround
"   vim-unimpaired - handy bracket mappings | https://github.com/tpope/vim-unimpaired
"   vim-vinegar - enhancements for netrw | https://github.com/tpope/vim-vinegar
"   vim-virtualenv - virtualenv switching within Vim | https://github.com/jmcantrell/vim-virtualenv
"   vimproc - asynchronous execution library | https://github.com/Shougo/vimproc.vim [see for installation instructions]
"   YankRing - emulates Emacs' kill ring | https://github.com/vim-scripts/YankRing.vim
" Suggested color themes:
"   gruvbox | https://github.com/morhetz/gruvbox
"   nofrils | https://github.com/robertmeta/nofrils
"   zenburn | https://github.com/jnurmine/Zenburn

" Pathogen package manager
runtime! autoload/pathogen.vim " force autoload so check on next lines works
if !exists('g:loaded_pathogen') " if yet to load, try the bundle dir
  runtime! bundle/vim-pathogen/autoload/pathogen.vim
endif
if exists('g:loaded_pathogen') " load plugins and update help docs
  execute pathogen#infect()
  execute pathogen#helptags()
endif

" CtrlP settings {{{2
let g:ctrlp_map = '<Leader><C-p>' " remap invocation key to <Leader><C-p>
let g:ctrlp_show_hidden = 1       " show dot files
" }}}2

" deoplete (Neovim) and neocomplete (Vim) settings {{{2
if has('nvim') && has('python3') " Neovim
  let g:deoplete#enable_at_startup = 1
elseif !has('nvim') && has('lua') " Vim
  let g:neocomplete#enable_at_startup = 1
endif
" }}}2

" Neomake settings {{{2
"let g:neomake_cpp_enable_makers = ['clang', 'g\+\+']
let g:neomake_cpp_clang_args = ['-std=c++11', '-Wextra', '-Wall']
" runs make command on current file
nnoremap <Leader>mm :Neomake<CR>
" runs make command with no input file (project-level)
nnoremap <Leader>m! :Neomake!<CR>
" }}}2

" Tagbar settings {{{2
let g:tagbar_width = 30          " set width of Tagbar window to 30 cols
let g:tagbar_show_visibility = 1 " show visibility symbols (public/protected/private)
" }}}2

" vim-sexp settings {{{2
let g:sexp_filetypes = '' " disable default sexp mappings
let g:sexp_leader = '<C-m>' " special leader character for sexp, see SexpMappings function
let g:sexp_motions = [
      \ [['n', 'x', 'o'], 'b',        '<Plug>(sexp_move_to_prev_element_head'],
      \ [['n', 'x', 'o'], 'w',        '<Plug>(sexp_move_to_next_element_head'],
      \ [['n', 'x', 'o'], 'ge',       '<Plug>(sexp_move_to_prev_element_tail'],
      \ [['n', 'x', 'o'], 'e',        '<Plug>(sexp_move_to_next_element_tail'],
      \ [['n'],           '<Space>h', '<Plug>(sexp_insert_at_list_head)'],
      \ [['n'],           '<Space>l', '<Plug>(sexp_insert_at_list_tail)'],
      \ [['n'],           '<Space>@', '<Plug>(sexp_splice_list)'],
      \ [['n', 'x'],      '<Space>o', '<Plug>(sexp_raise_list)'],
      \ [['n', 'x'],      '<Space>O', '<Plug>(sexp_raise_element)'],
      \ [['n', 'x'],      'k',        '<Plug>(sexp_swap_list_backward)'],
      \ [['n', 'x'],      'j',        '<Plug>(sexp_swap_list_forward)'],
      \ [['n', 'x'],      'h',        '<Plug>(sexp_swap_element_backward)'],
      \ [['n', 'x'],      'l',        '<Plug>(sexp_swap_element_forward)'],
      \ [['n', 'x'],      'J',        '<Plug>(sexp_emit_head_element)'],
      \ [['n', 'x'],      'K',        '<Plug>(sexp_emit_tail_element)'],
      \ [['n', 'x'],      'H',        '<Plug>(sexp_capture_prev_element)'],
      \ [['n', 'x'],      'L',        '<Plug>(sexp_capture_next_element)'],
      \ ] " list of sexp mappings for different modes, see SexpMappings function
" }}}2

" YankRing settings {{{2
" show yankring entries
nnoremap <Leader>y :YRShow<CR>
"}}}2

" }}}1
" Section: Options {{{1
" ---------------------

set autoindent  " use indent level from previous line
"set autoread    " watch for file changes by other programs
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set backup      " keep a backup file
set backupdir=~/.backup,.,~/tmp/,~/ " use ~/.backup to keep backups, note that file names do not contain the dirname as prefixes
"set directory=~/.tmp//,.,~/tmp/,/var/tmp,/tmp " use ~/.tmp for swap files
"set binary noeol " do not autowrite <EOL> at end of file, resets 'textwidth', 'wrapmargin', 'modeline' and 'expandtab'
"set complete=.,w,b,u,U,t,i,d " extra scanning on keyword completion
set complete+=k " also use dictionaries on keyword completion
set expandtab   " expand tabs into spaces; use <C-v><Tab> for a real tab
set formatoptions+=j " delete comment leader when joining comment lines
set gdefault    " substitute all matches in a string by default
set grepprg=grep\ -nH\ $* " set grep to always show filename
set ignorecase  " make searches case-insensitive
set laststatus=2 " always show status line, even when editing just one file
set list        " highlight tabs and trailing whitespace
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:. " chars for displaying whitespace when 'list' is set
"set hidden      " hide abandoned buffers instead of unloading them
set nojoinspaces " do not insert two spaces after '.', '?' and '!' on line joins
"set nomodeline  " do not have files overwrite settings from this vimrc
set nowrap      " do not wrap text
"set scrolloff=1 " num lines from top or bottom of window to begin scrolling
set sidescrolloff=5 " num lines from left or right of window to begin scrolling
set shiftwidth=2 " number of spaces for each indent level
"set showmatch   " show matching brackets
set showmode    " show current mode
set smartcase   " override 'ignorecase' if search pattern has upper case chars
set smarttab    " tabs inserts shiftwidth space
"set softtabstop=4 " num spaces a tab counts for while in insert mode
"set tabstop=8   " length of a real tab
set ttyfast     " smoother output
set wildmode=list:longest,full " command-line tab completion options

" Automatically switch working directory to current file's {{{2
"if has('autochdir')
"  set autochdir
"endif
" }}}2
" Set 'cindent' indentation to shiftwidth {{{2
if has('cindent')
  set cinoptions=>1s
endif
" }}}2
" More command line info {{{2
if has('cmdline_info')
  set showcmd   " Show partial command in status line
  set ruler     " Show line and column numbers, superceded by 'statusline'
endif
" }}}2
" Show matches as while inputting search strings {{{2
if has('extra_search')
  set incsearch
endif
" }}}2
" Enable folding {{{2
if has('folding')
  set foldenable
  set foldmethod=marker
endif
" }}}2
" Insert mode completion options (<C-n> and <C-p> in Insert mode) {{{2
if has('insert_expand')
  set completeopt=longest,menuone,preview
endif
" }}}2
" Visually indicate wrapped lines {{{2
if has('linebreak')
  set showbreak=...\  " put '... ' at start of each continued line
endif
" }}}2
" Status line {{{2
if has('statusline') && (version >= 700)
  set statusline=               " clear the statusline
  set statusline+=%<            " truncate if too long
  set statusline+=%F            " filename
  set statusline+=\             " spacer
  set statusline+=%h            " help buffer flag
  set statusline+=%m            " modified flag
  set statusline+=%r            " readonly flag
  set statusline+=%w            " preview window flag
  set statusline+=%{exists('g:loaded_fugitive')?fugitive#statusline():''} " Git branch and commit (requires fugitive.vim plugin)
  set statusline+=%=            " center auto-spacing
  set statusline+=%y            " filetype
  set statusline+=\             " spacer
  set statusline+=[%{&ff}]      " file format
  set statusline+=\             " spacer
  set statusline+=%{!&list?'':'[list]\ '} " show '[list'] if in list mode
  set statusline+=%{&pm==''?'':'[PM='.&pm.']\ '} " show patchmode if enabled
  set statusline+=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"} " show encoding and if 'bomb' option is set
  set statusline+=%k            " Value of 'b:keymap_name' of 'keymap' when :lmap mappings are being used
  set statusline+=\ \           " spacer
  set statusline+=%-14.(%l,%c%V%) " position (line, column and virtual column) of cursor
  set statusline+=\             " spacer
  set statusline+=%P            " Percentage through file of displayed window
endif
" }}}2
" Set syntax highlighting colorscheme {{{2
if has('syntax') && (&t_Co > 2)
  silent! unset g:colors_name
  syntax on
  " List of (colors_name[, numcolors needed])
  let s:colors_list = [['zenburn', 256], ['gruvbox', 256], ['nofrils-dark', 256], ['default', 8]]
  " In list order, try setting color scheme if the terminal emulator supports
  " the number of colors necessary for the scheme (default: 256)
  for color_pair in s:colors_list
    if !exists('colors_name') && (&t_Co >= get(color_pair, 1, 256))
      if color_pair[0] =~ '^gruvbox'
        set background=dark
        let g:gruvbox_contrast_dark = 'soft'
      endif
      if color_pair[0] =~ '^nofrils'
        "let g:nofrils_heavycomments = 1
        "let g:nofrils_heavylinenumbers = 1
        let g:nofrils_strbackgrounds = 1
      endif
      silent! execute 'colorscheme' color_pair[0]
    endif
  endfor
  unlet! s:colors_list
endif
" }}}2
" Set the terminal emulator title to path of file being edited {{{2
if has('title')
  set title
endif
" }}}2

" }}}1
" Section: Autocommands {{{1
" --------------------------

if has('autocmd')
  filetype plugin indent on " enable filetype detection for plugins and indents
  augroup cron " {{{2
    autocmd FileType crontab setlocal nobackup nowritebackup
  augroup END " }}}2
  augroup encrypted " {{{2
    " Transparent editing of gnupg encrypted files (Wouter Hanegraaff <wouter@blub.net>)
    autocmd!
    " First make sure nothing is written to ~/.viminfo while editing an encrypted file.
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set viminfo=
    " We don't want a backup and swap files, as they are written unencrypted to disk
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set noswapfile nobackup
    " Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set bin
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let shsave = &sh
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let &sh = 'sh'
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg '[,']!gpg --decrypt --default-recipient-self 2> /dev/null
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &sh = shsave
    " Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg set nobin
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg execute ":doautocmd BufReadPost " . expand("%:r")
    " Convert all text to encrypted text before writing
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg set bin
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let shsave = &sh
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh = 'sh'
    autocmd BufWritePre,FileWritePre    *.gpg '[,']!gpg --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc '[,']!gpg --armor --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh = shsave
    " Undo the encryption so we are back in the normal text, directly after the file has been written.
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg silent u
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg set nobin
  augroup END " }}}2
  augroup mail " {{{2
    autocmd!
    autocmd FileType mail if has('syntax') | set spell textwidth=70 wrap nonumber | endif
  augroup END " }}}2
  augroup programming " {{{2
    autocmd!
    " Display line numbers
    autocmd FileType c,cpp,java,javascript,haskell,make,perl,python set number
    " Omnifunc completions
    autocmd FileType c,cpp,java,javascript,haskell,make,perl,python
          \ if has('eval') || has('insert_expand')
          \ | set omnifunc=syntaxcomplete#Complete
          \ | endif
    " For languages where whitespace is not important,
    " expand tabs to 2 spaces and use 2 space indents
    autocmd FileType c,cpp,java,javascript,perl set expandtab shiftwidth=2 tabstop=2
    " For C (not C++) where comments have explicit end characters,
    " automatically insert comment leader characters when starting a new line
    " in the middle of a comment
    autocmd FileType c set formatoptions+=ro
    " For C-like languages, have automatic indentation:
    autocmd FileType c,cpp if has('cindent') | set cindent | endif
    " For Clojure, Lisp, Racket, Scheme, use custom sexp mappings for vim-sexp plugin
    autocmd FileType clojure,lisp,racket,scheme call SexpMappings()
    " For Haskell, expand tabs to 2 spaces and set 2 space indents
    autocmd FileType haskell set expandtab tabstop=2 shiftwidth=2
    " In Makefiles, do not expand tabs to spaces, since actual tab characters
    " are needed, and have indentation at 8 chars to be sure that all
    " indents are tabs (despite the mappings later):
    autocmd FileType make set noexpandtab nosmarttab shiftwidth=8 tabstop=8
    " For Perl, have things in braces indenting themselves:
    autocmd FileType perl if has('smartindent') | set smartindent | endif
    " For Python, expand tabs to 4 spaces and set 4 space indents
    autocmd FileType python set expandtab tabstop=4 shiftwidth=4
    " For Python, use cindent with the appropriate keywords
    autocmd FileType python
          \ if has('cindent')
          \ | set cindent cinwords=class,def,elif,else,except,finally,for,if,try,while
          \ | endif
  augroup END " }}}2
  augroup web " {{{2
    autocmd!
    " For both CSS and HTML, display line numbers
    autocmd FileType css,html,xhtml set number
    " Omnifunc completions
    autocmd FileType css,html,xhtml
          \ if has('eval') || has('insert_expand')
          \ | set omnifunc=syntaxcomplete#Complete
          \ | endif
    " For HTML, do not have Vim automatically add a <CR> or line break
    " at the end of the last line if there isn't one, otherwise
    " the default http headers will be sent
    autocmd FileType html,xhtml set binary noeol
    " For both CSS and HTML, use genuine tab characters for
    " indentation to make files a few bytes smaller:
    autocmd FileType css,html,xhtml set noexpandtab shiftwidth=2 tabstop=2
    " For CSS, also have things in braces indented:
    autocmd FileType css if has('smartindent') | set smartindent | endif
    " For HTML, generally format text, but if a long line has been created,
    " leave it alone when editing
    autocmd FileType html,xhtml set formatoptions+=tl
  augroup END " }}}2
endif

" }}}1
" Section: Commands {{{1
" ------------------------------------

" Write file as superuser with :Sudow (user should be in the /etc/sudoers file)
if executable('sudo')
  command! -nargs=0 -bang -bar Sudow w<bang> !sudo tee % >/dev/null
endif

" }}}1
" Section: Keymappings {{{1
" -------------------------

" Default map leader <Leader> is '\'

" Map <Space> in normal mode to map leader {{{2
if has('user_commands')
  nmap <Space> <Leader>
endif
" }}}2
" Buffer manipulation and navigation {{{2
nnoremap <silent> <Leader>bl :buffers<CR>
nnoremap <silent> <Leader>bd :bdelete<CR>
nnoremap <silent> <Leader>bn :bnext<CR>
nnoremap <silent> <Leader>bp :bprevious<CR>
nnoremap <Leader>bg :buffer!<Space>
nnoremap <Leader>bad :badd<Space>
nnoremap <silent> <Leader>bal :ball<CR>
" }}}2
" Tab and window manipulation and navigation {{{2
if has('windows')
  " Tab commands
  nnoremap <silent> <Leader>tl :tabs<CR>
  nnoremap <silent> <Leader>te :tabedit<CR>
  nnoremap <silent> <Leader>tc :tabclose<CR>
  nnoremap <silent> <Leader>tn :tabnext<CR>
  nnoremap <silent> <Leader>tp :tabprevious<CR>
  nnoremap <silent> <Leader>t0 :tabfirst<CR>
  nnoremap <silent> <Leader>t1 :tabnext 1<CR>
  nnoremap <silent> <Leader>t2 :tabnext 2<CR>
  nnoremap <silent> <Leader>t3 :tabnext 3<CR>
  nnoremap <silent> <Leader>t4 :tabnext 4<CR>
  nnoremap <silent> <Leader>t5 :tabnext 5<CR>
  nnoremap <silent> <Leader>t6 :tabnext 6<CR>
  nnoremap <silent> <Leader>t7 :tabnext 7<CR>
  nnoremap <silent> <Leader>t8 :tabnext 8<CR>
  nnoremap <silent> <Leader>t9 :tabnext 9<CR>
  nnoremap <silent> <Leader>t$ :tablast<CR>
  nnoremap <Leader>tm :tabmove<Space>
  nnoremap <Leader>tg :tabnext<Space>
  nnoremap <Leader>td :tabdo<Space>
  " Window commands
  " Resize window height to fit number of lines of buffer
  nnoremap <silent> <Leader>wr :execute ":resize " . line('$')<CR>
  " Resize window width to fit max line width of buffer
  nnoremap <silent> <Leader>wR :execute ": vertical resize "
        \ . max(map(range(1, line('$')), "virtcol([v:val, '$'])-1"))<CR>
endif
" }}}2
" Quickfix error and location window manipulation {{{2
if has('quickfix')
  nnoremap <silent> <Leader>qco :copen<CR>
  nnoremap <silent> <Leader>qcc :cclose<CR>
  nnoremap <silent> <Leader>qlo :lopen<CR>
  nnoremap <silent> <Leader>qlc :lclose<CR>
endif
" }}}2
" Toggle mouse {{{2
nnoremap <silent> <Leader>MM :if &mouse == 'a' <Bar> set mouse= <Bar> else <Bar> set mouse=a <Bar> endif <Bar> set mouse?<CR>
" }}}2
" Unhighlight search results (from https://github.com/tpope/vim-sensible) {{{2
if has('extra_search')
  nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
endif
" }}}2
" Change directory to current file's {{{2
nnoremap <silent> <Leader>cd :cd %:p:h<CR>:pwd<CR>
" }}}2
" Toggle folding {{{2
if has('folding')
  nnoremap <silent> <Leader>f :set foldenable! foldenable?<CR>
endif
" }}}2
" Toggle spell check {{{2
if has('syntax')
  nnoremap <silent> <Leader>Sp :set spell! spell?<CR>
endif
" }}}2
" Toggle wrapping of lines {{{2
nnoremap <silent> <Leader>W :set wrap! wrap?<CR>
" }}}2
" Toggle paste mode {{{2
nnoremap <silent> <Leader>P :set paste! paste?<CR>
" }}}2
" Toggle highlighting of listchars {{{2
nnoremap <silent> <Leader>L :set list! list?<CR>
" }}}2
" Toggle modeline (reload file with :e to effect change) {{{2
nnoremap <silent> <Leader>ML :set modeline! modeline?<CR>
" }}}2
" Tagbar commands (requires Tagbar plugin) {{{2
nnoremap <silent> <Leader>T :TagbarToggle<CR>
" }}}2

" }}}1
" Section: Functions {{{1
" -----------------------

function! SexpMappings() " {{{2
  " custom vim-sexp key mappings
  if exists('g:sexp_loaded') && empty(g:sexp_filetypes)
    if !exists('g:sexp_leader')
      echo 'g:sexp_leader should be defined'
      return
    endif
    if !exists('g:sexp_motions')
      echo 'g:sexp_motions should be defined'
      return
    endif
    " g:sexp_leader should be a key, e.g. '<C-L>'
    " g:sexp_motions should be a (mode list, key, vim-sexp command) tuple list,
    "   e.g. [[['n', 'x'], 'o', '<Plug>(sexp_raise_list)'], [['n'], '@', '<Plug>(sexp_splice_list)']]
    for map_motion in g:sexp_motions
      for map_mode in map_motion[0]
	execute map_mode . 'map <buffer> ' . g:sexp_leader . map_motion[1] . ' ' . map_motion[2]
      endfor
    endfor
  endif
endfunction
" }}}2

" }}}1
" Section: Local Vim Config {{{1
" ------------------------------

if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif

"}}}1
" Modeline {{{1
" vim:set ft=vim et sw=2 fen fdc=1 fdm=marker: }}}1
