" Vim settings
" Author: matheuristic
" Updated: Mar 02 2016

" Section: Vi Compatibility {{{1
" ------------------------------

set nocompatible " vi non-compatible mode

"}}}1
" Section: Plugins {{{1
" ---------------------

" Suggested plugins:
" pathogen.vim - package manager
" commentary.vim - block comment and uncomment
" ctrlp.vim <or> fzf - fuzzy file finder (https://github.com/ctrlpvim/ctrlp.vim)
" dispatch.vim - asynchronous builds and tests
" fugitive.vim - Git wrapper
" surround.vim - paranthesis/bracket/quote manipulations
" tagbar - tag browser
" unimpaired.vim - handy bracket mappings
" vinegar.vim - enhancements for netrw
" zenburn - color scheme

" Use the Pathogen package manager, see https://github.com/tpope/vim-pathogen
runtime! autoload/pathogen.vim  " force autoload so check on next lines works
if !exists('g:loaded_pathogen')  " if yet to load, try the bundle dir
  runtime! bundle/vim-pathogen/autoload/pathogen.vim
endif
if exists('g:loaded_pathogen') " load plugins and update help docs
  execute pathogen#infect()
  execute pathogen#helptags()
endif

" Plugin-specific settings
let g:ctrlp_map = "<Leader><C-p>" " remap invocation key to <Leader><C-p>
let g:ctrlp_show_hidden = 1     " show files beginning with dot
let g:tagbar_width = 30         " set width of Tagbar window to 30 cols
let g:tagbar_show_visibility = 1 " show visibility symbols (public/protected/private)

" }}}1
" Section: Options {{{1
" ---------------------

set autoindent  " use indent level from previous line
"set autoread    " watch for file changes by other programs
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
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
set showmatch   " show matching brackets
set showmode    " show which mode we're in
set smartcase   " override 'ignorecase' if search pattern has upper case chars
set smarttab    " tabs (spaces if expandtab is set) inserts shiftwidth space
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
  set statusline+=%=            " center auto-spacing
  set statusline+=%y            " filetype
  set statusline+=\ \           " spacer
  set statusline+=[%{&ff}]      " file format
  set statusline+=\ \           " spacer
  set statusline+=%{!&list?'':'[list]\ \ '} " show '[list'] if in list mode
  set statusline+=%{&pm==''?'':'[PM='.&pm.']\ \ '} " show patchmode if enabled
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
  let s:colors_list = [['zenburn', 256], ['default', 8]]
  " In list order, try setting color scheme if the terminal emulator supports
  " the number of colors necessary for the scheme (default: 256)
  for color_pair in s:colors_list
    if !exists('colors_name') && (&t_Co >= get(color_pair, 1, 256))
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

  augroup encrypted " {{{2
    " Transparent editing of gnupg encrypted files
    " By Wouter Hanegraaff <wouter@blub.net>
    autocmd!
    " First make sure nothing is written to ~/.viminfo while editing an encrypted file.
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set viminfo=
    " We don't want a swap file, as it writes unencrypted data to disk
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set noswapfile
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
    autocmd Filetype mail if has('syntax') | set spell textwidth=70 wrap nonumber | endif
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
    " For Haskell, expand tabs to 2 spaces and set 2 space indents
    autocmd Filetype haskell set expandtab tabstop=2 shiftwidth=2
    " In Makefiles, do not expand tabs to spaces, since actual tab characters
    " are needed, and have indentation at 8 chars to be sure that all
    " indents are tabs (despite the mappings later):
    autocmd FileType make set noexpandtab nosmarttab shiftwidth=8 tabstop=8
    " For Perl, have things in braces indenting themselves:
    autocmd FileType perl if has('smartindent') | set smartindent | endif
    " For Python, expand tabs to 4 spaces and set 4 space indents
    autocmd Filetype python set expandtab tabstop=4 shiftwidth=4
    " For Python, use cindent with the appropriate keywords
    autocmd Filetype python
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
    autocmd Filetype css if has('smartindent') | set smartindent | endif
    " For HTML, generally format text, but if a long line has been created,
    " leave it alone when editing
    autocmd FileType html,xhtml set formatoptions+=tl
  augroup END " }}}2

  augroup highlightextrawhitespace " {{{2
    " Note: autocommands designed to work to not clobber NERD tree colors
    autocmd!
    " Highlight extra white space
    autocmd Syntax *[^{nerdtree}]* highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
    " Show trailing whitepace and spaces before a tab
    autocmd Syntax *[^{nerdtree}]* if has('syntax') | syntax match ExtraWhitespace /\s\+$\| \+\ze\t/ | endif
    " Show tabs that are not at the start of a line
    "autocmd Syntax *[^{nerdtree}]* syntax match ExtraWhitespace /[^\t]\zs\t\+/
    " Show spaces used for indenting (so you use only tabs for indenting)
    "autocmd Syntax * syntax match ExtraWhitespace /^\t*\zs \+/
  augroup END " }}}2
endif

" }}}1
" Section: Commands and Functions {{{1
" ------------------------------------

" Write file as root/superuser with :Sudow
" (uses 'sudo'; user needs to be in the /etc/sudoers file)
if executable('sudo')
  command! -nargs=0 -bang -bar Sudow w<bang> !sudo tee % >/dev/null
endif

" }}}1
" Section: Keymappings {{{1
" -------------------------

" Default map leader <Leader> is backslash '\'

" Map <Space> to the map leader in normal mode {{{2
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
  " Resize window width to fit maximum line width of buffer
  nnoremap <silent> <Leader>wR :execute ": vertical resize "
        \ . max(map(range(1, line('$')), "virtcol([v:val, '$'])-1"))<CR>
endif
" }}}2
" Quickfix error and location window commands {{{2
if has('quickfix')
  nnoremap <silent> <Leader>qeo :copen<CR>
  nnoremap <silent> <Leader>qec :cclose<CR>
  nnoremap <silent> <Leader>qlo :lopen<CR>
  nnoremap <silent> <Leader>qlc :lclose<CR>
endif
" }}}2
" Unhighlight search results (from https://github.com/tpope/vim-sensible) {{{2
if has('extra_search')
  nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
endif
" }}}2
" Change directory to that of file being edited {{{2
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
" Toggle modeline (file should be reloaded using :e after that) {{{2
nnoremap <silent> <Leader>M :set modeline! modeline?<CR>
" }}}2
" Tagbar commands (requires Tagbar plugin) {{{2
nnoremap <silent> <Leader>T :TagbarToggle<CR>
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
