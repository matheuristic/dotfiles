" ~/.vimrc - Config file for Vim
" Author: matheuristic

" Section: Vi Compatibility {{{1
" ------------------------------

if &compatible | finish | endif " don't source script if vi compatible-mode is set
"set nocompatible " vi non-compatible mode, usually set automatically

" }}}1
" Section: Options {{{1
" ---------------------

set autoindent  " use indent level from the previous line
"set autoread    " watch for file changes made by external programs
set backspace=indent,eol,start " allow backspacing over everything in insert mode
"set backup      " keep backups (it is usually better to use version control)
"set backupdir=~/.vimfiles/backup//,.,~/tmp/,~/ " backup file folders, appending '//' uses the full path in the filename
"set directory=~/.vimfiles/swap//,.,~/tmp,/var/tmp,/tmp " swapfile folders, appending '//' uses the full path in the filename
"set binary noeol " do not autowrite <EOL> at end of file, resets 'textwidth', 'wrapmargin', 'modeline' and 'expandtab'
"set complete=.,w,b,u,U,t,i,d " extra scanning on keyword completion
set complete+=k " also use dictionaries on keyword completion
set expandtab   " expand tabs into spaces (use <C-v><Tab> in insert mode for a real tab)
set formatoptions+=j " delete comment leader when joining comment lines
set formatoptions+=mM " correctly break multibyte characters like CJK ones
"set gdefault    " substitute all matches in a string by default
set hidden      " hide buffers instead of unloading them, which allows unsaved buffers to be pushed to the background
"set ignorecase  " make searches case-insensitive
set nojoinspaces " do not insert two spaces after '.', '?' and '!' on line joins
set laststatus=2 " always show status line, even when editing just one file
set lazyredraw  " don't redraw screen when executing macros and registers
"set list        " highlight tabs and trailing whitespace
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:. " chars for displaying whitespace when 'list' is set
"set nomodeline  " do not have files overwrite settings from this vimrc
"set scrolloff=1 " num lines from top or bottom of window to begin scrolling
set sidescrolloff=5 " num lines from left or right of window to begin scrolling
set shiftwidth=2 " number of spaces for each indent level
"set showmatch   " show matching brackets
set showmode    " show current mode
set smartcase   " override 'ignorecase' if search pattern has upper case chars, only used when 'ignorecase' is set
set smarttab    " tabs inserts shiftwidth space
"set softtabstop=4 " num spaces a tab counts for while in insert mode
"set swapfile    " use swapfiles, swapfile location is determined by 'directory'
"set tabstop=8   " length of a real tab
"set ttyfast     " smoother output
set nowildmenu   " don't use enhanced command line completion
set wildmode=list:longest,full " command-line tab completion options
"set nowrap      " do not wrap text

" Prefer rg (ripgrep) to grep for grepping {{{2
if executable('rg')
  set grepprg=rg\ --vimgrep
else
  set grepprg=grep\ -nH\ $* " set grep to always show filename
endif " }}}2
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
" Show matches while inputting search string {{{2
if has('extra_search')
  "set hlsearch
  set incsearch
endif
" }}}2
" Enable folding {{{2
if has('folding')
  set foldenable
  set foldmethod=marker " Use markers to specify folds
endif
" }}}2
" Insert mode completion options (<C-n> and <C-p>) {{{2
if has('insert_expand')
  set completeopt=longest,menuone,preview
endif
" }}}2
" Visually indicate wrapped lines {{{2
if has('linebreak')
  set showbreak=...\  " put '... ' at start of each continued line
endif
" }}}2
" Persistent undo {{{2
if has("persistent_undo")
  set undofile
  set undodir=~/.vimfiles/undo//,. " undo file folders, appending // uses the full path in the name
endif
" }}}2
" Status line {{{2
if has('statusline') && (version >= 700)
  set statusline=               " clear statusline
  set statusline+=%<            " truncate if too long
  set statusline+=%f            " filepath relative to current directory
  set statusline+=\             " spacer
  set statusline+=%h            " help buffer flag
  set statusline+=%m            " modified flag
  set statusline+=%r            " readonly flag
  set statusline+=%w            " preview window flag
  set statusline+=%=            " center auto-spacing
  set statusline+=%{&pm==''?'':'[PM='.&pm.']\ '} " show patchmode if enabled
  set statusline+=%{!&list?'':'[list]\ '} " show '[list]' if in list mode
  set statusline+=%y            " filetype
  set statusline+=\             " spacer
  set statusline+=%{(&fenc==\"\"?&enc:&fenc)} " file encoding
  set statusline+=%{(exists(\"+bomb\")\ &&\ &bomb)?\"[BOM]\":\"\"} " show '[BOM]' if 'bomb' option is set
  set statusline+=%{\"[\".(&ff).\"]\"} " file format
  set statusline+=\             " spacer
  set statusline+=%k            " value of 'b:keymap_name' of 'keymap' when :lmap mappings are being used
  set statusline+=\ \           " spacer
  set statusline+=%-14.(%l,%c%V%) " position (line, column and virtual column) of cursor
  set statusline+=\             " spacer
  set statusline+=%P            " percentage through file of displayed window
endif
" }}}2
" Set syntax highlighting colorscheme {{{2
if has('syntax') && !exists('colors_name') && (&t_Co >= 8)
  syntax enable
  colorscheme elflord
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
    autocmd!
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
          \ | set cindent cinwords=class,def,elif,else,except,finally,for,if,try,while,with
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
" Section: Functions {{{1
" -----------------------

" Toggle commenting of lines {{{2
" Adapted from https://gist.github.com/PeterRincker/13bde011c01b7fc188c5
" Uses b:commentstring or 'commentstring' as comment pattern, e.g.
"   let &commentstring = '/*%s*/'
if has('eval')
  function! s:commentOp(...)
    '[,']call s:toggleComment()
  endfunction

  function! s:toggleComment() range
    let comment = substitute(get(b:, 'commentstring', &commentstring), '\s*\(%s\)\s*', '%s', '')
    let pattern = '\V\^\(\s\*\)' . printf(escape(comment, '\'), '\s\{,1\}\(\s\*\.\*\)\s\=')
    let replace = '\1\2'
    " Get min indent in range
    for lnum in range(a:firstline, a:lastline)
      let nextindent = matchstr(getline(lnum), '^\s*')
      if !exists('indent') || (strlen(nextindent) <= strlen(indent))
        let indent = nextindent
      endif
    endfor
    if getline('.') !~ pattern
      let pattern = '^' . indent . '\zs\(\s*\)\(\S.*\)'
      let replace = printf(comment, '\1 \2' . (comment =~ '%s$' ? '' : ' '))
    endif
    for lnum in range(a:firstline, a:lastline)
      call setline(lnum, substitute(getline(lnum), pattern, replace, ''))
    endfor
  endfunction
endif
" }}}2

" }}}1
" Section: Keymappings {{{1
" -------------------------

" Remap jk and kj to <ESC> {{{2
inoremap jk <ESC>
inoremap kj <ESC>
" }}}2

" Remap <Leader> from '\' to <Space> {{{2
let mapleader=' ' " }}}2

" Buffer manipulation and navigation ('b') {{{2
nnoremap <silent> <Leader>bl :buffers<CR>
nnoremap <silent> <Leader>bd :bdelete<CR>
nnoremap <silent> <Leader>bn :bnext<CR>
nnoremap <silent> <Leader>bp :bprevious<CR>
nnoremap <Leader>bg :buffer!<Space>
nnoremap <Leader>bad :badd<Space>
nnoremap <silent> <Leader>bal :ball<CR>
" }}}2

" Tab and window manipulation and navigation ('t' and 'w') {{{2
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
  nnoremap <silent> <Leader>ws :split<CR>
  nnoremap <silent> <Leader>wv :vsplit<CR>
  nnoremap <silent> <Leader>wc :close<CR>
  nnoremap <silent> <Leader>wo :only<CR>
  nnoremap <silent> <Leader>wh <C-w>h
  nnoremap <silent> <Leader>wj <C-w>j
  nnoremap <silent> <Leader>wk <C-w>k
  nnoremap <silent> <Leader>wl <C-w>l
  nnoremap <silent> <Leader>ww <C-w>w
  nnoremap <silent> <Leader>wr <C-w>r
  nnoremap <silent> <Leader>wR <C-w>R
  nnoremap <silent> <Leader>wx <C-w>x
  nnoremap <silent> <Leader>wT <C-w>T
  nnoremap <silent> <Leader>wH <C-w>H
  nnoremap <silent> <Leader>wJ <C-w>J
  nnoremap <silent> <Leader>wK <C-w>K
  nnoremap <silent> <Leader>wL <C-w>L
  nnoremap <silent> <Leader>w+ <C-w>+
  nnoremap <silent> <Leader>w- <C-w>-
  nnoremap <silent> <Leader>w< <C-w><
  nnoremap <silent> <Leader>w> <C-w>>
  nnoremap <silent> <Leader>w= <C-w>=
  " Resize window height to fit number of lines of buffer
  nnoremap <silent> <Leader>wf :execute ":resize " . line('$')<CR>
  " Resize window width to fit max line width of buffer
  nnoremap <silent> <Leader>wF :execute ":vertical resize "
        \ . max(map(range(1, line('$')), "virtcol([v:val, '$'])-1"))<CR>
endif
" }}}2

" Quickfix error and location window manipulation ('q') {{{2
if has('quickfix')
  nnoremap <silent> <Leader>qco :copen<CR>
  nnoremap <silent> <Leader>qcc :cclose<CR>
  nnoremap <silent> <Leader>qlo :lopen<CR>
  nnoremap <silent> <Leader>qlc :lclose<CR>
endif
" }}}2

" Navigate quickfix errors and locations ('[' and ']') {{{2
if has('quickfix')
  nnoremap [q :cprevious<CR>
  nnoremap ]q :cnext<CR>
  nnoremap [Q :cfirst<CR>
  nnoremap ]Q :clast<CR>
  nnoremap [l :lprevious<CR>
  nnoremap ]l :lnext<CR>
  nnoremap [L :lfirst<CR>
  nnoremap ]L :llast<CR>
endif
" }}}2

" Settings ('s') {{{2
" Toggle folding {{{3
if has('folding')
  nnoremap <silent> <Leader>sf :set foldenable! foldenable?<CR>
endif
" }}}3
" Toggle highlighting of listchars {{{3
nnoremap <silent> <Leader>sl :set list! list?<CR>
" }}}3
" Toggle modeline (reload file with :e to effect change) {{{3
nnoremap <silent> <Leader>sm :set modeline! modeline?<CR>
" }}}3
" Toggle line numbers {{{3
nnoremap <silent> <Leader>sn :set number! number?<CR>
" }}}3
" Toggle paste mode {{{3
nnoremap <silent> <Leader>sp :set paste! paste?<CR>
" }}}3
" Toggle spell check {{{3
if has('syntax')
  nnoremap <silent> <Leader>ss :set spell! spell?<CR>
endif
" }}}3
" Toggle line wrap {{{3
nnoremap <silent> <Leader>sw :set wrap! wrap?<CR>
" }}}3
" Toggle syntax highlighting {{{3
if has('syntax')
  nnoremap <silent> <Leader>sy :if exists('g:syntax_on') <Bar> syntax off <Bar>
        \ else <Bar> syntax enable <Bar> endif <CR>
        \:redraw<CR>
        \:echo "syntax enabled=" . string(exists("g:syntax_on"))<CR>
endif
" }}}3
" Toggle whether mouse is enabled {{{3
nnoremap <silent> <Leader>sM :if &mouse == 'a' <Bar> set mouse= <Bar>
      \ else <Bar> set mouse=a <Bar> endif <Bar> set mouse?<CR>
" }}}3
" }}}2

" Other {{{2
" Unhighlight search results (from https://github.com/tpope/vim-sensible) {{{3
" Change directory to current file's {{{3
nnoremap <silent> <Leader>cd :cd %:p:h<CR>:pwd<CR>
" }}}3
if has('extra_search')
  nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
endif
" }}}3
" Toggle commenting using gc[c|motion] like in vim-commentary {{{3
if has('eval')
  nnoremap gcc :<c-u>.,.+<c-r>=v:count<CR>call <SID>toggleComment()<CR>
  nnoremap gc :<c-u>set opfunc=<SID>commentOp<CR>g@
  xnoremap gc :call <SID>toggleComment()<CR>
endif
" }}}3
" }}}2

" }}}1
" Section: Local Vim Config {{{1
" ------------------------------

if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif

" }}}1
" Modeline {{{1
" vim:set ft=vim et sw=2 fen fdc=1 fdm=marker: }}}1
