"" Vim settings
"" Updated: Feb 15 2016


""""""""""""""""""""""""""""""""""""""""""""""""
"" Sections:
"" 0. Vi Compatibility Settings
"" 1. Runtime Path Settings
"" 2. User Interface Settings
"" 3. Text Formatting
"" 4. Search and Replace
"" 5. Filetype Specific Settings
"" 6. Settings for Vim Scripts
"" 7. Keymappings
"" 8. Other Settings
"" 9. Function Definitions


"""""""""""""""""""""""""""""""""
"" 0. Vi Compatibility Settings

"" Turn off compatibility with old vi
set nocompatible


"""""""""""""""""""""""""""""
"" 1. Runtime Path Settings

"" Use pathogen if it is loaded to add the paths to plugins in .vim/bundle/* to
"" runtime path and update vim help documentation as needed
"" See: http://www.vim.org/scripts/script.php?script_id=2332

if exists('*pathogen#infect')
  call pathogen#infect()
endif

if exists('*pathogen#helptags')
  call pathogen#helptags()
endif


"""""""""""""""""""""""""""""""
"" 2. User Interface Settings

"" Unset g:colors_name to avoid loading colorscheme several times
"" when sourcing .vimrc a second time.
silent! unset g:colors_name

"" Watch for file changes by other programs
"set autoread

"" Smoother output
set ttyfast

"" Abandoned buffers are not unloaded, but instead become hidden.
"set hidden

"" Status and command line information
if has('cmdline_info')
  "" Show ruler (line & column numbers) in status line,
  "" superceded by 'statusline' if it is supported and defined
  set ruler

  "" Show partial command in status line
  set showcmd
endif

"" Informative status line
"" For more info, see :help statusline
if has('statusline') && (version >= 700)
  set statusline=               "" clear the statusline
  set statusline+=%<            "" truncate if too long
  set statusline+=%F            "" filename
  set statusline+=\             "" spacer
  set statusline+=%h            "" help buffer flag
  set statusline+=%m            "" modified flag
  set statusline+=%r            "" readonly flag
  set statusline+=%w            "" preview window flag
  set statusline+=%=            "" seperation point between left and right aligned items
  set statusline+=%y            "" filetype
  set statusline+=\ \           "" spacer
  set statusline+=[%{&ff}]      "" file format
  set statusline+=\ \           "" spacer
  "" show '[list]' if in list mode
  set statusline+=%{!&list?'':'[list]\ \ '}
  "" show patchmode if enabled
  set statusline+=%{&pm==''?'':'[PM='.&pm.']\ \ '}
  "" show file encoding followed by comma and B if 'bomb' is functional
  "" and has value 1
  set statusline+=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}
  set statusline+=%k            "" Value of 'b:keymap_name' of 'keymap' when :lmap mappings are being used
  set statusline+=\ \           "" spacer
  "" show position (line, column and virtual column number) of cursor
  set statusline+=%-14.(%l,%c%V%)
  set statusline+=\             "" spacer
  set statusline+=%P            "" Percentage through file of displayed window
endif

"" Show which mode we're in
set showmode

"" Always show status line, even when editing just one file
set laststatus=2

"" Show matching brackets
set showmatch

"" Have command-line completion <Tab> (for filenames, help topics, option
"" names). First list the available options and complete the longest common 
"" part, then have further <Tab>s cycle through the possibilities
"" There are two options, either setting wildmenu, or wildmode, or both.
"" Setting wildmenu requires Vim to be compiled with the +wildmenu feature.
"" Setting wildmode has no requirements.
"" wildmenu is better for small numbers of matching, wildmode for large
"if has('wildmenu')
"  set wildmenu
"endif
set wildmode=list:longest,full

"" Improve completion menu behaviour, so the menu of completions
"" brought up by CTRL-n and CTRL-p in insert mode will remain as one types
"" further characters, with matches being refined as more text is typed
if has('insert_expand')
  set completeopt=longest,menuone,preview
endif

"" Do lots of scanning on keyword completion
"set complete=.,w,b,u,U,t,i,d

"" Allow usage of dictionaries for completion
set complete+=k

"" Adds : as a keyword. Useful for <C-n> completions like fig:... in LaTeX
set iskeyword+=:

"" Mouse settings
if has('mouse')
  "" Have the mouse enabled all the time:
  "set mouse=a

  "" Hide the mouse pointer while typing. GUI only
  "set mousehide
endif

"" Automatically switch current working directory to current working file's.
"" Note: Some scripts need autochdir to be unset, so unset as needed, e.g.:
""   vim-latex : add 'set noautochdir' to ~.vim/ftplugin/tex.vim
"if has('autochdir')
"  set autochdir
"endif

"" Start scrolling when 2 lines from top or bottom of the window
set scrolloff=2

"" Start scrolling when 2 lines from left or right of the window
set sidescrolloff=2

"" Turn on syntax highlighting in terminals that can display colors
"" Enable a color scheme if colors are supported
if has('syntax') && (&t_Co > 2)
  syntax on
  "" 256-color colorscheme
  let g:colSch256 = 'zenburn'
  "" 88-color colorscheme
  let g:colSch88 = 'wombat256'
  "" 8-color colorscheme
  let g:colSch8 = 'default'
  "" Set the 256-color colorscheme only if the defined scheme exists,
  "" at least 256 colors are supported, and if screen is not running
  "" or if screen is running and supports 256 colors
  "" (screen only supports 16 colors out of the box, but there is a
  "" compile time flag to enable 256 color support)
  "" Note: When screen is running, the TERMCAP environment variable will
  "" be set and will begin with 'SC'
  if !empty(matchstr(globpath(&runtimepath,'colors/*.vim'), substitute(g:colSch256,g:colSch256,'/\0\\\.','')))
        \ && (&t_Co>=256) && (($TERMCAP !~ '^SC') || !empty(matchstr($TERM,'screen-256color')) )
    silent! execute 'colorscheme' g:colSch256
  endif
  "" Try setting the 88-color colorscheme if 256-color one has not been set
  if !exists("colors_name")
    if !empty(matchstr(globpath(&runtimepath,'colors/*.vim'), substitute(g:colSch88,g:colSch88,'/\0\\\.','')))
          \ && (&t_Co>=88) && (($TERMCAP !~ '^SC') || !empty(matchstr($TERM,'screen-256color')) )
      silent! execute 'colorscheme' g:colSch88
    endif
  endif
  "" Otherwise set the 8-color colorscheme if no other one has been set
  if !exists("colors_name")
    silent! execute 'colorscheme' g:colSch8
  endif
  unlet! g:colSch256
  unlet! g:colSch88
  unlet! g:colSch8
endif


"""""""""""""""""""""""
"" 3. Text Formatting

"" Uncomment the following to prevent Vim automatically writing
"" an <EOL> at the end of a file it does not end in one
"" Setting this option automatically resets textwidth, wrapmargin,
"" modeline and expandtab
"set binary noeol

"" Do not have files override settings from this .vimrc:
"set nomodeline

"" Do not wrap text
set nowrap

"" Visually indicate wrapped lines
set showbreak=...\  " put '... ' at start of each continued line

"" Do not insert two spaces after '.', '?' and '!'
"" when performing line joins
set nojoinspaces

"" Set how 'cindent' reindents lines in a C program
"" Here, regular indentation set to the value of shiftwidth
if has('cindent')
  set cinoptions=>1s
endif

"" Number of spaces to use for each step of (auto)indent,
"" used for 'cindent', >> , << , etc
set shiftwidth=2

"" sets the length of a real tab
"set tabstop=8

"" Number of spaces a tab counts for while performing editing operations.
"" If expandtab is not set, these spaces get converted into a
"" minimal sequence of tabs followed by spaces.
"" To use <Tab>s to handle space in front of lines for proper indentation
"" (e.g. using shiftwidth), look at the 'smarttab' option.
"set softtabstop=4

"" <Tab>s in front of a line inserts 'shiftwidth' space.
"" If expandtab is not set, the space at the start of a line gets
"" converted into a minimal sequence of tabs followed by spaces.
set smarttab

"" Expand <Tab>s into spaces in insert mode. Use CTRL-v<TAB> for a real tab.
set expandtab

"" Make vim perform indentation
"" Uses the indent level from the previous line
set autoindent
"" Evaluates an expression to compute the indent of a line
"set indentexpr='(getline(v:lnum)=~"^\\s*{" && getline(v:lnum-1)=~"^\\s*throws\\s")? cindent(v:lnum)-&sw : cindent(v:lnum)'

"" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

"" Highlight tabs and trailing whitespace
set list
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:.
"" If the character set on the system supports it, it would be better to use
"" the right double-chevron and mid-dots. To do this, uncomment the commands
"" that follow.  (Character 187 is a right double-chevron, 183 a mid-dot.)
" execute 'set listchars+=tab:' . nr2char(187) . nr2char(183)
" execute 'set listchars+=trail:' . nr2char(183)
" execute 'set listchars+=nbsp:' . nr2char(183)


""""""""""""""""""""""""""
"" 4. Search and Replace

"" Make searches case-insensitive, unless they contain upper-case letters
set ignorecase
set smartcase

if has('extra_search')
  "" Show the 'best match so far' as search strings are being typed
  set incsearch
endif

"" Assume the /g flag is on :s substitutions to replace all matches in a line
set gdefault


""""""""""""""""""""""""""""""""""
"" 5. Filetype Specific Settings

if has('autocmd')
  "" Enable filetype detection
  filetype on

  "" Load plugin files for specific file types
  filetype plugin on

  "" Load the indent file for specific file types
  filetype indent on

  "" Transparent editing of gpg binary and ascii armor encrypted files.
  "" By Wouter Hanegraaff <wouter@blub.net>
  augroup encrypted
    autocmd!
    "" First make sure nothing is written to ~/.viminfo while editing
    "" an encrypted file.
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set viminfo=
    "" We don't want a swap file, as it writes unencrypted data to disk
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set noswapfile
    "" Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set bin
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let shsave=&sh
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let &sh='sh'
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg '[,']!gpg --decrypt --default-recipient-self 2> /dev/null
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &sh=shsave
    "" Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg set nobin
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg execute ":doautocmd BufReadPost " . expand("%:r")
    "" Convert all text to encrypted text before writing
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg set bin
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let shsave=&sh
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh='sh'
    autocmd BufWritePre,FileWritePre    *.gpg '[,']!gpg --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc '[,']!gpg --armor --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh=shsave
    "" Undo the encryption so we are back in the normal text, directly
    "" after the file has been written.
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg silent u
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg set nobin
  augroup END

  augroup programming
    autocmd!
    "" Display line numbers
    autocmd FileType c,cpp,java,javascript,make,perl,python,slang set number

    "" For most programming languages except those where whitespace/tabs are
    "" important, use only tab characters for block indentation.
    "" Recommended coding convention: use tabs for the block indentation,
    "" then use spaces for any additional indentation that is internal to the
    "" block. That preserves code readability when using different tab stops.
    "autocmd FileType c,cpp,java,javascript,perl,slang set noexpandtab shiftwidth=8 tabstop=8
    autocmd FileType c,cpp,java,javascript,perl,slang set expandtab shiftwidth=2 tabstop=2

    "" For C-like programming, have automatic indentation:
    autocmd FileType c,cpp,slang if has('cindent') | set cindent | endif

    "" For actual C (not C++) programming where comments have explicit end
    "" characters, if starting a new line in the middle of a comment, automatically
    "" insert the comment leader characters
    autocmd FileType c set formatoptions+=ro

    "" For Perl programming, have things in braces indenting themselves:
    autocmd FileType perl if has('smartindent') | set smartindent | endif

    "" For Python programming, expand tabs to 4 spaces
    "" since whitespace is important (4 spaces per indentation level)
    autocmd Filetype python set tabstop=4 shiftwidth=4 expandtab
    "" For Python programming, use cindent with the appropriate keywords
    autocmd Filetype python if has('cindent') && has('smartindent') | set cindent
          \ cinwords=class,def,elif,else,except,finally,for,if,try,while
          \ | endif

    "" In Makefiles, do not expand tabs to spaces, since actual tab characters
    "" are needed, and have indentation at 8 chars to be sure that all
    "" indents are tabs (despite the mappings later):
    autocmd FileType make set noexpandtab nosmarttab shiftwidth=8 tabstop=8

    "" Omnifunc completions
    autocmd FileType c      if has('eval') || has('insert_expand') | set omnifunc=ccomplete#Complete | endif
    autocmd FileType javascript if has('eval') || has('insert_expand') | set omnifunc=javascriptcomplete#CompleteJS | endif
    autocmd FileType php    if has('eval') || has('insert_expand') | set omnifunc=phpcomplete#CompletePHP | endif
    autocmd FileType python if has('eval') || has('insert_expand') | set omnifunc=pythoncomplete#Complete | endif
    autocmd FileType ruby   if has('eval') || has('insert_expand') | set omnifunc=rubycomplete#Complete | endif
    autocmd FileType sql    if has('eval') || has('insert_expand') | set omnifunc=sqlcomplete#Complete | endif
    autocmd FileType xml    if has('eval') || has('insert_expand') | set omnifunc=xmlcomplete#CompleteTags | endif
  augroup END

  augroup web
    autocmd!
    "" For both CSS and HTML, display line numbers
    autocmd FileType css,html,xhtml set number

    "" For HTML, do not have Vim automatically add a <CR> or line break
    "" at the end of the last line if there isn't one, otherwise
    "" the default http headers will be sent
    autocmd FileType html,xhtml set binary noeol

    "" For both CSS and HTML, use genuine tab characters for
    "" indentation to make files a few bytes smaller:
    autocmd FileType css,html,xhtml set noexpandtab shiftwidth=2 tabstop=2

    "" For CSS, also have things in braces indented:
    autocmd Filetype css if has('smartindent') | set smartindent | endif

    "" For HTML, generally format text, but if a long line has been created,
    "" leave it alone when editing
    autocmd FileType html,xhtml set formatoptions+=tl

    "" Omnifunc completions
    autocmd FileType css if has('eval') || has('insert_expand') | set omnifunc=csscomplete#CompleteCSS | endif
    autocmd FileType html,xhtml if has('eval') || has('insert_expand') | set omnifunc=htmlcomplete#CompleteTags | endif
  augroup END

  augroup mail
    autocmd!
    autocmd Filetype mail if has('syntax') | set spell textwidth=70 wrap nonumber | endif
  augroup END

  "" Note: The autocommand event is defined to avoid setting this augroup when in the
  "" NERD tree buffer, which causes a conflict with its color settings.
  augroup highlightextrawhitespace
    autocmd!
    "" Highlight extra white space:
    autocmd Syntax *[^{nerdtree}]* highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
    "" Show trailing whitepace and spaces before a tab:
    autocmd Syntax *[^{nerdtree}]* if has('syntax') | syntax match ExtraWhitespace /\s\+$\| \+\ze\t/ | endif
    "" Show tabs that are not at the start of a line:
    "autocmd Syntax *[^{nerdtree}]* syntax match ExtraWhitespace /[^\t]\zs\t\+/
    "" Show spaces used for indenting (so you use only tabs for indenting).
    "autocmd Syntax * syntax match ExtraWhitespace /^\t*\zs \+/
  augroup END
endif


""""""""""""""""""""""""""""""""""""""
"" 6. Compiler and External Commands

"" IMPORTANT: grep will sometimes skip displaying the file name if you
"" search in a single file. Set grep program to alway generate a file-name
set grepprg=grep\ -nH\ $*


"""""""""""""""""""
"" 7. Keymappings

"" Default map leader <Leader> is backslash '\'

"" Write file as root/superuser using :Sudow (requires sudo be present on the
"" system and that the action is permitted the user in the /etc/sudoers file)
if executable('sudo')
  command! -nargs=0 -bang -bar Sudow w<bang> !sudo tee % >/dev/null
endif

"" Buffer manipulation and navigation
nnoremap <silent> <Leader>bl :buffers<CR>
nnoremap <silent> <Leader>bd :bdelete<CR>
nnoremap <silent> <Leader>bn :bnext<CR>
nnoremap <silent> <Leader>bp :bprevious<CR>
nnoremap <Leader>bg :buffer!<Space>
nnoremap <Leader>bad :badd<Space>
nnoremap <silent> <Leader>bal :ball<CR>

"" Tab manipulation and navigation
if has('windows')
  nnoremap <silent> <Leader>tl :tabs<CR>
  nnoremap <silent> <Leader>te :tabedit<CR>
  nnoremap <silent> <Leader>tc :tabclose<CR>
  nnoremap <silent> <Leader>tn :tabnext<CR>
  nnoremap <silent> <Leader>tp :tabprevious<CR>
  nnoremap <silent> <Leader>tf :tabfirst<CR>
  nnoremap <silent> <Leader>tl :tablast<CR>
  nnoremap <silent> <Leader>t1 :tabnext 1<CR>
  nnoremap <silent> <Leader>t2 :tabnext 2<CR>
  nnoremap <silent> <Leader>t3 :tabnext 3<CR>
  nnoremap <silent> <Leader>t4 :tabnext 4<CR>
  nnoremap <silent> <Leader>t5 :tabnext 5<CR>
  nnoremap <silent> <Leader>t6 :tabnext 6<CR>
  nnoremap <silent> <Leader>t7 :tabnext 7<CR>
  nnoremap <silent> <Leader>t8 :tabnext 8<CR>
  nnoremap <silent> <Leader>t9 :tabnext 9<CR>
  nnoremap <silent> <Leader>t0 :tabnext 10<CR>
  nnoremap <Leader>tg :tabnext<Space>
  nnoremap <Leader>tm :tabmove<Space>
  nnoremap <Leader>td :tabdo<Space>
  "" Resize tab to fit number of lines in buffer
  nnoremap <Leader>ts :execute ":resize " . line('$')<CR>
endif

"" Quickfix error window commands
nnoremap <silent> <Leader>qo :copen
nnoremap <silent> <Leader>qc :cclose

"" Toggle spell check
if has('syntax')
  nnoremap <silent> <Leader>sp :set spell! spell?<CR>
endif

"" Toggle wrapping of lines
nnoremap <silent> <Leader>w :set wrap! wrap?<CR>

"" Unhighlight search results
nnoremap <silent> <Leader>h :nohlsearch<CR>

"" Set paste/nopaste
nnoremap <silent> <Leader>p :set paste! paste?<CR>

"" Toggle highlighting of listchars
nnoremap <silent> <Leader>l :set list! list?<CR>

"" Toggle modeline (file should be reloaded using :e after that)
nnoremap <silent> <Leader>m :set modeline! modeline?<CR>

"" Change directory to that of file being edited
nnoremap <silent> <Leader>cd :cd %:p:h<CR>:pwd<CR>

"" Toggle NERD tree plugin directory listing (if the plugin is loaded)
nnoremap <silent> <Leader>n :if exists('loaded_nerd_tree') <Bar> :NERDTreeToggle <Bar> else <Bar> echo 'NERD tree plugin not loaded' <Bar> endif<CR>


""""""""""""""""""""""
"" 8. Other Settings

" Sets the terminal emulator's title according to the file being edited
set title


""""""""""""""""""""""""""""
"" 9. Function Definitions
