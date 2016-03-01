"" Vim settings
"" Updated: Feb 29 2016


"""""""""""""""""""""""""""""""""
" Sections:
" 0. Vi Compatibility
" 1. Plugins
" 2. User Interface
" 3. Text Formatting
" 4. Search and Replace
" 5. Filetype Specific Settings
" 6. Settings for Vim Scripts
" 7. Keymappings
" 8. Other Settings
" 9. Commands and Functions


"""""""""""""""""""""""
" 0. Vi Compatibility

" Turn off compatibility with old vi
set nocompatible


""""""""""""""
" 1. Plugins

" Use Vundle if available, otherwise try using Pathogen
if filereadable(expand('~/.vim/bundle/Vundle.vim/README.md')) " Vundle
  " Preamble
  filetype off
  set runtimepath+=~/.vim/bundle/Vundle.vim
  call vundle#begin()
  Plugin 'VundleVim/Vundle.vim'  " let Vundle manage Vundle, required
  " Examples:
  " 1. plugin on GitHub repo
  "    Plugin 'tpop/vim-fugitive'
  " 2. plugin from http://vim-scripts.org/vim/scripts.html
  "    Plugin 'L9'
  " 3. Git plugin not hosted on GitHub
  "    Plugin 'git://git.wincent.com/command-t.git'
  " 4. git repos on your local machine (i.e. when working on your own plugin)
  "    Plugin 'file:///home/gmarik/path/to/plugin'
  " 5. The sparkup vim script is in a subdirectory of this repo called vim.
  "    Pass path to set the runtimepath properly.
  "    Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
  " 6. Install L9 and avoid a Naming conflict if you've already installed a
  "    different version somewhere else.
  "    Plugin 'ascenator/L9', {'name': 'newL9'}
  " Define plugins to use here
  Plugin 'jnurmine/Zenburn'     " color scheme
  Plugin 'majutsushi/tagbar'    " tag browser
  "Plugin 'SirVer/ultisnips'     " snippets engine
  "Plugin 'honza/vim-snippets'   " snippets files
  " Postamble
  call vundle#end()
  filetype on
else " Pathogen
  " See: http://www.vim.org/scripts/script.php?script_id=2332
  if exists('*pathogen#infect')
    call pathogen#infect()
    call pathogen#helptags()
  endif
endif

" Set plugin-specific settings here


""""""""""""""""""""""""""""""
" 2. User Interface Settings

" Unset g:colors_name to avoid loading colorscheme several times
" when sourcing .vimrc a second time
silent! unset g:colors_name

" Watch for file changes by other programs
"set autoread

" Smoother output
set ttyfast

" Abandoned buffers are not unloaded, but instead become hidden.
"set hidden

" Status and command line information
if has('cmdline_info')
  " Show ruler (line & column numbers) in status line,
  " superceded by 'statusline' if it is supported and defined
  set ruler

  " Show partial command in status line
  set showcmd
endif

" Informative status line
" For more info, see :help statusline
if has('statusline') && (version >= 700)
  set statusline=               " clear the statusline
  set statusline+=%<            " truncate if too long
  set statusline+=%F            " filename
  set statusline+=\             " spacer
  set statusline+=%h            " help buffer flag
  set statusline+=%m            " modified flag
  set statusline+=%r            " readonly flag
  set statusline+=%w            " preview window flag
  set statusline+=%=            " seperation point between left and right aligned items
  set statusline+=%y            " filetype
  set statusline+=\ \           " spacer
  set statusline+=[%{&ff}]      " file format
  set statusline+=\ \           " spacer
  " Show '[list]' if in list mode
  set statusline+=%{!&list?'':'[list]\ \ '}
  " Show patchmode if enabled
  set statusline+=%{&pm==''?'':'[PM='.&pm.']\ \ '}
  " Show file encoding followed by comma and B if 'bomb' is functional and has value 1
  set statusline+=%{\"[\".(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\").\"]\ \"}
  set statusline+=%k            " Value of 'b:keymap_name' of 'keymap' when :lmap mappings are being used
  set statusline+=\ \           " spacer
  " Show position (line, column and virtual column number) of cursor
  set statusline+=%-14.(%l,%c%V%)
  set statusline+=\             " spacer
  set statusline+=%P            " Percentage through file of displayed window
endif

" Show which mode we're in
set showmode

" Always show status line, even when editing just one file
set laststatus=2

" Show matching brackets
set showmatch

" Have command-line <Tab> (or 'wildchar') completion (for filenames, help
" topics, option names). The first <Tab> lists the matches and completes
" until the longest common string. Further <Tab>s cycle through the matches
set wildmode=list:longest,full

" Have Insert mode completion (CTRL-n and CTRL-p in Insert mode) show a menu,
" insert until the longest common string from the matches and show extra info
" about the currently selected completion
if has('insert_expand')
  set completeopt=longest,menuone,preview
endif

" Do lots of scanning on keyword completion
"set complete=.,w,b,u,U,t,i,d

" Allow usage of dictionaries for completion
set complete+=k

" Automatically switch current working directory to current working file's.
" Note: Some scripts need autochdir to be unset, so unset as needed, e.g.:
"   vim-latex : add 'set noautochdir' to ~.vim/ftplugin/tex.vim
"if has('autochdir')
"  set autochdir
"endif

" Start scrolling when 1 line from top or bottom of the window
"set scrolloff=1

" Start scrolling when 5 lines from left or right of the window
set sidescrolloff=5

" Set folding to group lines by indentation (but turn off folding by default)
if has('folding')
  set foldmethod=indent
  set nofoldenable
endif

" Syntax highlighting colorscheme, list of (colors_name[, numcolors needed])
let s:colorList = [['zenburn', 256], ['default', 8]]
" Turn on syntax highlighting in terminals that can display colors
if has('syntax') && (&t_Co > 2)
  syntax on
  " In list order, try setting color scheme if the terminal supports at least
  " that number of colors (default: 256)
  for colorPair in s:colorList
    if !exists('colors_name') && (&t_Co >= get(colorPair, 1, 256))
      silent! execute 'colorscheme' colorPair[0]
    endif
  endfor
endif
unlet! s:colorList


""""""""""""""""""""""
" 3. Text Formatting

" Uncomment the following to prevent Vim automatically writing
" an <EOL> at the end of a file it does not end in one
" Setting this option automatically resets textwidth, wrapmargin,
" modeline and expandtab
"set binary noeol

" Do not have files override settings from this .vimrc:
"set nomodeline

" Do not wrap text
set nowrap

" Visually indicate wrapped lines
if has('linebreak')
  set showbreak=...\  " put '... ' at start of each continued line
endif

" Delete comment leader when joining comment lines
" (from https://github.com/tpope/vim-sensible)
set formatoptions+=j

" Do not insert two spaces after '.', '?' and '!'
" when performing line joins
set nojoinspaces

" Set how 'cindent' reindents lines in a C program
" Here, regular indentation set to the value of shiftwidth
if has('cindent')
  set cinoptions=>1s
endif

" Number of spaces to use for each step of (auto)indent,
" used for 'cindent', >> , << , etc
set shiftwidth=2

" Length of a real tab
"set tabstop=8

" Number of spaces a tab counts for while performing editing operations.
" If expandtab is not set, these spaces get converted into a
" minimal sequence of tabs followed by spaces.
" To use <Tab>s to handle space in front of lines for proper indentation
" (e.g. using shiftwidth), look at the 'smarttab' option.
"set softtabstop=4

" <Tab>s in front of a line inserts 'shiftwidth' space.
" If expandtab is not set, the space at the start of a line gets
" converted into a minimal sequence of tabs followed by spaces.
set smarttab

" Expand <Tab>s into spaces in insert mode. Use CTRL-v<TAB> for a real tab.
set expandtab

" Use the indent level from the previous line
set autoindent

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Highlight tabs and trailing whitespace
set list
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:.
" If the system character set supports it, it is better to use the right
" double-chevron and mid-dots. To do so, uncomment the following commands
" (Character 187 is a right double-chevron, 183 a mid-dot)
"execute 'set listchars+=tab:' . nr2char(187) . nr2char(183)
"execute 'set listchars+=trail:' . nr2char(183)
"execute 'set listchars+=nbsp:' . nr2char(183)


"""""""""""""""""""""""""
" 4. Search and Replace

" Make searches case-insensitive, unless they contain upper-case letters
set ignorecase
set smartcase

" Show the best match so far as search strings are being typed
if has('extra_search')
  set incsearch
endif

" Assume the /g flag is on :s substitutions to replace all matches in a line
set gdefault


"""""""""""""""""""""""""""""""""
" 5. Filetype Specific Settings

if has('autocmd')
  " Enable filetype detection
  filetype on

  " Load plugin files for specific file types
  filetype plugin on

  " Load the indent file for specific file types
  filetype indent on

  " Transparent editing of gpg binary and ascii armor encrypted files.
  " By Wouter Hanegraaff <wouter@blub.net>
  augroup encrypted
    autocmd!
    " First make sure nothing is written to ~/.viminfo while editing an encrypted file.
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set viminfo=
    " We don't want a swap file, as it writes unencrypted data to disk
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set noswapfile
    " Switch to binary mode to read the encrypted file
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg set bin
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let shsave=&sh
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let &sh='sh'
    autocmd BufReadPre,FileReadPre      *.asc,*.gpg let ch_save = &ch|set ch=2
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg '[,']!gpg --decrypt --default-recipient-self 2> /dev/null
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &sh=shsave
    " Switch to normal mode for editing
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg set nobin
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg let &ch = ch_save|unlet ch_save
    autocmd BufReadPost,FileReadPost    *.asc,*.gpg execute ":doautocmd BufReadPost " . expand("%:r")
    " Convert all text to encrypted text before writing
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg set bin
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let shsave=&sh
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh='sh'
    autocmd BufWritePre,FileWritePre    *.gpg '[,']!gpg --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc '[,']!gpg --armor --encrypt --default-recipient-self 2>/dev/null
    autocmd BufWritePre,FileWritePre    *.asc,*.gpg let &sh=shsave
    " Undo the encryption so we are back in the normal text, directly after the file has been written.
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg silent u
    autocmd BufWritePost,FileWritePost  *.asc,*.gpg set nobin
  augroup END

  augroup mail
    autocmd!
    autocmd Filetype mail if has('syntax') | set spell textwidth=70 wrap nonumber | endif
  augroup END

  augroup programming
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

    " For Haskell programming, expand tabs to 4 spaces and set 4 space indents
    autocmd Filetype haskell set tabstop=4 shiftwidth=4 expandtab

    " In Makefiles, do not expand tabs to spaces, since actual tab characters
    " are needed, and have indentation at 8 chars to be sure that all
    " indents are tabs (despite the mappings later):
    autocmd FileType make set noexpandtab nosmarttab shiftwidth=8 tabstop=8

    " For Perl programming, have things in braces indenting themselves:
    autocmd FileType perl if has('smartindent') | set smartindent | endif

    " For Python programming, expand tabs to 4 spaces and set 4 space indents
    autocmd Filetype python set tabstop=4 shiftwidth=4 expandtab
    " For Python programming, use cindent with the appropriate keywords
    autocmd Filetype python
          \ if has('cindent')
          \ | set cindent cinwords=class,def,elif,else,except,finally,for,if,try,while
          \ | endif
  augroup END

  augroup web
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
  augroup END

  " Note: The autocommand events are defined to avoid setting this augroup when
  " in the NERD tree buffer, which causes a conflict with its color settings.
  augroup highlightextrawhitespace
    autocmd!
    " Highlight extra white space
    autocmd Syntax *[^{nerdtree}]* highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
    " Show trailing whitepace and spaces before a tab
    autocmd Syntax *[^{nerdtree}]* if has('syntax') | syntax match ExtraWhitespace /\s\+$\| \+\ze\t/ | endif
    " Show tabs that are not at the start of a line
    "autocmd Syntax *[^{nerdtree}]* syntax match ExtraWhitespace /[^\t]\zs\t\+/
    " Show spaces used for indenting (so you use only tabs for indenting)
    "autocmd Syntax * syntax match ExtraWhitespace /^\t*\zs \+/
  augroup END
endif


"""""""""""""""""""""""""""""""""""""
" 6. Compiler and External Commands

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a single file. Set grep program to alway generate a file-name
set grepprg=grep\ -nH\ $*


""""""""""""""""""
" 7. Keymappings

" Default map leader <Leader> is backslash '\'

" Map <Space> to the map leader in normal mode
if has('eval')
  nmap <Space> <Leader>
endif

" Buffer manipulation and navigation
nnoremap <silent> <Leader>bl :buffers<CR>
nnoremap <silent> <Leader>bd :bdelete<CR>
nnoremap <silent> <Leader>bn :bnext<CR>
nnoremap <silent> <Leader>bp :bprevious<CR>
nnoremap <Leader>bg :buffer!<Space>
nnoremap <Leader>bad :badd<Space>
nnoremap <silent> <Leader>bal :ball<CR>

" Tab and window manipulation and navigation
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

" Quickfix error window commands
if has('quickfix')
  nnoremap <silent> <Leader>qo :copen<CR>
  nnoremap <silent> <Leader>qc :cclose<CR>
endif

" Unhighlight search results (from https://github.com/tpope/vim-sensible)
if has('extra_search')
  nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
endif

" Change directory to that of file being edited
nnoremap <silent> <Leader>cd :cd %:p:h<CR>:pwd<CR>

" Toggle folding
if has('folding')
  nnoremap <silent> <Leader>f :set foldenable! foldenable?<CR>
endif

" Toggle spell check
if has('syntax')
  nnoremap <silent> <Leader>sp :set spell! spell?<CR>
endif

" Toggle wrapping of lines
nnoremap <silent> <Leader>W :set wrap! wrap?<CR>

" Toggle paste mode
nnoremap <silent> <Leader>P :set paste! paste?<CR>

" Toggle highlighting of listchars
nnoremap <silent> <Leader>L :set list! list?<CR>

" Toggle modeline (file should be reloaded using :e after that)
nnoremap <silent> <Leader>M :set modeline! modeline?<CR>

" Toggle Tagbar window (requires Tagbar plugin)
nnoremap <silent> <Leader>T :TagbarToggle<CR>


"""""""""""""""""""""
" 8. Other Settings

" Sets the terminal emulator's title to path of file being edited
if has('title')
  set title
endif


"""""""""""""""""""""""""""""
" 9. Commands and Functions

" Write file as root/superuser using :Sudow (requires sudo be present on the
" system and that the action is permitted the user in the /etc/sudoers file)
if executable('sudo')
  command! -nargs=0 -bang -bar Sudow w<bang> !sudo tee % >/dev/null
endif

" vim:set ft=vim et sw=2:
