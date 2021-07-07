" ~/.config/nvim/init.vim - Config file for Neovim
" Author: matheuristic

" Section: Base install requirements {{{1
" ---------------------------------------

" 1. Create the backup, swap and undo directories
"    $ mkdir -p $XDG_DATA_HOME/nvim/backup
"    $ mkdir -p $XDG_DATA_HOME/nvim/swap
"    $ mkdir -p $XDG_DATA_HOME/nvim/undo

" 2. Install ripgrep

" 3. Install the pynvim PyPI package in the Neovim usage environments

" }}}1
" Section: Base configuration {{{1
" --------------------------------

filetype plugin indent on " enable filetype detection for plugins and indents

set nobackup    " no backups by default, usually better to use version control
set backupdir=~/.config/nvim/backup//,.,~/tmp " backup file folders, appending // uses the full path in the name
set completeopt=longest,menuone " triggering autocompletion menu only inserts the longest common text of all matches
set directory=~/.config/nvim/swap//,.,~/tmp,/var/tmp,/tmp " swapfile folders, appending // uses the full path in the name
set expandtab   " expand tabs into spaces; use <C-v><Tab> for a real tab
set foldenable  " enable folding
set foldmethod=marker " use markers to specify folds
set hidden      " hide abandoned buffers instead of unloading them
set inccommand=nosplit " show effects of commands incrementally in the current window
set nojoinspaces " do not insert two spaces after '.', '?' and '!' on line joins
set nolist       " do not highlight tabs and trailing whitespace by default
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:. " chars for displaying whitespace when 'list' is set
set showbreak=…\ " show '… ' at start of each continued line
set swapfile    " use swapfiles
set termguicolors " use GUI colors in the terminal (requires 24-bit color support)
set title " Set the terminal emulator title to path of file being edited
set undofile    " persist undo history to disk
set undodir=~/.config/nvim/undo//,.,~/tmp " undo file folders, appending // uses the full path in the name
set wildmenu    " use enhanced command line completion
set wildmode=longest:full,full " command-line tab completion options, see https://github.com/neovim/neovim/issues/10771#issuecomment-521210434

" Use ripgrep if available {{{2
if executable('rg')
  set grepprg=rg\ --vimgrep
endif " }}}2

" Don't reuse netrw buffers
let g:netrw_fastbrowse = 0

" Disable Python 2 support
let g:loaded_python_provider = 0

" Use Solarized Light colors for the terminal emulator
let g:terminal_color_0  = '#073642' " black          = base02
let g:terminal_color_1  = '#dc322f' " red            = red
let g:terminal_color_2  = '#859900' " green          = green
let g:terminal_color_3  = '#b58900' " yellow         = yellow
let g:terminal_color_4  = '#268bd2' " blue           = blue
let g:terminal_color_5  = '#d33682' " magenta        = magenta
let g:terminal_color_6  = '#2aa198' " cyan           = cyan
let g:terminal_color_7  = '#eee8d5' " white          = base2
let g:terminal_color_8  = '#002b36' " bright black   = base03
let g:terminal_color_9  = '#cb4b16' " bright red     = orange
let g:terminal_color_10 = '#586e75' " bright green   = base01
let g:terminal_color_11 = '#657b83' " bright yellow  = base00
let g:terminal_color_12 = '#839496' " bright blue    = base0
let g:terminal_color_13 = '#6c71c4' " bright magenta = violet
let g:terminal_color_14 = '#93a1a1' " bright cyan    = base1
let g:terminal_color_15 = '#fdf6e3' " bright white   = base3

" When in a Neovim terminal, starting a new Neovim session opens
" buffers in the parent session instead of nesting, adapted from
" https://gist.github.com/nelstrom/ced14300f689bf5ffafac592d3aa9373
function! s:preventNestedNeovim()
  if !empty($NVIM_LISTEN_ADDRESS) && $NVIM_LISTEN_ADDRESS !=# v:servername
    let g:r=sockconnect('pipe', $NVIM_LISTEN_ADDRESS, {'rpc':v:true})
    let g:f=fnameescape(expand('%:p'))
    noautocmd bwipe
    if empty(g:f)
      call rpcrequest(g:r, "nvim_command", "enew ")
    else
      call rpcrequest(g:r, "nvim_command", "edit ".g:f)
    endif
    qall
  endif
endfunction
augroup prevent_nested_neovim
  autocmd!
  autocmd VimEnter * call s:preventNestedNeovim()
augroup END

" }}}1
" Section: Base key mapping {{{1
" ------------------------------

" Remap jk and kj to <ESC> {{{2
inoremap jk <ESC>
inoremap kj <ESC>
" }}}2

" Remap <Leader> from '\' to ',' {{{2
let mapleader=',' " }}}2

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
" }}}2

" Quickfix error and location window manipulation ('q') {{{2
nnoremap <silent> <Leader>qco :copen<CR>
nnoremap <silent> <Leader>qcc :cclose<CR>
nnoremap <silent> <Leader>qlo :lopen<CR>
nnoremap <silent> <Leader>qlc :lclose<CR>
" }}}2

" Settings ('s') {{{2
" Toggle default case-sensitivity of search {{{3
nnoremap <silent> <Leader>sc :set ignorecase! ignorecase?<CR>
" }}}3
" Toggle folding {{{3
nnoremap <silent> <Leader>sf :set foldenable! foldenable?<CR>
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
nnoremap <silent> <Leader>ss :set spell! spell?<CR>
" }}}3
" Toggle wrapping of lines {{{3
nnoremap <silent> <Leader>sw :set wrap! wrap?<CR>
" }}}3
" Toggle syntax highlighting {{{3
function! s:toggleSyntaxHighlighting()
  if exists('g:syntax_on')
    execute "syntax off"
  else
    execute "syntax enable"
  endif
  echo "syntax enabled=" . string(exists("g:syntax_on"))
endfunction
nnoremap <silent> <Leader>sy :call <SID>toggleSyntaxHighlighting()<CR>
" }}}2

" Other {{{2
" Change directory to current file's {{{3
nnoremap <silent> <Leader>cd :cd %:p:h<CR>:pwd<CR>
" }}}3
" Unhighlight search results (from https://github.com/tpope/vim-sensible) {{{3
nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
" }}}3
" Terminal emulator {{{3
nnoremap <silent> <Leader>T :terminal<CR>
" }}}3
" }}}2

" }}}1
" Section: Packages {{{1
" ----------------------

" Using minpac - https://github.com/k-takata/minpac
" To install the minpac package manager, run in the shell:
"   $ git clone https://github.com/k-takata/minpac.git ~/.config/nvim/pack/minpac/opt/minpac
" To install or update registered packages, run in Neovim:
"   :call minpac#update()
" To delete unneeded packages, run in Neovim:
"   :call minpac#clean()

" Many packages also use Neovim's Python3 integration, so it may be necessary
" to install the pynvim PyPI package either using pip:
"   $ pip install pynvim
" or in a conda environment (run Neovim from this environment):
"   $ conda activate someenv
"   $ conda install pynvim
" To verify that Python3 integration is enabled:
"   :echo has("python3")

packadd minpac

if exists('g:loaded_minpac')
  " Initialize minpac
  call minpac#init()

  " minpac must have {'type': 'opt'} so that it can be loaded with `packadd`
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  " 1. Interface {{{2
  call minpac#add('robertmeta/nofrils') " buffer colorscheme {{{3
  let g:nofrils_heavycomments = 0 " low contrast comments
  let g:nofrils_heavylinenumbers = 0 " low contrast line numbers
  let g:nofrils_strbackgrounds = 1 " highlight string backgrounds
  let g:my_nofrils_colorscheme = 2 " 0=acme, 1=dark, 2=light, 3=sepia
  function! s:setNofrilsColorscheme(echo_colorname)
    if g:my_nofrils_colorscheme == 0
      let colorname = "nofrils-acme"
    elseif g:my_nofrils_colorscheme == 1
      let colorname = "nofrils-dark"
    elseif g:my_nofrils_colorscheme == 2
      let colorname = "nofrils-light"
    elseif g:my_nofrils_colorscheme == 3
      let colorname = "nofrils-sepia"
    else
      echoerr "g:my_nofrils_colorscheme should take value from {0,1,2,3}"
    endif
    execute "colorscheme " . colorname
    if a:echo_colorname
      echo colorname
    endif
  endfunction
  function! s:rotateNofrilsColorscheme(reverse_direction)
    if a:reverse_direction
      let incr = 3 " equals to -1 modulo 4
    else
      let incr = 1
    endif
    let g:my_nofrils_colorscheme = (g:my_nofrils_colorscheme + incr) % 4
    call s:setNofrilsColorscheme(v:true)
  endfunction
  nnoremap <silent> <Leader>sCr :call <SID>rotateNofrilsColorscheme(0)<CR>
  nnoremap <silent> <Leader>sCR :call <SID>rotateNofrilsColorscheme(1)<CR>
  nnoremap <silent> <Leader>sC1 :let g:nofrils_heavycomments=!g:nofrils_heavycomments<CR>:NofrilsFocusNormal<CR>:echo "g:nofrils_heavycomments=" . string(g:nofrils_heavycomments)<CR>
  nnoremap <silent> <Leader>sC2 :let g:nofrils_heavylinenumbers=!g:nofrils_heavylinenumbers<CR>:NofrilsFocusNormal<CR>:echo "g:nofrils_heavylinenumbers=" . string(g:nofrils_heavylinenumbers)<CR>
  nnoremap <silent> <Leader>sC3 :let g:nofrils_strbackgrounds=!g:nofrils_strbackgrounds<CR>:NofrilsFocusNormal<CR>:echo "g:nofrils_strbackgrounds=" . string(g:nofrils_strbackgrounds)<CR>
  silent! call <SID>setNofrilsColorscheme(v:false)
  " }}}3
  call minpac#add('vim-airline/vim-airline') " status line {{{3
  call minpac#add('vim-airline/vim-airline-themes') " status line themes
  let g:airline_theme='minimalist' " minimal airline theme
  set noshowmode " don't show --INSERT-- or --VISUAL-- in the command line
  " }}}3
  call minpac#add('liuchengxu/vim-which-key') " display available keybindings in a popup {{{3
  nnoremap <silent> <Leader> :<c-u>WhichKey '<Leader>'<CR>
  " }}}3
  " }}}2
  " 2. Editing {{{2
  call minpac#add('mbbill/undotree') " undo history visualizer {{{3
  nnoremap <silent> <Leader>u :UndotreeToggle<CR>
  " }}}3
  call minpac#add('mg979/vim-visual-multi') " multiple selection like Sublime Text
  call minpac#add('tpope/vim-abolish') " search, substitute and abbreviate word variants
  call minpac#add('tpope/vim-commentary') " toggle commenting of lines
  call minpac#add('tpope/vim-surround') " mappings to manipulate parentheses, brackets and quotes
  call minpac#add('tpope/vim-unimpaired') " handy bracket mappings
  call minpac#add('ervandew/supertab') " tab completion in insert mode {{{3
  let g:SuperTabClosePreviewOnPopupClose = 1 " try to close preview window when the completion popup is closed
  " }}}3
  " }}}2
  " 3. Search {{{2
  call minpac#add('junegunn/fzf', {'do': '!./install --bin'}) " fuzzy file search {{{3
  call minpac#add('junegunn/fzf.vim')
  " Some fzf commands have dependencies on vim packages or system utilities
  "   :Commits  - vim-fugitive
  "   :Rg       - rg (ripgrep) needs to be installed on the system
  "   :Snippets - UltiSnips
  " The fzf preview window colors uses bat by default, so its colors are
  " determined by the selected bat theme on systems with bat installed
  " https://github.com/junegunn/fzf.vim/issues/969#issuecomment-611611154
  " Use a light theme for the fzf preview window as needed
  " https://github.com/junegunn/fzf.vim/issues/969#issuecomment-706137418
  function ToggleBatEnvVar()
    if (&background == "light")
        let $BAT_THEME='Monokai Extended Light'
    else
        let $BAT_THEME=''
    endif
  endfunction
  augroup update_bat_theme
    autocmd!
    autocmd colorscheme * call ToggleBatEnvVar()
  augroup end
  nnoremap <silent> <Leader>fb :Buffers<CR>
  nnoremap <silent> <Leader>fc :Commits<CR>
  nnoremap <silent> <Leader>ff :Files<CR>
  nnoremap <silent> <Leader>fh :History<CR>
  nnoremap <silent> <Leader>fg :Rg<CR>
  nnoremap <silent> <Leader>fl :Lines<CR>
  nnoremap <silent> <Leader>fm :Marks<CR>
  nnoremap <silent> <Leader>fw :Windows<CR>
  nnoremap <silent> <Leader>fH :Helptags<CR>
  nnoremap <silent> <Leader>fM :Maps<CR>
  nnoremap <silent> <Leader>fS :Snippets<CR>
  nnoremap <silent> <Leader>f: :Commands<CR>
  " }}}3
  " }}}2
  " 4. Snippets {{{2
  call minpac#add('SirVer/ultisnips') " snippet manager {{{3
  let g:UltiSnipsExpandTrigger = "<tab>"
  let g:UltiSnipsListSnippets = "<c-tab>"
  let g:UltiSnipsJumpForwardTrigger = "<c-j>"
  let g:UltiSnipsJumpBackwardTrigger = "<c-k>"
  " }}}3
  call minpac#add('honza/vim-snippets') " default snippets
  " }}}2
  " 5. Version control {{{2
  call minpac#add('tpope/vim-fugitive') " Git wrapper {{{3
  nnoremap <silent> <Leader>gd :Gdiff<CR>
  nnoremap <silent> <Leader>gs :Gstatus<CR>
  nnoremap <silent> <Leader>gb :Gblame<CR>
  nnoremap <silent> <Leader>gB :Gbrowse<CR>
  nnoremap <silent> <Leader>gl :0Glog<CR>:bot copen<CR>
  nnoremap <silent> <Leader>gL :Glog<CR>:bot copen<CR>
  nnoremap <silent> <Leader>gf :Gfetch<CR>
  nnoremap <silent> <Leader>gF :Gpull<CR>
  nnoremap <silent> <Leader>gm :Gmerge<CR>
  nnoremap <Leader>gg :Ggrep<Space>
  nnoremap <Leader>gp :Gpush<Space>
  nnoremap <Leader>gM :Gmove<Space>
  nnoremap <Leader>gD :Gdelete<Space>
  " }}}3
  call minpac#add('tpope/vim-rhubarb') " Github extension for vim-fugitive
  call minpac#add('junegunn/gv.vim') " Git commit browser, requires vim-fugitive {{{3
  nnoremap <silent> <Leader>gv :GV<CR>
  " }}}3
  " }}}2
  " 6. Other non-programming {{{2
  call minpac#add('editorconfig/editorconfig-vim') " EditorConfig plugin {{{3
  let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
  autocmd FileType gitcommit let b:EditorConfig_disable = 1
  " }}}3
  call minpac#add('justinmk/vim-dirvish') " directory viewer, replaces netrw
  call minpac#add('tpope/vim-obsession') " continuously updated session files {{{3
  nnoremap <Leader>of :Obsession<Space>
  nnoremap <silent> <Leader>oo :Obsession<CR>
  nnoremap <silent> <Leader>o! :Obsession!<CR>
  " }}}3
  call minpac#add('tpope/vim-projectionist') " project-specific configuration
  call minpac#add('tpope/vim-sleuth') " auto-adjust 'shiftwidth' and 'expandtab'
  " }}}2
  " 7. Programming / General {{{2
  call minpac#add('dense-analysis/ale') " linting engine and LSP client {{{3
  let g:ale_linters = {
        \ 'awk': ['gawk'],
        \ 'bash': ['shellcheck'],
        \ 'cython': ['cython'],
        \ 'python': ['mypy', 'pylint'],
        \ 'sh': ['shellcheck'],
        \ }
  let g:ale_fixers = {
        \ 'json': ['jq'],
        \ 'python': ['black'],
        \ }
  nnoremap <silent> <Leader>aT :ALEToggleBuffer<CR>
  nnoremap <silent> <Leader>ah :ALEHover<CR>
  nnoremap <silent> <Leader>aL :ALELint<CR>
  nnoremap <silent> <Leader>aH :ALEDetail<CR>
  nnoremap <silent> <Leader>a? :ALEInfo<CR>
  nnoremap <silent> <Leader>al :ALEResetBuffer<CR>
  nnoremap <silent> <Leader>as :ALELintStop<CR>
  nnoremap <silent> <Leader>aS :ALEStopAllLSPs<CR>
  nnoremap <silent> <Leader>a[ :ALEPreviousWrap<CR>
  nnoremap <silent> <Leader>a] :ALENextWrap<CR>
  nnoremap <silent> <Leader>a{ :ALEFirst<CR>
  nnoremap <silent> <Leader>a} :ALELast<CR>
  nnoremap <silent> <Leader>a/ :ALESymbolSearch<Space>
  nnoremap <silent> <Leader>ai :ALEImport<CR>
  nnoremap <silent> <Leader>aI :ALEOrganizeImports<CR>
  nnoremap <silent> <Leader>aR :ALERename<CR>
  nnoremap <silent> <Leader>ac :ALECodeAction<CR>
  nnoremap <silent> <Leader>a. :ALEGoToDefinition<CR>
  nnoremap <silent> <Leader>at :ALEGoToTypeDefinition<CR>
  nnoremap <silent> <Leader>ar :ALEFindReferences<CR>
  nnoremap <silent> <Leader>ad :ALEDocumentation<CR>
  nnoremap <silent> <Leader>af :ALEFix<CR>
  nnoremap <silent> <Leader>aF :ALEFixSuggest<CR>
  " }}}3
  " }}}2
  " 8. Programming / Python {{{2
  call minpac#add('davidhalter/jedi-vim') " autocompletion for Python {{{3
  " jedi-vim configuration
  let g:jedi#auto_initialization = 1
  let g:jedi#auto_vim_configuration = 0
  let g:jedi#popup_on_dot = 1
  let g:jedi#popup_select_first = 0
  let g:jedi#show_call_signatures = 2 " show signatures in command line instead of popup window
  " jedi-vim mappings
  let g:jedi#completions_command = "<C-Space>"
  let g:jedi#goto_command = "<Leader>jd"
  let g:jedi#goto_assignments_command = "<Leader>jg"
  let g:jedi#goto_stubs_command = "<Leader>js"
  let g:jedi#documentation_command = "K"
  let g:jedi#rename_command = "<Leader>jr"
  let g:jedi#usages_command = "<Leader>jn"
  " }}}3
  call minpac#add('Vimjas/vim-python-pep8-indent') " nicer Python indentation
  " }}}2
  " 9. Programming / Database {{{2
  " Database
  call minpac#add('tpope/vim-dadbod') " database interface
  call minpac#add('kristijanhusak/vim-dadbod-completion') " completion support for vim-dadbod {{{3
  " Omnifunc support
  autocmd FileType sql setlocal omnifunc=vim_dadbod_completion#omni
  " }}}3
  call minpac#add('kristijanhusak/vim-dadbod-ui') " front end interface for vim-dadbod {{{3
  nnoremap <silent> <Leader>D :DBUI<CR>
  " Example of configuring connection details, define in init.vim.local
  " let g:dbs = {
  "       \ 'dev': 'postgres://postgres:mypassword@localhost:5432/my-dev-db',
  "       \ 'staging': 'postgres://postgres:mypassword@localhost:5432/my-staging-db',
  "       \ 'wp': 'mysql://root@localhost/wp_awesome',
  "       \ }
  " }}}3
  " }}}2
endif

" }}}1
" Section: Local Neovim config {{{1
" ---------------------------------

if filereadable(expand("~/.config/nvim/init.vim.local"))
  source ~/.config/nvim/init.vim.local
endif

" }}}1
" Modeline {{{1
" vim:set ft=vim et sw=2 fen fdc=1 fdm=marker: }}}1
