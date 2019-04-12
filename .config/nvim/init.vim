" ~/.config/nvim/init.vim - Config file for Nvim
" Author: matheuristic

" Section: Options {{{1
" ---------------------

"set backup      " keep backups, usually better to use version control
"set backupdir=~/.nvimfiles/backup//,.,~/tmp/,~/ " backup file folders, appending // uses the full path in the name
set directory=~/.nvimfiles/swap//,.,~/tmp,/var/tmp,/tmp " swapfile folders, appending // uses the full path in the name
set expandtab   " expand tabs into spaces; use <C-v><Tab> for a real tab
set hidden      " hide abandoned buffers instead of unloading them
set inccommand=nosplit " show effects of commands incrementally
"set list        " highlight tabs and trailing whitespace
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:. " chars for displaying whitespace when 'list' is set
set nojoinspaces " do not insert two spaces after '.', '?' and '!' on line joins
set swapfile    " use swapfiles, which are stored in 'directory'

" Use ripgrep if available {{{2
if executable('rg')
  set grepprg=rg\ --vimgrep
endif " }}}2

" Enable filetype detection for plugins and indents {{{2
if has('autocmd')
  filetype plugin indent on
endif " }}}2
" Enable folding {{{2
if has('folding')
  set foldenable
  set foldmethod=marker
endif " }}}2
" Visually indicate wrapped lines {{{2
if has('linebreak')
  set showbreak=…\  " put '… ' at start of each continued line
endif " }}}2
" Persistent undo {{{2
if has("persistent_undo")
  set undofile
  set undodir=~/.nvimfiles/undo//,. " undo file folders, appending // uses the full path in the name
endif " }}}2
" Use GUI colors in the terminal (requires 24-bit color support) {{{2
if has("termguicolors")
  set termguicolors
endif " }}}2
" Set the terminal emulator title to path of file being edited {{{2
if has('title')
  set title
endif " }}}2

" }}}1
" Section: Basic Keymappings {{{1
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
  nnoremap <silent> <Leader>F :set foldenable! foldenable?<CR>
endif
" }}}2
" Toggle spell check {{{2
if has('syntax')
  nnoremap <silent> <Leader>ssp :set spell! spell?<CR>
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
nnoremap <silent> <Leader>sml :set modeline! modeline?<CR>
" }}}2

" }}}1
" Section: Packages {{{1
" ----------------------

" Using minpac - https://github.com/k-takata/minpac
" To install the minpac package manager, run in the shell:
"   $ git clone https://github.com/k-takata/minpac.git ~/.config/nvim/pack/minpac/opt/minpac
" To install or update registered packages, run:
"   :call minpac#update()
" To delete packages that are no longer registered, run:
"   :call minpac#clean()

packadd minpac

if exists('*minpac#init')
  " Initialize minpac
  call minpac#init()

  " minpac must have {'type': 'opt'} so that it can be loaded with `packadd`
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  " 1. Interface {{{2
  call minpac#add('chriskempson/base16-vim', {'type': 'opt'}) " color scheme {{{3
  colorscheme base16-grayscale-dark " }}}3
  call minpac#add('vim-airline/vim-airline') " status line
  call minpac#add('vim-airline/vim-airline-themes') " status line themes {{{3
  let g:airline_theme='base16_grayscale' " }}}3
  call minpac#add('mhinz/vim-startify') " fancy start screen {{{3
  if has('autocmd')
    augroup startify " run startify on new tabs
      autocmd!
      autocmd TabNewEntered * if bufname('%') == '' | Startify | endif
    augroup END
  endif " }}}3
  call minpac#add('liuchengxu/vim-which-key') " display available keybindings in a popup {{{3
  nnoremap <silent> <Leader> :<c-u>WhichKey '<Leader>'<CR>
  " }}}2
  " 2. Editing {{{2
  call minpac#add('mbbill/undotree') " undo history visualizer {{{3
  nnoremap <silent> <Leader>u :UndotreeToggle<CR>
  " }}}3
  call minpac#add('terryma/vim-multiple-cursors') " multiple selection like Sublime Text
  call minpac#add('tpope/vim-abolish') " search, substitute and abbreviate word variants
  call minpac#add('tpope/vim-commentary') " toggle commenting of lines
  call minpac#add('tpope/vim-surround') " mappings to manipulate parentheses, brackets and quotes
  call minpac#add('tpope/vim-unimpaired') " handy bracket mappings
  " }}}2
  " 3. Search {{{2
  call minpac#add('junegunn/fzf', {'do': '!./install --bin'}) " fuzzy file search {{{3
  call minpac#add('junegunn/fzf.vim')
  " Some fzf commands have dependencies on vim packages or system utilities
  "   :Commits  - vim-fugitive
  "   :Rg       - rg (ripgrep) needs to be installed on the system
  "   :Snippets - UltiSnips
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
  call minpac#add('SirVer/ultisnips') " snippet manager, requires Python pynvim package be installed
  call minpac#add('honza/vim-snippets') " default snippets
  let g:UltiSnipsExpandTrigger="<NUL>" " disable expand trigger
  let g:UltiSnipsListTriggers="<c-tab>" " choose expansion from completing snippet list
  let g:UltiSnipsJumpForwardTrigger="<tab>" " jump to next placeholder in snippet
  let g:UltiSnipsJumpBackwardTrigger="<s-tab>" " jump to prev placeholder in snippet
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
  " 6. Other {{{2
  call minpac#add('tpope/vim-obsession') " continuously updated session files {{{3
  nnoremap <Leader>of :Obsession<Space>
  nnoremap <silent> <Leader>oo :Obsession<CR>
  nnoremap <silent> <Leader>o! :Obsession!<CR>
  " }}}3
  call minpac#add('tpope/vim-projectionist') " project-specific configuration
  call minpac#add('tpope/vim-sleuth') " auto-adjust 'shiftwidth' and 'expandtab'
  " }}}2
endif

" }}}1
" Section: Local Vim Config {{{1
" ------------------------------

if filereadable(expand("~/.config/nvim/init.vim.local"))
  source ~/.config/nvim/init.vim.local
endif

" }}}1
" Modeline {{{1
" vim:set ft=vim et sw=2 fen fdc=1 fdm=marker: }}}1
