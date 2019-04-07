" ~/.config/nvim/init.vim - Config file for Nvim
" Author: matheuristic

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
  call minpac#init()

  " minpac must have {'type': 'opt'} so that it can be loaded with `packadd`
  call minpac#add('k-takata/minpac', {'type': 'opt'})

  " General enhancements
  call minpac#add('junegunn/fzf', {'do': '!./install --bin'})
  call minpac#add('junegunn/fzf.vim')
  call minpac#add('mbbill/undotree')
  call minpac#add('tpope/vim-abolish')
  call minpac#add('tpope/vim-commentary')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('tpope/vim-projectionist')
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-unimpaired')

  " User interface
  call minpac#add('vim-airline/vim-airline') " status line
  call minpac#add('morhetz/gruvbox', {'type': 'opt'}) " color scheme
endif

" }}}1
" Section: Options {{{1
" ---------------------

"set backup      " keep backups, usually better to use version control
"set backupdir=~/.vimfiles/backup//,.,~/tmp/,~/ " backup file folders, appending // uses the full path in the name
set directory=~/.vimfiles/swap//,.,~/tmp,/var/tmp,/tmp " swapfile folders, appending // uses the full path in the name
set expandtab   " expand tabs into spaces; use <C-v><Tab> for a real tab
set hidden      " hide abandoned buffers instead of unloading them
set inccommand=nosplit " show effects of commands incrementally
"set list        " highlight tabs and trailing whitespace
set listchars=tab:\|\ ,trail:.,extends:>,precedes:<,nbsp:. " chars for displaying whitespace when 'list' is set
set nojoinspaces " do not insert two spaces after '.', '?' and '!' on line joins
set swapfile    " use swapfiles, swapfile location is determined by 'directory'

" Use ripgrep if available {{{2
if executable('/opt/local/bin/rg')
  set grepprg=/opt/local/bin/rg\ --vimgrep
endif
" }}}2
" Enable folding {{{2
if has('folding')
  set foldenable
  set foldmethod=marker
endif
" }}}2
" Visually indicate wrapped lines {{{2
if has('linebreak')
  set showbreak=…\  " put '… ' at start of each continued line
endif
" }}}2
" Persistent undo {{{2
if has("persistent_undo")
  set undofile
  set undodir=~/.vimfiles/undo//,. " undo file folders, appending // uses the full path in the name
endif
" }}}2
" Set syntax highlighting colorscheme {{{2
if has('syntax') && (&t_Co > 2)
  silent! unset g:colors_name
  syntax on
  " List of (colors_name[, numcolors needed])
  let s:colors_list = [['gruvbox', 256], ['zenburn', 256], ['desert', 8], ['default', 8]]
  " In list order, try setting color scheme if the terminal emulator supports
  " the number of colors necessary for the scheme (default: 256)
  for color_pair in s:colors_list
    if !exists('colors_name') && (&t_Co >= get(color_pair, 1, 256))
      if color_pair[0] =~ '^gruvbox'
        set background=dark
        let g:gruvbox_contrast_dark = 'soft'
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
" Unhighlight search results (from https://github.com/tpope/vim-sensible) {{{2
if has('extra_search')
  nnoremap <silent> <Leader>l :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR>
endif
" }}}2
" Change directory to current file's {{{2
nnoremap <silent> <Leader>C :cd %:p:h<CR>:pwd<CR>
" }}}2
" Toggle folding {{{2
if has('folding')
  nnoremap <silent> <Leader>f :set foldenable! foldenable?<CR>
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
" Toggle undotree {{{2
if exists('g:loaded_undotree') | nnoremap <silent> <Leader>u :UndotreeToggle | endif
" }}}2

" }}}1
" Modeline {{{1
" vim:set ft=vim et sw=2 fen fdc=1 fdm=marker: }}}1
