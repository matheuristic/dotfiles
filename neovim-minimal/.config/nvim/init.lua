-- init.lua --- Neovim configuration

vim.opt.foldmethod = "marker"          -- use markers to specify folds
vim.opt.formatoptions = vim.opt.formatoptions + "mM" -- correctly break multibyte chars like CJK ones
vim.opt.listchars = "tab:| ,trail:.,extends:>,precedes:<,nbsp:." -- non-visible whitespace (use `:set list`)
vim.opt.linebreak = true               -- soft wrap
vim.opt.number = true                  -- line numbers
vim.opt.undofile = true                -- undo files (~/.local/share/nvim/undo/)
vim.opt.wildmode = "list:longest,full" -- tab completion options

if vim.fn.executable('rg') == 1 then
    vim.opt.grepprg="rg --vimgrep"
end

vim.cmd [[ autocmd FileType markdown setlocal et sw=2 sts=2 tw=80 ]]
vim.cmd [[ autocmd FileType python setlocal et sw=4 sts=4 ]]
vim.cmd [[ autocmd FileType sql setlocal et sw=2 sts=2 ]]
vim.cmd [[ autocmd FileType yaml setlocal et sw=2 sts=2 ]]

-- Source local configs
vim.cmd [[ runtime! local.vim ]]
vim.cmd [[ runtime! local.lua ]]
